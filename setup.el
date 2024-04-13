;;; setup.el --- Helpful Configuration Macro    -*- lexical-binding: t -*-

;; Copyright (C) 2021, 2022, 2023, 2024  Free Software Foundation, Inc.

;; Author: Philip Kaludercic <philipk@posteo.net>
;; Maintainer: Philip Kaludercic <~pkal/public-inbox@lists.sr.ht>
;; Version: 1.3.2
;; Package-Requires: ((emacs "26.1"))
;; Keywords: lisp, local
;; URL: https://git.sr.ht/~pkal/setup

;; This package is Free Software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; The `setup' macro simplifies repetitive configuration patterns, by
;; providing context-sensitive local macros in `setup' bodies.  These
;; macros can be mixed with regular elisp code without any issues,
;; allowing for flexible and terse configurations.  The list of local
;; macros can be extended by the user via `setup-define'.  A list of
;; currently known local macros are documented in the docstring for `setup'.

;; Examples and extended documentation can be found on Emacs wiki:
;; https://www.emacswiki.org/emacs/SetupEl.  Please feel free to
;; contribute your own local macros or ideas.

;;; News:

;;;; Version 1.3.2 (bug fix release)

;; - Fix `:and' once again.

;;; Code:

(defvar setup-opts `((quit . ,(make-symbol "setup-quit")))
  "Alist defining the context for local macros.
Context-modifying macros (`:with-feature', `:with-mode', ...)
prepend the new context to this variable using `let', before
calling `setup-expand'.  Within the macro definitions `setup-get'
is used to retrieve the current context.")

(defvar setup-attributes '()
  "A list symbols used to store a state during macro processing.
The list is populated during macro expansion, and may modify the
behaviour of the functions in `setup-modifier-list'.")

(defun setup-wrap-to-catch-quits (body _name)
  "Wrap BODY in a catch block if necessary.
The body is wrapped in a `catch' block if `setup-attributes'
contains the symbol `need-quit'."
  (if (memq 'need-quit setup-attributes)
      `(catch ',(setup-get 'quit) ,@(macroexp-unprogn body))
    body))

(defun setup-wrap-to-demote-errors (body _name)
  "Wrap BODY in a `with-demoted-errors' block.
This behaviour is prevented, if `setup-attributes' contains the
symbol `without-error-demotion'."
  (if (memq 'without-error-demotion setup-attributes)
      body
    `(with-demoted-errors ,(format "Error in setup form on line %d: %%S"
                                     (line-number-at-pos))
       ,body)))

(defvar setup-modifier-list
  '(setup-wrap-to-catch-quits)
  "List of wrapper functions to be called after macro expansion.
Each function is invoked by passing the current body and the name
of the default feature, returning the modified body.")

(defvar setup-macros nil
  "Local macro definitions to be bound in `setup' bodies.
Do not modify this variable by hand.  Instead use
`setup-define.'")

;;;###autoload
(defun setup--make-docstring ()
  "Return a docstring for `setup'."
  (with-temp-buffer
    (insert (documentation (symbol-function 'setup) 'raw)
            "\n\n")
    (if (null setup-macros)
        (insert "No local macros are defined.")
      (insert "Within BODY, `setup' is able to expand context-sensitive local macros.  "
              "Some of these may be evaluated after the respective current feature "
              "has been loaded, by wrapping the expression in a `with-eval-after-load' block.  "
              "In the following list this is indicated by a \"*\".  "
              "Otherwise a \"-\" is used for all macros that expand to code "
              "that is immediately evaluated.")
      (fill-paragraph)
      (dolist (sym (sort (mapcar #'car setup-macros) #'string-lessp))
        (if (fboundp 'make-separator-line)
            (insert "\n" (make-separator-line) "\n")
          (newline 2))
        (let ((sig (mapcar
                    (lambda (arg)
                      (if (string-match "\\`&" (symbol-name arg))
                          arg
                        (intern (upcase (symbol-name arg)))))
                    (get sym 'setup-signature))))
          (insert (format " %c %s\n\n"
                          (if (get sym 'setup-delayed-eval)
                              ?* ?-)
                          (cons sym sig))
                  (or (get sym 'setup-documentation)
                      "No documentation.")))))
    (buffer-string)))

;;;###autoload
(defmacro setup (name &rest body)
  "Configure feature or subsystem NAME.
BODY may contain special forms defined by `setup-define', but
will otherwise just be evaluated as is.
NAME may also be a macro, if it can provide a symbol."
  (declare (debug (&rest &or [symbolp sexp] form))
           (indent 1))
  (when (consp name)
    (push name body)
    (let ((shorthand (get (car name) 'setup-shorthand)))
      (setq name (and shorthand (funcall shorthand name)))))
  (let ((setup-attributes setup-attributes))
    (setq body (macroexpand-all
                (if (assq :with-feature setup-macros)
                    `(:with-feature ,name ,@body)
                  (macroexp-progn body))
                (append setup-macros macroexpand-all-environment)))
    (dolist (mod-fn setup-modifier-list)
      (setq body (funcall mod-fn body name)))
    body))

;;;###autoload
(put 'setup 'function-documentation '(setup--make-docstring))

(defun setup--ensure (ensure-spec args &optional check-len)
  "Ensure that ARGS matches the form of ENSURE-SPEC.

The symbol `kbd' means to apply the function `kbd' to the
argument.  The symbol `func' means to sharp-quote the argument.
The symbol `&rest' means that the remaining elements of
ENSURE-SPEC are applied repeatedly to the remaining elements of
ARGS.

When CHECK-LEN is non-nil, check that the lengths
of ENSURE-SPEC and ARGS are compatible."
  (when check-len
    (let ((check ensure-spec)
          (found nil)
          (count 0))
      (while check
        (if (eq (car check) '&rest)
            (if (zerop (mod (- (length args) count)
                            (length (cdr check))))
                (setq found t
                      check nil)
              (error "Bad `:ensure' spec or argument list"))
          (setq check (cdr check)
                count (1+ count))))
      (unless (or found (= (length args) count))
        (error "Bad `:ensure' spec or argument list"))))

  (let ((result))
    (while args
      (let ((ensure (pop ensure-spec)))
        (if (eq ensure '&rest)
            (let* ((rest-spec ensure-spec)
                   (rest-spec-len (length rest-spec)))
              ;; Consume remaining `args'
              (while args
                (dolist (ensured
                         (setup--ensure rest-spec
                                        (let ((to-be-ensured))
                                          (dotimes (_ rest-spec-len)
                                            (push (pop args) to-be-ensured))
                                          (nreverse to-be-ensured))
                                        nil))
                  (push ensured result))))
          (let ((arg (pop args)))
            (push (cond ((null ensure) arg) ;Do not modify argument
                        ((eq ensure 'kbd) (cond
                                           ((stringp arg) (kbd arg))
                                           ((symbolp arg) `(kbd ,arg))
                                           (arg)))
                        ((eq ensure 'func) (cond
                                            ((eq (car-safe arg) 'function)
                                             arg)
                                            ((eq (car-safe arg) 'quote)
                                             `#',(cadr arg))
                                            ((symbolp arg)
                                             `#',arg)
                                            (arg)))
                        ((error "Invalid ensure spec %S" ensure)))
                  result)))))
    (nreverse result)))

(defun setup-define (name fn &rest opts)
  "Define `setup'-local macro NAME using function FN.
The plist OPTS may contain the key-value pairs:

  :indent SPEC
Change indentation behaviour.  See symbol `lisp-indent-function'.

  :after-loaded BOOL
Wrap the macro in a `with-eval-after-load' body.

  :repeatable ARITY
Allow macro to be automatically repeated.  If ARITY is t, use
`func-arity' to determine the minimal number of arguments.
If ARITY is a dotted pair, then the car of ARITY is the number
of shared arguments in the repeated function calls and the
cdr is the number of arguments that make of the remainder
of the function call.  The cdr may also be t, as above.

  :signature SIG
Give an advertised calling convention.

  :documentation STRING
A documentation string.

  :shorthand EXTRACTOR
If a macro defines a shorthand, it might be used as the first
argument of a `setup' form, instead of a symbol.  EXTRACTOR must
be a function of one argument.  It takes the entire macro and
returns a symbol to replace NAME.

  :debug SPEC
A edebug specification, see Info node `(elisp) Specification List'.
If not given, it is assumed nothing is evaluated.

  :ensure SPEC

A list of symbols indicating what kind of argument each parameter
to FN is.  If the nth parameter is not to be reinterpreted, the
nth symbol in SPEC should nil.  For key bindings `kbd' and for
functions `func'.  `&rest' means that the remaining symbols apply
to the remaining arguments repeatedly.  Any other value is
invalid."
  (declare (indent 1))
  ;; NB.: NAME is not required to by a keyword, even though all macros
  ;;      specified on the next page use keywords.  The rationale for
  ;;      this is currently that there is no clean way to "locally"
  ;;      modify indentation, without setting `lisp-indent-function',
  ;;      chaining the indentation behaviour everywhere.
  (unless (symbolp name)
    (error "Macro name must be a symbol"))
  ;; save metadata
  (put name 'setup-documentation (plist-get opts :documentation))
  (put name 'setup-signature
       (or (plist-get opts :signature)
           (append (help-function-arglist fn 'preserve-names)
                   (if (plist-get opts :repeatable) '(...)))))
  (put name 'setup-shorthand (plist-get opts :shorthand))
  (put name 'setup-definition-file (or load-file-name buffer-file-name))
  (put name 'setup-delayed-eval (plist-get opts :after-loaded))
  (put name 'lisp-indent-function (plist-get opts :indent))
  ;; define macro for `macroexpand-all'
  (setf (alist-get name setup-macros)   ;New in Emacs-25.
        (let* ((possible-num-repeated (if (eq (plist-get opts :repeatable) t)
                                          (car (func-arity fn))
                                        (plist-get opts :repeatable)))
               (fn (if (null possible-num-repeated)
                       fn
                     (lambda (&rest args)
                       (let ((aggr)
                             (using-shared-args (consp possible-num-repeated))
                             (num-shared)
                             (shared-args)
                             (num-repeated))

                         (if using-shared-args
                             (progn
                               (setq num-shared (car possible-num-repeated)
                                     num-repeated (if (eq (cdr possible-num-repeated) t)
                                                      (- (car (func-arity fn))
                                                         num-shared)
                                                    (cdr possible-num-repeated))
                                     shared-args args
                                     args (nthcdr num-shared args))
                               (setf (nthcdr num-shared shared-args) nil))
                           (setq num-repeated possible-num-repeated))

                         (unless (zerop (mod (length args) num-repeated))
                           (error "Illegal arguments"))

                         (while args
                           (let ((rest (nthcdr num-repeated args)))
                             (setf (nthcdr num-repeated args) nil)
                             (let ((ensure-spec (plist-get opts :ensure)))
                               (cond
                                ((and using-shared-args ensure-spec)
                                 (setq args (setup--ensure
                                             ensure-spec
                                             (append shared-args args) t)))
                                (using-shared-args
                                 (setq args (append shared-args args)))
                                (ensure-spec
                                 (setq args (setup--ensure ensure-spec args t)))))
                             (push (apply fn args) aggr)
                             (setq args rest)))
                         (macroexp-progn (nreverse aggr)))))))
          (if (plist-get opts :after-loaded)
              (lambda (&rest args)
                `(with-eval-after-load ',(setup-get 'feature)
                   ,(apply fn args)))
            fn)))
  ;; FIXME: Use `&interpose' with `edebug-lexical-macro-ctx' in Emacs≥28;
  ;; see `cl-macrolet' how to do it.
  (setf (alist-get (symbol-name name)
                   (cdddr (get 'setup 'edebug-form-spec))
                   nil nil #'equal)
        (let ((spec (plist-get opts :debug)))
          (cond ((null spec) '(&rest sexp))
                ((plist-get opts :repeatable)
                 (cons '&rest spec))
                (t spec)))))

(defun setup--xref-def-function (symbol)
  "Return an elisp xref location for SYMBOL."
  (require 'elisp-mode)
  (and (assq symbol setup-macros)
       (let ((file (get symbol 'setup-definition-file)))
         (list (elisp--xref-make-xref nil symbol file)))))

(add-to-list 'elisp-xref-find-def-functions
             #'setup--xref-def-function)


;;; Common utility functions for local macros

(defun setup-get (opt)
  "Retrieve the context-sensitive value for OPT.
If the context is not defined, an error is thrown.  See
`setup-opts' for more details."
  (or (cdr (assq opt setup-opts))
      (error "Cannot deduce %S from context" opt)))

(defun setup-expand (body)
  "Expand local macros in BODY.
This must be used in context-setting macros (`:with-feature',
`:with-mode', ...) to ensure that all child-macros use the right
settings."
  (macroexpand-all (macroexp-progn body) setup-macros))

(defmacro setup-bind (body &rest vars)
  "Add VARS to `setup-opts' in BODY.
Each entry in VARS is a list of the form (VAR VAL), comparable to
`let'.  This macro makes sure that the BODY is expanded correctly
so that it can make use of the new bindings in VARS."
  (declare (debug let) (indent 1))
  `(let ((setup-opts (append
                      (list
                       ,@(mapcar
                          (lambda (bind)
                            (list 'cons
                                  (list 'quote (car bind))
                                  (cadr bind)))
                          vars))
                      setup-opts)))
     (setup-expand ,body)))

(defun setup-quit (&optional return)
  "Generate code to quit evaluation.
If RETURN is given, throw that value."
  (push 'need-quit setup-attributes)
  `(throw ',(setup-get 'quit) ,return))

(defun setup-make-setter (old-val-fn wrap-fn)
  "Return a macro function to generate a setter.
The function OLD-VAL-FN is used to extract the old value of VAL.
The function WRAP-FN combines the transformed values of NAME and
VAL into one s-expression."
  (lambda (name val)
    (cond ((symbolp name) (funcall wrap-fn name val))
          ((eq (car-safe name) 'append)
           (funcall wrap-fn
                    (cadr name)
                    (let ((sym (gensym)))
                      `(let ((,sym ,val)
                             (list ,(funcall old-val-fn (cadr name))))
                         (if (member ,sym list)
                             list
                           (append list (list ,sym)))))))
          ((eq (car-safe name) 'append*)
           (funcall wrap-fn
                    (cadr name)
                    (let ((sym (gensym))
                          (i (gensym)))
                      `(let ((list ,(funcall old-val-fn (cadr name)))
                             (,sym nil))
                         (dolist (,i ,val)
                           (if (member ,i list)
                               nil
                             (push ,i ,sym)))
                         (append list (nreverse ,sym))))))
          ((eq (car-safe name) 'prepend)
           (funcall wrap-fn
                    (cadr name)
                    (let ((sym (gensym)))
                      `(let ((,sym ,val)
                             (list ,(funcall old-val-fn (cadr name))))
                         (if (member ,sym list)
                             list
                           (cons ,sym list))))))
          ((eq (car-safe name) 'prepend*)
           (funcall wrap-fn
                    (cadr name)
                    (let ((sym (gensym))
                          (i (gensym)))
                      `(let ((list ,(funcall old-val-fn (cadr name)))
                             (,sym nil))
                         (dolist (,i ,val)
                           (if (member ,i list)
                               nil
                             (push ,i ,sym)))
                         (append (nreverse ,sym) list)))))
          ((eq (car-safe name) 'remove)
           (funcall wrap-fn
                    (cadr name)
                    `(remove ,val ,(funcall old-val-fn (cadr name)))))
          ((eq (car-safe name) 'remove*)
           (funcall wrap-fn
                    (cadr name)
                    (let ((i (gensym)))
                      `(let ((list ,(funcall old-val-fn (cadr name))))
                         (dolist (,i ,val)
                           (setq list (remove ,i list)))
                         list))))
          ((error "Invalid option %S" name)))))


;;; Default local macros definitions

(setup-define :with-feature
  (lambda (features &rest body)
    (let (bodies)
      (dolist (feature (if (listp features) features (list features)))
        (push (if feature
                  (let ((mode (if (string-match-p "-mode\\'" (symbol-name feature))
                                  feature
                                (intern (format "%s-mode" feature)))))
                    (setup-bind body
                      (feature feature)
                      (mode (or (get feature 'setup-mode) mode))
                      (func (or (get feature 'setup-func) mode))
                      (hook (or (get feature 'setup-hook)
                                (get mode 'setup-hook)
                                (intern (format "%s-hook" mode))))
                      (map (or (get feature 'setup-map)
                               (get mode 'setup-map)
                               (intern (format "%s-map" mode))))))
                body)
              bodies))
      (macroexp-progn (if features (nreverse bodies) body))))
  :documentation "Change the FEATURE that BODY is configuring.
This macro also:
- Declares a current mode by appending \"-mode\" to
  FEATURE, unless it already ends with \"-mode\"
- Declares a current hook by appending \"-hook\" to the mode
- Declares a current map by appending \"-map\" to the mode
- Declares a current function that has the same name as the mode
If FEATURE is a list, apply BODY to all elements of FEATURE."
  :debug '([&or ([&rest sexp]) sexp] setup)
  :indent 1)

(setup-define :with-mode
  (lambda (modes &rest body)
    (let (bodies)
      (dolist (mode (if (listp modes) modes (list modes)))
        (push (setup-bind body
                (mode mode)
                (func (or (get mode 'setup-func)
                          mode))
                (hook (or (get mode 'setup-hook)
                          (intern (format "%s-hook" mode))))
                (map (or (get mode 'setup-map)
                         (intern (format "%s-map" mode)))))
              bodies))
      (macroexp-progn (nreverse bodies))))
  :documentation "Change the MODE that BODY is configuring.
If MODE is a list, apply BODY to all elements of MODE.
This macro also:
- Declares a current hook by appending \"-hook\" to the mode
- Declares a current map by appending \"-map\" to the mode
- Declares a current function that has the same name as the mode"
  :debug '([&or ([&rest sexp]) sexp] setup)
  :indent 1)

(setup-define :with-map
  (lambda (maps &rest body)
    (let (bodies)
      (dolist (map (if (listp maps) maps (list maps)))
        (push (setup-bind body (map map))
              bodies))
      (macroexp-progn (nreverse bodies))))
  :documentation "Change the MAP that BODY will bind to.
If MAP is a list, apply BODY to all elements of MAP."
  :debug '([&or ([&rest sexp]) sexp] setup)
  :indent 1)

(setup-define :with-hook
  (lambda (hooks &rest body)
    (let (bodies)
      (dolist (hook (if (listp hooks) hooks (list hooks)))
        (push (setup-bind body (hook hook))
              bodies))
      (macroexp-progn (nreverse bodies))))
  :documentation "Change the HOOK that BODY will use.
If HOOK is a list, apply BODY to all elements of HOOK."
  :debug '([&or ([&rest sexp]) sexp] setup)
  :indent 1)

(setup-define :with-function
  (lambda (functions &rest body)
    (let (bodies)
      (dolist (func (if (listp functions) functions (list functions)))
        (let ((fn (if (memq (car-safe func) '(quote function))
                      (cadr func)
                    func)))
          (push (setup-bind body (func fn))
                bodies)))
      (macroexp-progn (nreverse bodies))))
  :documentation "Change the FUNCTION that BODY will use.
If FUNCTION is a list, apply BODY to all elements of FUNCTION."
  :debug '(sexp setup)
  :indent 1)

(setup-define :package
  (lambda (package)
    `(unless (package-installed-p ',package)
       (unless (assq ',package package-archive-contents)
         (package-refresh-contents))
       (package-install ',package)))
  :documentation "Install PACKAGE if it hasn't been installed yet.
The first PACKAGE can be used to deduce the feature context."
  :repeatable t
  :shorthand #'cadr)

(setup-define :autoload-this
  (lambda (&rest args)
    `(autoload ',(setup-get 'func) ,(format "%s" (setup-get 'feature)) ,@args))
  :documentation "Explicitly mark the context function to be autoloaded.")

(setup-define :require
  (lambda (feature)
    `(unless (require ',feature nil t)
       ,(setup-quit)))
  :documentation "Try to require FEATURE, or stop evaluating body.
The first FEATURE can be used to deduce the feature context."
  :repeatable t
  :shorthand #'cadr)

(setup-define :global
  (lambda (key command)
    `(global-set-key ,key ,command))
  :documentation "Globally bind KEY to COMMAND."
  :debug '(form sexp)
  :ensure '(kbd func)
  :repeatable t)

(setup-define :bind
  (lambda (key command)
    `(define-key ,(setup-get 'map) ,key ,command))
  :documentation "Bind KEY to COMMAND in current map."
  :after-loaded t
  :debug '(form sexp)
  :ensure '(kbd func)
  :repeatable t)

(setup-define :unbind
  (lambda (key)
    `(define-key ,(setup-get 'map) ,key nil))
  :documentation "Unbind KEY in current map."
  :after-loaded t
  :debug '(form)
  :ensure '(kbd)
  :repeatable t)

(setup-define :rebind
  (lambda (key command)
    `(progn
       (dolist (key (where-is-internal ',command ,(setup-get 'map)))
         (define-key ,(setup-get 'map) ,key nil))
       (define-key ,(setup-get 'map) ,key ,command)))
  :documentation "Unbind the current key for COMMAND, and bind it to KEY."
  :after-loaded t
  :debug '(form sexp)
  :ensure '(kbd func)
  :repeatable t)

(setup-define :bind-into
  (lambda (feature &rest rest)
    (if (string-match-p "-map\\'" (symbol-name feature))
        (progn
          ;; https://lists.sr.ht/~pkal/public-inbox/%3C87pluzt28q.fsf@ushin.org%3E
          ;; https://lists.sr.ht/~pkal/public-inbox/%3C87edbdma8j.fsf@ushin.org%3E
          (warn "The `:bind-into' with a map %S is considered unreliable, and will be deprecated." feature)
          `(:with-map ,feature (:bind ,@rest)))
      `(:with-feature ,feature (:bind ,@rest))))
  :documentation "Bind into keys into the map of FEATURE.
The arguments REST are handled as by `:bind'."
  :debug '(sexp &rest form sexp)
  :ensure '(nil &rest kbd func)
  :indent 1)

(setup-define :hook
  (lambda (function)
    `(add-hook ',(setup-get 'hook) ,function))
  :documentation "Add FUNCTION to current hook."
  :ensure '(func)
  :repeatable t)

(setup-define :hook-into
  (lambda (mode)
    `(add-hook ',(let ((name (symbol-name mode)))
                   (if (string-match-p "-hook\\'" name)
                       mode
                     (intern (concat name "-hook"))))
               #',(setup-get 'func)))
  :documentation "Add current function to HOOK."
  :repeatable t)

(setup-define :bind-to
  (lambda (binding)
    `(global-set-key ,binding #',(setup-get 'func) ))
  :documentation "Bind current function to HOOK."
  :ensure '(kbd)
  :repeatable t)

(setup-define :option
  (setup-make-setter
   (lambda (name)
     `(funcall (or (get ',name 'custom-get)
                   #'symbol-value)
               ',name))
   (lambda (name val)
     `(progn
        (custom-load-symbol ',name)
        (funcall (or (get ',name 'custom-set) #'set-default)
                 ',name ,val))))

  :documentation "Set the option NAME to VAL.
NAME may be a symbol, or a cons-cell.  If NAME is a cons-cell, it
will use the car value to modify the behaviour.  These forms are
supported:

(append VAR)    Assuming VAR designates a list, add VAL as its last
                element, unless it is already member of the list.

(prepend VAR)   Assuming VAR designates a list, add VAL to the
                beginning, unless it is already member of the
                list.

(remove VAR)    Assuming VAR designates a list, remove all instances
                of VAL.

(append* VAR)  Assuming VAR designates a list, add each element
               of VAL to the end of VAR, keeping their order,
               unless it is already a member of the list.

(prepend* VAR) Assuming VAR designates a list, add each element
               of VAL to the start of VAR, keeping their order,
               unless it is already a member of the list.

(remove* VAR)  Assuming VAR designates a list, remove all
               instances of each element of VAL.

Note that if the value of an option is modified partially by
append, prepend, remove, one should ensure that the default value
has been loaded. Also keep in mind that user options customized
with this macro are not added to the \"user\" theme, and will
therefore not be stored in `custom-set-variables' blocks."
  :debug '(sexp form)
  :repeatable t)

(setup-define :local-set
  (setup-make-setter
   (lambda (name)
     (if (consp name) (cadr name) name))
   (lambda (name val)
     `(add-hook ',(setup-get 'hook) (lambda () (setq-local ,name ,val)))))
  :documentation "Set the value of NAME to VAL in buffers of the current mode.
NAME may be a symbol, or a cons-cell.  If NAME is a cons-cell, it
will use the car value to modify the behaviour. These forms are
supported:

(append VAR)    Assuming VAR designates a list, add VAL as its last
                element, unless it is already member of the list.

(prepend VAR)   Assuming VAR designates a list, add VAL to the
                beginning, unless it is already member of the
                list.

(remove VAR)    Assuming VAR designates a list, remove all instances
                of VAL.

(append* VAR)   Assuming VAR designates a list, add each element
                of VAL to the end of VAR, keeping their order,
                unless it is already a member of the list.

(prepend* VAR)  Assuming VAR designates a list, add each element
                of VAL to the start of VAR, keeping their order,
                unless it is already a member of the list.

(remove* VAR)   Assuming VAR designates a list, remove all
                instances of each element of VAL."
  :debug '(sexp form)
  :repeatable t)

(setup-define :local-hook
  (lambda (hook function)
    `(add-hook ',(setup-get 'hook)
               (lambda ()
                 (add-hook ',hook ,function nil t))))
  :documentation "Add FUNCTION to HOOK only in buffers of the current mode."
  :debug '(symbolp sexp)
  :ensure '(nil func)
  :repeatable '(1 . 1))

(setup-define :also-load
  (lambda (feature)
    `(require ',feature))
  :documentation "Load FEATURE with the current body."
  :after-loaded t
  :repeatable t)

(setup-define :if-package
  (lambda (package)
    `(unless (package-installed-p ',package)
       ,(setup-quit)))
  :documentation "If package is not installed, stop evaluating the body.
The first PACKAGE can be used to deduce the feature context."
  :repeatable t
  :shorthand #'cadr)

(setup-define :if-feature
  (lambda (feature)
    `(unless (featurep ',feature)
       ,(setup-quit)))
  :documentation "If FEATURE is not available, stop evaluating the body.
The first FEATURE can be used to deduce the feature context."
  :repeatable t
  :shorthand #'cadr)

(setup-define :only-if
  (lambda (condition)
    `(unless ,condition
       ,(setup-quit)))
  :documentation "If CONDITION evaluates to nil, stop evaluating the body."
  :debug '(form)
  :repeatable t)

(setup-define :file-match
  (lambda (pat)
    `(add-to-list 'auto-mode-alist (cons ,pat ',(setup-get 'mode))))
  :documentation "Associate the current mode with files that match PAT."
  :debug '(form)
  :repeatable t)

(setup-define :when-loaded
  (lambda (&rest body)
    (macroexp-progn body))
  :documentation "Evaluate BODY after the current feature has been loaded.
Avoid using this macro whenever possible, and
instead choose a more specialized alternative or write one
yourself."
  :debug '(setup)
  :after-loaded t
  :indent 0)

(setup-define :and
  (lambda (&rest conds)
    (let ((tail (car (last conds))))
      `(if (and ,@(butlast conds))
           ,@(cond
              ((symbolp tail) '(nil))
              ((consp tail) (last conds))
              ((error "Illegal tail")))
         ,(setup-quit))))
  :documentation "Abort evaluation of CONDS are not all true.
The expression of the last condition is used to deduce the
feature context."
  :shorthand
  (lambda (head)
    (unless (cdr head)
      (error ":and requires at least one condition"))
    (let ((tail (car (last head))))
      (cond
       ((symbolp tail) tail)
       ((consp tail)
        (let ((shorthand (get (car tail) 'setup-shorthand)))
          (and shorthand (funcall shorthand tail)))))))
  :debug '(setup))

(provide 'setup)

;;; setup.el ends here
