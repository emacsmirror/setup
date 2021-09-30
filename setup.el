;;; setup.el --- Helpful Configuration Macro    -*- lexical-binding: t -*-

;; Copyright (C) 2021  Free Software Foundation, Inc.

;; Author: Philip Kaludercic <philipk@posteo.net>
;; Maintainer: Philip Kaludercic <philipk@posteo.net>
;; Version: 1.0.1
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

;; The `setup` macro simplifies repetitive configuration patterns, by
;; providing context-sensitive local macros in `setup' bodies.  These
;; macros can be mixed with regular elisp code without any issues,
;; allowing for flexible and terse configurations.  The list of local
;; macros can be extended by the user via `setup-define'.  A list of
;; currently known local macros are documented in the docstring for `setup'.

;; Examples and extended documentation can be found on Emacs wiki:
;; https://www.emacswiki.org/emacs/SetupEl

;;; Code:

(require 'elisp-mode)

(defvar setup-opts `((quit . ,(make-symbol "setup-quit")))
  "Alist defining the context for local macros.
Context-modifying macros (`:with-feature', `:with-mode', ...)
prepend the new context to this variable using `let', before
calling `setup-expand'.  Within the macro definitions `setup-get'
is used to retrieve the current context.")

(defvar setup-attributes '(error-demotion)
  "A list symbols to be used by `setup-modifier-list'.")

(defun setup-wrap-to-catch-quits (body _name)
  "Wrap BODY in a catch block if necessary."
  (if (memq 'need-quit setup-attributes)
      `(catch ',(setup-get 'quit) ,@(macroexp-unprogn body))
    body))

(defun setup-wrap-to-demote-errors (body _name)
  "Wrap BODY in a `with-demoted-errors' block."
  (if (memq 'error-demotion setup-attributes)
      `(with-demoted-errors ,(format "Error in setup form on line %d: %%S"
                                     (line-number-at-pos))
         ,body)
    body))

(defvar setup-modifier-list
  '(setup-expand-local-macros
    setup-wrap-to-catch-quits
    setup-wrap-to-demote-errors)
  "List of wrapper functions to be called after macro expansion.")

(defvar setup-macros nil
  "Local macro definitions to be bound in `setup' bodies.
Do not modify this variable by hand.  Instead use
`setup-define.'")

(defun setup-expand-local-macros (body name)
  "Expand macros in BODY given by `setup-macros'.
NAME is a symbol or string designating the default feature."
  (macroexpand-all
      (if (assq :with-feature setup-macros)
          `(:with-feature ,name ,@body)
        (macroexp-progn body))
      (append setup-macros macroexpand-all-environment)))

;;;###autoload
(defun setup-make-docstring ()
  "Return a docstring for `setup'."
  (with-temp-buffer
    (insert (documentation (symbol-function 'setup) 'raw)
            "\n\n")
    (if (null setup-macros)
        (insert "No local macros are defined.")
      (insert "Within BODY, `setup' provides these local macros:")
      (dolist (sym (sort (mapcar #'car setup-macros) #'string-lessp))
        (newline 2)
        (let ((sig (mapcar
                    (lambda (arg)
                      (if (string-match "\\`&" (symbol-name arg))
                          arg
                        (intern (upcase (symbol-name arg)))))
                    (get sym 'setup-signature))))
          (insert (format " - %s\n\n" (cons sym sig))
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
           (indent defun))
  (when (consp name)
    (push name body)
    (let ((shorthand (get (car name) 'setup-shorthand)))
      (setq name (and shorthand (funcall shorthand name)))))
  (let ((setup-attributes setup-attributes))
    (dolist (mod-fn setup-modifier-list)
      (setq body (funcall mod-fn body name)))
    body))

;;;###autoload
(put 'setup 'function-documentation '(setup-make-docstring))

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
If not given, it is assumed nothing is evaluated."
  (declare (indent 2))
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
  (put name 'lisp-indent-function (plist-get opts :indent))
  ;; define macro for `macroexpand-all'
  (setf (alist-get name setup-macros)   ;New in Emacs-25.
        (let* ((arity (if (eq (plist-get opts :repeatable) t)
                          (car (func-arity fn))
                        (plist-get opts :repeatable)))
               (fn (if (null arity) fn
                     (lambda (&rest args)
                       (unless (zerop (mod (length args) arity))
                         (error "Illegal arguments"))
                       (let (aggr)
                         (while args
                           (let ((rest (nthcdr arity args)))
                             (setf (nthcdr arity args) nil)
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

(defun setup-xref-def-function (symbol)
  "Return an elisp xref location for SYMBOL."
  (and (assq symbol setup-macros)
       (let ((file (get symbol 'setup-definition-file)))
         (list (elisp--xref-make-xref nil symbol file)))))

(add-to-list 'elisp-xref-find-def-functions
             #'setup-xref-def-function)


;;; Common utility functions for keywords

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

(defun setup-quit (&optional return)
  "Generate code to quit evaluation.
If RETURN is given, throw that value."
  (push 'need-quit setup-attributes)
  `(throw ',(setup-get 'quit) ,return))

(defun setup-ensure-kbd (sexp)
  "Attempt to return SEXP as a key binding expression."
  (cond ((stringp sexp) (kbd sexp))
        ((symbolp sexp) `(kbd ,sexp))
        (sexp)))

(defun setup-ensure-function (sexp)
  "Attempt to return SEXP as a quoted function name."
  (cond ((eq (car-safe sexp) 'function)
         sexp)
        ((eq (car-safe sexp) 'quote)
         `#',(cadr sexp))
        ((symbolp sexp)
         `#',sexp)
        (sexp)))

(defun setup-make-setter (name val old-val-fn wrap-fn)
  "Convert NAME and VAL into setter code.
The function OLD-VAL-FN is used to extract the old value of
VAL.  The function WRAP-FN combines the transformed values of NAME
and VAL into one s-expression."
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
        ((eq (car-safe name) 'prepend)
         (funcall wrap-fn
          (cadr name)
          (let ((sym (gensym)))
            `(let ((,sym ,val)
                   (list ,(funcall old-val-fn (cadr name))))
               (if (member ,sym list)
                   list
                 (cons ,sym list))))))
        ((eq (car-safe name) 'remove)
         (funcall wrap-fn
          (cadr name)
          `(remove ,val ,(funcall old-val-fn name))))
        ((error "Invalid option %S" name))))


;;; Definitions of `setup' keywords

(setup-define :with-feature
  (lambda (features &rest body)
    (let (bodies)
      (dolist (feature (if (listp features) features (list features)))
        (push (if feature
                  (let* ((mode (if (string-match-p "-mode\\'" (symbol-name feature))
                                   feature
                                 (intern (format "%s-mode" feature))))
                         (setup-opts `((feature . ,feature)
                                       (mode . ,mode)
                                       (hook . ,(intern (format "%s-hook" mode)))
                                       (map . ,(intern (format "%s-map" mode)))
                                       ,@setup-opts)))
                    (setup-expand body))
                body)
              bodies))
      (macroexp-progn (if features (nreverse bodies) body))))
  :documentation "Change the FEATURE that BODY is configuring.
This macro also declares a current mode by appending \"-mode\" to
FEATURE, unless it already ends with \"-mode\".
If FEATURE is a list, apply BODY to all elements of FEATURE."
  :debug '(sexp setup)
  :indent 1)

(setup-define :with-mode
  (lambda (modes &rest body)
    (let (bodies)
      (dolist (mode (if (listp modes) modes (list modes)))
        (push (let ((setup-opts `((mode . ,mode)
                                  (hook . ,(intern (format "%s-hook" mode)))
                                  (map . ,(intern (format "%s-map" mode)))
                                  ,@setup-opts)))
                (setup-expand body))
              bodies))
      (macroexp-progn (nreverse bodies))))
  :documentation "Change the MODE that BODY is configuring.
If MODE is a list, apply BODY to all elements of MODE."
  :debug '(sexp setup)
  :indent 1)

(setup-define :with-map
  (lambda (maps &rest body)
    (let (bodies)
      (dolist (map (if (listp maps) maps (list maps)))
        (push (let ((setup-opts (cons `(map . ,map) setup-opts)))
                (setup-expand body))
              bodies))
      (macroexp-progn (nreverse bodies))))
  :documentation "Change the MAP that BODY will bind to.
If MAP is a list, apply BODY to all elements of MAP."
  :debug '(sexp setup)
  :indent 1)

(setup-define :with-hook
  (lambda (hooks &rest body)
    (let (bodies)
      (dolist (hook (if (listp hooks) hooks (list hooks)))
        (push (let ((setup-opts (cons `(hook . ,hook) setup-opts)))
                (setup-expand body))
              bodies))
      (macroexp-progn (nreverse bodies))))
  :documentation "Change the HOOK that BODY will use.
If HOOK is a list, apply BODY to all elements of HOOK."
  :debug '(sexp setup)
  :indent 1)

(setup-define :package
  (lambda (package)
    `(unless (package-installed-p ',package)
       (unless (memq ',package package-archive-contents)
         (package-refresh-contents))
       (package-install ',package)))
  :documentation "Install PACKAGE if it hasn't been installed yet.
This macro can be used as NAME, and it will replace itself with
the first PACKAGE."
  :repeatable t
  :shorthand #'cadr)

(setup-define :require
  (lambda (feature)
    `(unless (require ',feature nil t)
       ,(setup-quit)))
  :documentation "Try to require FEATURE, or stop evaluating body.
This macro can be used as NAME, and it will replace itself with
the first FEATURE."
  :repeatable t
  :shorthand #'cadr)

(setup-define :global
  (lambda (key command)
    `(global-set-key
      ,(setup-ensure-kbd key)
      ,(setup-ensure-function command)))
  :documentation "Globally bind KEY to COMMAND."
  :debug '(form sexp)
  :repeatable t)

(setup-define :bind
  (lambda (key command)
    `(define-key ,(setup-get 'map)
       ,(setup-ensure-kbd key)
       ,(setup-ensure-function command)))
  :documentation "Bind KEY to COMMAND in current map."
  :after-loaded t
  :debug '(form sexp)
  :repeatable t)

(setup-define :unbind
  (lambda (key)
    `(define-key ,(setup-get 'map)
       ,(setup-ensure-kbd key)
       nil))
  :documentation "Unbind KEY in current map."
  :after-loaded t
  :debug '(form)
  :repeatable t)

(setup-define :rebind
  (lambda (key command)
    `(progn
       (dolist (key (where-is-internal ',command ,(setup-get 'map)))
         (define-key ,(setup-get 'map) key nil))
       (define-key ,(setup-get 'map)
         ,(setup-ensure-kbd key)
         ,(setup-ensure-function command))))
  :documentation "Unbind the current key for COMMAND, and bind it to KEY."
  :after-loaded t
  :debug '(form sexp)
  :repeatable t)

(setup-define :hook
  (lambda (function)
    `(add-hook ',(setup-get 'hook) ,(setup-ensure-function function)))
  :documentation "Add FUNCTION to current hook."
  :repeatable t)

(setup-define :hook-into
  (lambda (mode)
    `(add-hook ',(let ((name (symbol-name mode)))
                   (if (string-match-p "-hook\\'" name)
                       mode
                     (intern (concat name "-hook"))))
               ,(setup-ensure-function (setup-get 'mode))))
  :documentation "Add current mode to HOOK."
  :repeatable t)

(setup-define :option
  (lambda (name val)
    (setup-make-setter
     name val
     (lambda (name)
       `(funcall (or (get ',name 'custom-get)
                     #'symbol-value)
                 ',name))
     (lambda (name val)
       `(progn
          (custom-load-symbol ',name)
          (funcall (or (get ',name 'custom-set) #'set-default)
                   ',name ,val)))))
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

Note that if the value of an option is modified partially by
append, prepend, remove, one should ensure that the default value
has been loaded. Also keep in mind that user options customized
with this macro are not added to the \"user\" theme, and will
therefore not be stored in `custom-set-variables' blocks."
  :debug '(sexp form)
  :repeatable t)

(setup-define :hide-mode
  (lambda ()
    `(setq minor-mode-alist
           (delq (assq ,(setup-get 'mode) minor-mode-alist)
                 minor-mode-alist)))
  :documentation "Hide the mode-line lighter of the current mode."
  :after-loaded t)

(setup-define :local-set
  (lambda (name val)
    (setup-make-setter
     name val
     (lambda (name)
       (if (consp name) (cadr name) name))
     (lambda (name val)
       `(add-hook ',(setup-get 'hook) (lambda () (setq-local ,name ,val))))))
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
                of VAL."
  :debug '(sexp form)
  :repeatable t)

(setup-define :local-hook
  (lambda (hook function)
    `(add-hook ',(setup-get 'hook)
               (lambda ()
                 (add-hook ',hook ,(setup-ensure-function function) nil t))))
  :documentation "Add FUNCTION to HOOK only in buffers of the current mode."
  :debug '(symbolp sexp)
  :repeatable t)

(setup-define :advise
  (lambda (symbol where function)
    `(advice-add ',symbol ,where ,(setup-ensure-function function)))
  :documentation "Add a piece of advice on a function.
See `advice-add' for more details."
  :after-loaded t
  :debug '(sexp sexp function-form)
  :repeatable t)

(setup-define :also-load
  (lambda (feature)
    `(require ',feature))
  :documentation "Load FEATURE with the current body."
  :after-loaded t
  :repeatable t)

(setup-define :needs
  (lambda (executable)
    `(unless (executable-find ,executable)
       ,(setup-quit)))
  :documentation "If EXECUTABLE is not in the path, stop here."
  :repeatable 1)

(setup-define :if-package
  (lambda (package)
    `(unless (package-installed-p ',package)
       ,(setup-quit)))
  :documentation "If package is not installed, stop evaluating the body.
This macro can be used as NAME, and it will replace itself with
the first PACKAGE."
  :repeatable t
  :shorthand #'cadr)

(setup-define :if-feature
  (lambda (feature)
    `(unless (featurep ',feature)
       ,(setup-quit)))
  :documentation "If FEATURE is not available, stop evaluating the body.
This macro can be used as NAME, and it will replace itself with
the first PACKAGE."
  :repeatable t
  :shorthand #'cadr)

(setup-define :if-host
  (lambda (hostname)
    `(unless (string= (system-name) ,hostname)
       ,(setup-quit)))
  :documentation "If HOSTNAME is not the current hostname, stop evaluating form.")

(setup-define :only-if
  (lambda (condition)
    `(unless ,condition
       ,(setup-quit)))
  :documentation "If CONDITION is non-nil, stop evaluating the body."
  :debug '(form)
  :repeatable t)

(setup-define :load-from
  (lambda (path)
    `(let ((path* (expand-file-name ,path)))
       (if (file-exists-p path*)
           (add-to-list 'load-path path*)
         ,(setup-quit))))
  :documentation "Add PATH to load path.
This macro can be used as NAME, and it will replace itself with
the nondirectory part of PATH.
If PATH does not exist, abort the evaluation."
  :shorthand (lambda (args) (intern
                             (file-name-nondirectory
                              (directory-file-name (cadr args))))))

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

(setup-define :without-error-demotion
    (lambda ()
      (setq setup-attributes (delq 'error-demotion setup-attributes))
      nil)
  :documentation "Prevent the setup body from demoting errors.
See `setup-wrap-to-demote-errors'.")

(provide 'setup)

;;; setup.el ends here
