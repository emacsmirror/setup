;;; setup.el --- Helpful Configuration Macro    -*- lexical-binding: t -*-

;; Copyright (C) 2021  Free Software Foundation, Inc.

;; Author: Philip K. <philipk@posteo.net>
;; Maintainer: Philip K. <philipk@posteo.net>
;; Version: 0.1.1
;; Package-Requires: ((emacs "26.1"))
;; Keywords: lisp, local

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

;; The `setup' macro simplifies repetitive configuration patterns.
;; For example, these macros:

;;     (setup shell
;;       (let ((key "C-c s"))
;;         (:global key shell)
;;         (:bind key bury-buffer)))
;;
;;     (setup (:package paredit)
;;       (:hide-mode)
;;       (:hook-into scheme-mode lisp-mode))
;;
;;    (setup (:package yasnippet)
;;      (:with-mode yas-minor-mode
;;      (:rebind "<backtab>" yas-expand)
;;      (:option yas-prompt-functions '(yas-completing-prompt)
;;               yas-wrap-around-region t)
;;      (:hook-into prog-mode)))

;; will be replaced with the functional equivalent of

;;     (global-set-key (kbd "C-c s") #'shell)
;;     (with-eval-after-load 'shell
;;        (define-key shell-mode-map (kbd "C-c s") #'bury-buffer))
;;
;;     (unless (package-install-p 'paredit)
;;       (package-install 'paredit ))
;;     (delq (assq 'paredit-mode minor-mode-alist)
;;           minor-mode-alist)
;;     (add-hook 'scheme-mode-hook #'paredit-mode)
;;     (add-hook 'lisp-mode-hook #'paredit-mode)
;;
;;    (unless (package-install-p 'yasnippet)
;;      (package-install 'yasnippet))
;;    (with-eval-after-load 'yasnippet
;;      (dolist (key (where-is-internal 'yas-expand yas-minor-mode-map))
;;      (define-key yas-minor-mode-map key nil))
;;      (define-key yas-minor-mode-map "<backtab>" #'yas-expand)
;;    (customize-set-variable 'yas-prompt-functions '(yas-completing-prompt))
;;    (customize-set-variable 'yas-wrap-around-region t))
;;    (add-hook 'prog-mode-hook #'yas-minor-mode)


;; Additional "keywords" can be defined using `setup-define'.  All
;; known keywords are documented in the docstring for `setup'.

;;; Code:

(defvar setup-macros nil
  "Local macro definitions to be bound in `setup' bodies.")

(defvar setup-edebug-specifications nil
  "Part of the edebug specification for `setup'.")

;;;###autoload
(defun setup-make-docstring ()
  "Return a docstring for `setup'."
  (with-temp-buffer
    (insert (documentation (symbol-function 'setup) 'raw))
    (dolist (sym (sort (mapcar #'car setup-macros)
                       #'string-lessp))
      (let ((sig (mapcar
                  (lambda (arg)
                    (if (string-match "\\`&" (symbol-name arg))
                        arg
                      (intern (upcase (symbol-name arg)))))
                  (get sym 'setup-signature))))
        (insert (format " - %s\n\n" (cons sym sig))
                (or (get sym 'setup-documentation)
                    "No documentation.")
                "\n\n")))
    (buffer-string)))

;;;###autoload
(defmacro setup (name &rest body)
  "Configure feature or subsystem NAME.
BODY may contain special forms defined by `setup-define', but
will otherwise just be evaluated as is.

The following local macros are defined in a `setup' body:\n\n"
  (declare (debug (&rest &or [symbolp sexp] form))
           (indent defun))
  (when (consp name)
    (let ((shorthand (get (car name) 'setup-shorthand)))
      (when shorthand
        (push name body)
        (setq name (funcall shorthand name)))))
  (macroexpand-all
   `(catch 'setup-exit
      (:with-feature ,name ,@body)
      t)
   (append setup-macros macroexpand-all-environment)))

;;;###autoload
(put 'setup 'function-documentation '(setup-make-docstring))

(defun setup-define (name fn &rest opts)
  "Define `setup'-local macro NAME using function FN.
The plist OPTS may contain the key-value pairs:

  :name NAME
Specify a function to use, for extracting the feature name of a
NAME entry, if it is the first element in a setup macro.

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

  :debug SPEC
A edebug specification, see Info node `(elisp) Specification List'.
If not given, it is assumed nothing is evaluated."
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
                         `(progn ,@(nreverse aggr)))))))
          (if (plist-get opts :after-loaded)
              (lambda (&rest args)
                `(with-eval-after-load setup-name ,(apply fn args)))
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


;;; definitions of `setup' keywords

(setup-define :with-feature
  (lambda (feature &rest body)
    `(let ((setup-name ',feature))
       (ignore setup-name)
       (:with-mode ,(if (string-match-p "-mode\\'" (symbol-name feature))
                        feature
                      (intern (format "%s-mode" feature)))
         ,@body)))
  :documentation "Change the FEATURE that BODY is configuring."
  :debug '(sexp setup)
  :indent 1)

(setup-define :with-mode
  (lambda (mode &rest body)
    `(let ((setup-mode ',mode)
           (setup-map ',(intern (format "%s-map" mode)))
           (setup-hook ',(intern (format "%s-hook" mode))))
       (ignore setup-mode setup-map setup-hook)
       ,@body))
  :documentation "Change the MODE that BODY is configuring."
  :debug '(sexp setup)
  :indent 1)

(setup-define :with-map
  (lambda (map &rest body)
    `(let ((setup-map ',map))
       ,@body))
  :documentation "Change the MAP that BODY will bind to"
  :debug '(sexp setup)
  :indent 1)

(setup-define :with-hook
  (lambda (hook &rest body)
    `(let ((setup-hook ',hook))
       ,@body))
  :documentation "Change the HOOK that BODY will use."
  :debug '(sexp setup)
  :indent 1)

(setup-define :package
  (lambda (package)
    `(unless (package-installed-p ',package)
       (package-install ',package)))
  :documentation "Install PACKAGE if it hasn't been installed yet."
  :shorthand #'cadr
  :repeatable t)

(setup-define :require
  (lambda (feature)
    `(require ',feature))
  :documentation "Eagerly require FEATURE."
  :shorthand #'cadr
  :repeatable t)

(setup-define :global
  (lambda (key command)
    `(global-set-key
      ,(cond ((stringp key) (kbd key))
             ((symbolp key) `(kbd ,key))
             (key))
      #',command))
  :documentation "Globally bind KEY to COMMAND."
  :debug '(form [&or [symbolp sexp] form])
  :repeatable t)

(setup-define :bind
  (lambda (key command)
    `(define-key (eval setup-map)
       ,(if (or (symbolp key) (stringp key))
            `(kbd ,key)
          ,key)
       #',command))
  :documentation "Bind KEY to COMMAND in current map."
  :after-loaded t
  :debug '(form [&or [symbolp sexp] form])
  :repeatable t)

(setup-define :unbind
  (lambda (key)
    `(define-key (symbol-value setup-map)
       ,(if (or (symbolp key) (stringp key))
              `(kbd ,key)
          ,key)
       nil))
  :documentation "Unbind KEY in current map."
  :after-loaded t
  :debug '(form)
  :repeatable t)

(setup-define :rebind
  (lambda (key command)
    `(progn
       (dolist (key (where-is-internal ',command (eval setup-map)))
         (define-key (eval setup-map) key nil))
       (define-key (eval setup-map)
         ,(if (or (symbolp key) (stringp key))
              `(kbd ,key)
            ,key)
         #',command)))
  :documentation "Unbind the current key for COMMAND, and bind it to KEY."
  :after-loaded t
  :repeatable t)

(setup-define :hook
  (lambda (function)
    `(add-hook setup-hook #',function))
  :documentation "Add FUNCTION to current hook."
  :debug '(form [&or [symbolp sexp] form])
  :repeatable t)

(setup-define :hook-into
  (lambda (mode)
    `(add-hook ',(intern (concat (symbol-name mode) "-hook"))
               setup-mode))
  :documentation "Add current mode to HOOK."
  :repeatable t)

(setup-define :option
  (lambda (name val)
    (cond ((symbolp name) t)
          ((eq (car-safe name) 'append)
           (setq name (cadr name)
                 val `(append (funcall (or (get ',name 'custom-get)
                                           #'symbol-value)
                                       ',name)
                              (list ,val))))
          ((eq (car-safe name) 'prepend)
           (setq name (cadr name)
                 val `(cons ,val
                            (funcall (or (get ',name 'custom-get)
                                         #'symbol-value)
                                     ',name))))
          ((error "Invalid option %S" name)))
    `(customize-set-variable ',name ,val "Modified by `setup'"))
  :documentation "Set the option NAME to VAL.

NAME may be a symbol, or a cons-cell.  If NAME is a cons-cell, it
will use the car value to modify the behaviour.  If NAME has the
form (append VAR), VAL is appended to VAR.  If NAME has the
form (prepend VAR), VAL is prepended to VAR."
  :debug '(sexp form)
  :repeatable t)

(setup-define :hide-mode
  (lambda ()
    `(delq (assq setup-mode minor-mode-alist)
           minor-mode-alist))
  :documentation "Hide the mode-line lighter of the current mode."
  :after-loaded t)

(setup-define :local-set
  (lambda (name val)
    (cond ((symbolp name) t)
          ((eq (car-safe name) 'append)
           (setq name (cadr name)
                 val `(append ,name (list val))))
          ((eq (car-safe name) 'prepend)
           (setq name (cadr name)
                 val `(cons ,val ,name)))
          ((error "Invalid variable %S" name)))
    `(add-hook setup-hook (lambda () (setq-local ,name ,val))))
  :documentation "Set the value of NAME to VAL in buffers of the current mode.

NAME may be a symbol, or a cons-cell.  If NAME is a cons-cell, it
will use the car value to modify the behaviour.  If NAME has the
form (append VAR), VAL is appended to VAR.  If NAME has the
form (prepend VAR), VAL is prepended to VAR."
  :debug '(sexp form)
  :repeatable t)

(setup-define :local-hook
  (lambda (hook function)
    `(add-hook setup-hook
               (lambda ()
                 (add-hook ',hook #',function nil t))))
  :documentation "Add FUNCTION to HOOK only in buffers of the current mode."
  :debug '(symbolp form)
  :repeatable t)

(setup-define :also-load
  (lambda (feature)
    `(require ',feature))
  :documentation "Load FEATURE with the current body."
  :repeatable t
  :after-loaded t)

(setup-define :needs
  (lambda (executable)
    `(unless (executable-find ,executable)
       (throw 'setup-exit nil)))
  :documentation "If EXECUTABLE is not in the path, stop here."
  :repeatable 1)

(setup-define :only-if
  (lambda (condition)
    `(unless ,condition
       (throw 'setup-exit nil)))
  :documentation "If CONDITION is non-nil, stop evaluating the body."
  :debug '(form)
  :repeatable t)

(setup-define :when-loaded
  (lambda (&rest body) `(progn ,@body))
  :documentation "Evaluate BODY after the current feature has been loaded."
  :debug '(body)
  :after-loaded t)

(provide 'setup)

;;; setup.el ends here
