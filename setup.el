;;; setup.el --- Helpful Configuration Macro    -*- lexical-binding: t -*-

;; Author: Philip K. <philipk@posteo.net>
;; Maintainer: Philip K. <philipk@posteo.net>
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1") (cl-lib "0.6.1"))
;; Keywords: lisp, local

;; This file is NOT part of Emacs.
;;
;; This file is in the public domain, to the extent possible under law,
;; published under the CC0 1.0 Universal license.
;;
;; For a full copy of the CC0 license see
;; https://creativecommons.org/publicdomain/zero/1.0/legalcode

;;; Commentary:

;; The `setup' macro simplifies repetitive configuration patterns.
;; For example, these macros:

;;     (setup shell
;;       (let ((key "C-c s"))
;;         (:global key shell)
;;         (:bind key bury-buffer)))
;;
;;
;;     (setup (:package paredit)
;;       (:hide-mode)
;;       (:hook-into scheme-mode lisp-mode))

;; will be replaced with the functional equivalent of

;;     (global-set-key (kbd "C-c s") #'shell)
;;     (with-eval-after-load 'shell
;;        (define-key shell-mode-map (kbd "C-c s") #'bury-buffer))
;;
;;
;;     (unless (package-install-p 'paredit)
;;       (package-install 'paredit ))
;;     (delq (assq 'paredit-mode minor-mode-alist)
;;           minor-mode-alist)
;;     (add-hook 'scheme-mode-hook #'paredit-mode)
;;     (add-hook 'lisp-mode-hook #'paredit-mode)

;; Additional "keywords" can be defined using `setup-define'.  All
;; known keywords are documented in the docstring for `setup'.

;;; Code:

(eval-when-compile (require 'cl-lib))


;;; `setup' macros

(defvar setup-macros nil
  "Local macro definitions to be bound in `setup' bodies.")

;;;###autoload
(defun setup-make-docstring ()
  "Return a docstring for `setup'."
  (with-temp-buffer
    (insert (documentation (symbol-function 'setup) 'raw))
    (dolist (sym (mapcar #'car setup-macros))
      (let ((sig (if (get sym 'setup-signature)
                     (cons sym (get sym 'setup-signature))
                   (list sym))))
        (insert "- " (prin1-to-string sig)
                "\n\n"
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
  (declare (debug (name &rest body)))
  (when (consp name)
    (let ((shorthand (get (car name) 'setup-shorthand)))
      (when shorthand
        (push name body)
        (setq name (funcall shorthand name)))))
  (let ((mode (if (string-match-p "-mode\\'" (symbol-name name))
                  name
                (intern (format "%s-mode" name)))))
    `(let ((setup-name ',name))
       (ignore setup-name)
       (cl-macrolet ,setup-macros
         (catch 'setup-exit
           (:with-mode ,mode ,@body))))))

;;;###autoload
(put 'setup 'function-documentation '(setup-make-docstring))

(defun setup-define (name fn &rest opts)
  "Define `setup'-local macro NAME using function FN.
The plist OPTS may contain the key-value pairs:

  :name
Specify a function to use, for extracting the feature name of a
NAME entry, if it is the first element in a setup macro.

  :indent
Change indentation behaviour.  See symbol `lisp-indent-function'.

  :after-loaded
Wrap the macro in a `with-eval-after-load' body.

  :signature
Give an advertised calling convention.

  :documentation
A documentation string."
  (declare (indent 1))
  (cl-assert (symbolp name))
  (cl-assert (functionp fn))
  (cl-assert (listp opts))
  ;; save metadata
  (put name 'setup-documentation (plist-get opts :documentation))
  (put name 'setup-signature (plist-get opts :signature))
  (put name 'setup-shorthand (plist-get opts :shorthand))
  (put name 'lisp-indent-function (plist-get opts :indent))
  (put name 'setup-indent (plist-get opts :indent))
  ;; forget previous definition
  (setq setup-macros (delq (assq name setup-macros)
                           setup-macros))
  ;; define macro for `cl-macrolet'
  (push (let* ((arity (func-arity fn))
               (body (if (plist-get opts :repeatable)
                         `(progn
                            (unless (zerop (mod (length args) ,(car arity)))
                              (error "Illegal arguments"))
                            (let (aggr)
                              (while args
                                (let ((rest (nthcdr ,(car arity) args)))
                                  (setf (nthcdr ,(car arity) args) nil)
                                  (push (apply #',fn args) aggr)
                                  (setq args rest)))
                              `(progn ,@(nreverse aggr))))
                       `(apply #',fn args))))
          (if (plist-get opts :after-loaded)
              `(,name (&rest args)
                      `(with-eval-after-load setup-name ,,body))
            `(,name (&rest args) `,,body)))
        setup-macros)
  (set-advertised-calling-convention name (plist-get opts :signature) nil))


;;; definitions of `setup' keywords

(setup-define :with-mode
  (lambda (mode &rest body)
    `(let ((setup-mode ',mode)
           (setup-map ',(intern (format "%s-map" mode)))
           (setup-hook ',(intern (format "%s-hook" mode))))
       (ignore setup-mode setup-map setup-hook)
       ,@body))
  :signature '(MODE &body BODY)
  :documentation "Change the MODE that BODY is configuring."
  :indent 1)

(setup-define ;with-map
  (lambda (map &rest body)
    `(let ((setup-map ',map))
       ,@body))
  :signature '(MAP &body BODY)
  :documentation "Change the MAP that BODY will bind to"
  :indent 1)

(setup-define :with-hook
  (lambda (hook &rest body)
    `(let ((setup-hook ',hook))
       ,@body))
  :signature '(HOOK &body BODY)
  :documentation "Change the HOOK that BODY will use."
  :indent 1)

(setup-define :package
  (lambda (package)
    `(unless (package-installed-p ',package)
       (package-install ',package)))
  :signature '(PACKAGE ...)
  :documentation "Install PACKAGE if it hasn't been installed yet."
  :shorthand #'cadr
  :repeatable t)

(setup-define :require
  (lambda (feature)
    `(require ',feature))
  :signature '(FEATURE ...)
  :documentation "Eagerly require FEATURE."
  :shorthand #'cadr
  :repeatable t)

(setup-define :global
  (lambda (key fn)
    `(global-set-key
      ,(cond ((stringp key) (kbd key))
             ((symbolp key) `(kbd ,key))
             (key))
      #',fn))
  :signature '(KEY FUNCTION ...)
  :documentation "Globally bind KEY to FUNCTION."
  :repeatable t)

(setup-define :bind
  (lambda (key fn)
    `(define-key (eval setup-map)
       ,(cond ((stringp key) (kbd key))
              ((symbolp key) `(kbd ,key))
              (key))
       #',fn))
  :signature '(KEY FUNCTION ...)
  :documentation "Bind KEY to FUNCTION in current map."
  :after-loaded t
  :repeatable t)

(setup-define :unbind
  (lambda (key)
    `(define-key
       ,(cond ((stringp key) (kbd key))
              ((symbolp key) `(kbd ,key))
              (key))
       nil))
  :signature '(KEY ...)
  :documentation "Unbind KEY in current map."
  :after-loaded t
  :repeatable t)

(setup-define :rebind
  (lambda (key fn)
    `(progn
       (dolist (key (where-is-internal ',fn))
         (define-key (eval setup-map) ,key nil))
       (define-key
         ,(cond ((stringp key) (kbd key))
                ((symbolp key) `(kbd ,key))
                (key))
         #',fn)))
  :signature '(KEY FUNCTION ...)
  :documentation "Unbind the current key for FUNCTION, and bind it to KEY."
  :after-loaded t
  :repeatable t)

(setup-define :hook
  (lambda (hook)
    `(add-hook setup-hook #',hook))
  :signature '(FUNCTION ...)
  :documentation "Add FUNCTION to current hook."
  :repeatable t)

(setup-define :hook-into
  (lambda (mode)
    `(add-hook ',(intern (concat (symbol-name mode) "-hook"))
               setup-mode))
  :signature '(HOOK ...)
  :documentation "Add current mode to HOOK."
  :repeatable t)

(setup-define :option
  (lambda (var val)
    (cond ((symbolp var)
           `(customize-set-variable ',var ,val "Modified by `setup'"))
          ((eq (car-safe var) 'append)
           `(customize-set-variable
             ',(cadr var)
             (append (funcall (or (get ',(cadr var) 'custom-get) 'symbol-value)
                              ',(cadr var))
                     (list ,val))
             "Modified by `setup'"))
          ((eq (car-safe var) 'prepend)
           `(customize-set-variable
             ',(cadr var)
             (cons ,val (funcall (or (get ',(cadr var) 'custom-get) 'symbol-value)
                                ',(cadr var)))
             "Modified by `setup'"))))
  :signature '(NAME VAL ...)
  :documentation "Set the option NAME to VAL.

NAME may be a symbol, or a cons-cell.  If NAME is a cons-cell, it
will use the car value to modify the behaviour.  If NAME has the
form (append VAR), "
  :repeatable t)

(setup-define :hide-mode
  (lambda ()
    `(delq (assq setup-mode minor-mode-alist)
           minor-mode-alist))
  :documentation "Hide the mode-line lighter of the current mode."
  :after-loaded t)

(setup-define :local-set
  (lambda (var val)
    `(add-hook setup-hook (lambda () (setq-local ,var ,val))))
  :signature '(VAR VAL ...)
  :documentation "Set the value of VAR to VAL in buffers of the current mode."
  :repeatable t)

(setup-define :local-hook
  (lambda (hook fn)
    `(add-hook setup-hook
               (lambda ()
                 (add-hook ',hook #',fn nil t))))
  :signature '(HOOK FUNCTION ...)
  :documentation "Add FUNCTION to HOOK only in buffers of the current mode."
  :repeatable t)

(setup-define :needs
  (lambda (binary)
    `(unless (executable-find ,binary)
       (throw 'setup-exit nil)))
  :signature '(PROGRAM ...)
  :documentation "If PROGRAM is not in the path, stop here."
  :repeatable t)

(setup-define :when-loaded
  (lambda (&rest body) `(progn ,@body))
  :signature '(&body BODY)
  :documentation "Evaluate BODY after the current feature has been loaded."
  :after-loaded t)

(provide 'setup)

;;; setup.el ends here
