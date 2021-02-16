`setup.el`
==========

The `setup' macro simplifies repetitive configuration patterns.  For
example, these macros:

    (setup shell
      (let ((key "C-c s"))
        (:global (key shell))
        (:bind (key bury-buffer))))


    (setup (:package paredit)
      (:hide-mode)
      (:hook-into scheme-mode lisp-mode))


    (setup (:package yasnippet)
      (:with-mode yas-minor-mode
	(:rebind "<backtab>" yas-expand)
	(:option yas-prompt-functions '(yas-completing-prompt)
		 yas-wrap-around-region t)
	(:hook-into prog-mode)))

will be replaced with the functional equivalent of

    (global-set-key (kbd "C-c s") #'shell)
    (with-eval-after-load 'shell
       (define-key shell-mode-map (kbd "C-c s") #'bury-buffer))


    (unless (package-install-p 'paredit)
      (package-install 'paredit))
    (delq (assq 'paredit-mode minor-mode-alist)
          minor-mode-alist)
    (add-hook 'scheme-mode-hook #'paredit-mode)
    (add-hook 'lisp-mode-hook #'paredit-mode)


    (unless (package-install-p 'yasnippet)
      (package-install 'yasnippet))
    (with-eval-after-load 'yasnippet
      (dolist (key (where-is-internal 'yas-expand yas-minor-mode-map))
	(define-key yas-minor-mode-map key nil))
      (define-key yas-minor-mode-map "<backtab>" #'yas-expand)
      (customize-set-variable 'yas-prompt-functions '(yas-completing-prompt))
      (customize-set-variable 'yas-wrap-around-region t))
    (add-hook 'prog-mode-hook #'yas-minor-mode)

Additional "keywords" can be defined using `setup-define'. All known
keywords are documented in the docstring for `setup'.

**Note:** This package is still being developed, and will probably
change a lot. See [this thread][thread] from emacs-devel for more
information.

Bugs
----

Bugs or comments can be submitted to my [public inbox][mail].
own. Note that non-trivial contributions require a [copyright
assignment][ca] to the FSF.

Copying
-------

`setup.epl` is distributed under the [CC0 1.0 Universal (CC0 1.0)
Public Domain Dedication][cc0] license. 

[thread]: https://lists.gnu.org/archive/html/emacs-devel/2021-02/msg00188.html
[mail]: https://lists.sr.ht/~zge/public-inbox
[ca]: https://www.gnu.org/software/emacs/manual/html_node/emacs/Copyright-Assignment.html#Copyright-Assignment
[cc0]: https://creativecommons.org/publicdomain/zero/1.0/deed
