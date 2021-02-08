`setup.el`
==========

The `setup' macro simplifies repetitive configuration patterns.  For
example, these macros:

    (setup shell
      (let ((key "C-c s"))
        (global (key shell))
        (bind (key bury-buffer))))


    (setup (package paredit)
      (hide-lighter)
      (hook-into scheme-mode lisp-mode))

will be replaced with the functional equivalent of

    (global-set-key (kbd "C-c s") #'shell)
    (with-eval-after-load 'shell
       (define-key shell-mode-map (kbd "C-c s") #'bury-buffer))


    (unless (package-install-p 'paredit)
      (package-install 'paredit ))
    (delq (assq 'paredit-mode minor-mode-alist)
          minor-mode-alist)
    (add-hook 'scheme-mode-hook #'paredit-mode)
    (add-hook 'lisp-mode-hook #'paredit-mode)

Additional "keywords" can be defined using `setup-define'. Invoke
the command `setup-help' to get a list of macros.

**Note:** This package is still being developed, and will probably
change a lot. See [this thread][thread] from emacs-devel for more
information.

Bugs
----

Bugs or comments can be submitted to my [public inbox][mail].
own.

Copying
-------

`setup.el` is distributed under the [CC0 1.0 Universal (CC0 1.0)
Public Domain Dedication][cc0] license. 

[thread]: https://lists.gnu.org/archive/html/emacs-devel/2021-02/msg00188.html
[mail]: https://lists.sr.ht/~zge/public-inbox
[cc0]: https://creativecommons.org/publicdomain/zero/1.0/deed
