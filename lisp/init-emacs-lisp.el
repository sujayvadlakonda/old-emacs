;; -*- lexical-binding: t -*-

;; Make working with emacs-lisp pleasant and simple
;; Not for creating ELPA packages

;; Use `elisp-slime-nav-describe-elisp-thing-at-point' for help documentation
;; Use `elisp-slime-nav-find-elisp-thing-at-point' to jump to definition
;; Use `pop-tag-mark' to jump back

(require-package 'elisp-slime-nav)
(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
  (add-hook hook 'turn-on-elisp-slime-nav-mode))

(add-hook 'emacs-lisp-mode-hook (lambda () (setq mode-name "ELisp")))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (push '("lambda" . ?λ) prettify-symbols-alist)
            (prettify-symbols-mode)))

(require-package 'page-break-lines)
(add-hook 'emacs-lisp-mode-hook 'page-break-lines-mode)

(add-hook 'emacs-lisp-mode-hook (lambda () (flymake-mode -1)))


(provide 'init-emacs-lisp)
