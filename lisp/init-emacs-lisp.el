;;; init-emacs-lisp.el --- Write emacs lisp -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'elisp-slime-nav)
(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook help-mode-hook))
  (add-hook hook 'turn-on-elisp-slime-nav-mode))

(add-hook 'emacs-lisp-mode-hook (lambda () (flymake-mode -1)))

(add-hook 'emacs-lisp-mode-hook (lambda () (setq mode-name "ELisp")))

(defun sanityinc/headerise-elisp ()
  "Add minimal header and footer to an elisp buffer in order to placate flycheck."
  (interactive)
  (let ((fname (if (buffer-file-name)
                   (file-name-nondirectory (buffer-file-name))
                 (error "This buffer is not visiting a file"))))
    (save-excursion
      (goto-char (point-min))
      (insert ";;; " fname " --- Insert description here -*- lexical-binding: t -*-\n"
              ";;; Commentary:\n"
              ";;; Code:\n\n")
      (goto-char (point-max))
      (insert ";;; " fname " ends here\n"))))


(provide 'init-emacs-lisp)
;;; init-emacs-lisp.el ends here
