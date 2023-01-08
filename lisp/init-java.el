;;; init-java.el --- Support for Java language -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'aggressive-indent)
(add-hook 'java-mode-hook 'aggressive-indent-mode)

(defun copy-buffer ()
  "Copy contents of buffer."
  (interactive)
  (kill-ring-save (point-min) (point-max)))

(defun replace-buffer ()
  "Replace contents of buffer with first item on kill ring."
  (interactive)
  (delete-region (point-min) (point-max))
  (yank))

(with-eval-after-load 'cc-mode
  (define-key java-mode-map (kbd "C-c y") 'copy-buffer)
  (define-key java-mode-map (kbd "C-c w") 'replace-buffer))

(provide 'init-java)
;;; init-java.el ends here
