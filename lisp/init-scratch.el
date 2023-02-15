;;; init-scratch.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; (require-package 'immortal-scratch)
;; (add-hook 'after-init-hook 'immortal-scratch-mode)

(defun remove-scratch-buffer ()
  (when (get-buffer "*scratch*")
    (kill-buffer "*scratch*")))

(add-hook 'after-change-major-mode-hook 'remove-scratch-buffer)

(provide 'init-scratch)
;;; init-scratch.el ends here
