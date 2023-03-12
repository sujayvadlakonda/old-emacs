;; -*- lexical-binding: t -*-

(defun remove-scratch-buffer ()
  (when (get-buffer "*scratch*")
    (kill-buffer "*scratch*")))

(add-hook 'after-change-major-mode-hook 'remove-scratch-buffer)
