;; -*- lexical-binding: t; -*-

(set-frame-font "Source Code Pro")

;; Change global font size easily
(require-package 'default-text-scale)
(default-text-scale-mode)

(autoload 'default-text-scale-increment "default-text-scale")

(defun default-text-scale-set (new-height)
  "Set the default font size."
  (interactive "nNew Height: ")
  (let* ((current-height (face-attribute 'default :height))
         (increment (- new-height current-height)))
    (default-text-scale-increment increment)))

(default-text-scale-set 220)
