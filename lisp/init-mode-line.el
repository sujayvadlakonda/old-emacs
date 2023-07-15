;; -*- lexical-binding: t -*-
;; Puts some information in echo area, and gets rid of mode line

(setq-default mode-line-format nil)

(require 'awesome-tray)
(setq awesome-tray-active-modules '("buffer-name" "date")
      awesome-tray-essential-modules '("buffer-name")
      awesome-tray-date-format "%l:%M %e %b %a")

(defun inherit! (child-face parent-face)
  (set-face-attribute child-face nil :inherit parent-face))

(inherit! 'awesome-tray-module-buffer-name-face 'font-lock-constant-face)
(inherit! 'awesome-tray-module-date-face 'default)

(awesome-tray-mode)

(provide 'init-mode-line)
