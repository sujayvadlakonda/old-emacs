;;; init-mode-line.el --- Mode line optimization :) -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Don't display mode line
(setq-default mode-line-format nil)

(require 'awesome-tray)
(setq awesome-tray-active-modules '("buffer-name" "date")
      awesome-tray-essential-modules '("buffer-name")
      awesome-tray-date-format "%l:%M %e %b %a")


(awesome-tray-mode)

(provide 'init-mode-line)
;;; init-mode-line.el ends here
