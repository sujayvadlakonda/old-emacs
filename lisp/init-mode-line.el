;;; init-mode-line.el --- Configure mode line -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq-default line-number-mode nil)
(setq-default column-number-mode nil)

(setq-default display-time-format "%a %d %b %H:%M")
(add-hook 'after-init-hook 'display-time-mode)


(provide 'init-mode-line)
;;; init-mode-line.el ends here
