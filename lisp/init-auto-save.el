;; -*- lexical-binding: t; -*-

(setq auto-save-visited-interval 2)
(add-hook 'after-init-hook 'auto-save-visited-mode)

(define-key global-map (kbd "C-x C-s") nil)
