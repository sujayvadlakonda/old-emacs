;;; init-auto-save.el --- Automatically save visited buffers -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq auto-save-visited-interval 2)
(add-hook 'after-init-hook 'auto-save-visited-mode)

(provide 'init-auto-save)
;;; init-auto-save.el ends here
