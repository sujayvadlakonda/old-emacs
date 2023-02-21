(setq-default show-trailing-whitespace t)

;; I only hooked prog-mode to preserve the virtual space
;; in capture buffers with auto-save-visited-mode
(use-package ws-butler
  :ensure t
  :diminish
  :hook prog-mode)


(provide 'init-whitespace)
