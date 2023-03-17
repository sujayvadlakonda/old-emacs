;; -*- lexical-binding: t; -*-

;; Remove trailing whitespace on save
;; Only in lines edited (prevent noisy git commits)
;; While preserving spaces in buffer (prevent cursor jumping on autosave)

(setq-default show-trailing-whitespace nil)

;; I only hooked prog-mode to preserve the virtual space
;; in capture buffers with auto-save-visited-mode
(use-package ws-butler
  :ensure t
  :diminish
  :hook prog-mode)
