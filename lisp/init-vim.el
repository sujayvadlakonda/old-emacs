(package! 'evil)
(setq evil-want-C-i-jump nil)
(setq evil-want-integration t)
(setq evil-want-keybinding nil)
(evil-mode)

(setq evil-undo-system 'undo-redo)

(evil-global-set-key 'insert (kbd "C-g") 'evil-normal-state)
(evil-global-set-key 'insert (kbd "<escape>") nil)

(evil-global-set-key 'motion (kbd "j") 'evil-backward-char)
(evil-global-set-key 'motion (kbd "k") 'evil-next-line)
(evil-global-set-key 'motion (kbd "l") 'evil-previous-line)
(evil-global-set-key 'motion (kbd ";") 'evil-forward-char)
(evil-global-set-key 'motion (kbd "h") 'evil-repeat-find-char)

(package! 'evil-mc)
(global-evil-mc-mode)

(package! 'evil-leader)
(global-evil-leader-mode)
(evil-leader/set-leader "SPC")
(evil-leader/set-key
  "b" 'consult-buffer
  "f" 'find-file
  "s" 'save-buffer
  "0" 'delete-window
  "1" 'sanityinc/toggle-delete-other-window
  "2" 'vim-vertical-split
  "3" 'vim-horizontal-split
  "o" 'switch-window
  "a" 'sujay/org-agenda
  "c" 'sujay/org-capture
  "g" 'magit
  "B" 'ibuffer
  "k" 'kill-this-buffer
  "m" 'increment-medicine
  "w" 'push-window-configuration
  "W" 'pop-window-configuration)

(provide 'init-vim)
