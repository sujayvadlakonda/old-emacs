(package! 'evil)
(setq evil-disable-insert-state-bindings t)
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

;; Don't bind 't' in special buffers
(evil-global-set-key 'motion (kbd "t") nil)
(evil-global-set-key 'normal (kbd "t") 'evil-find-char-to)

;; Don't bind 'RET' in special buffers
(evil-global-set-key 'motion (kbd "RET") nil)

;; Rebind move to beginning of line
(evil-global-set-key 'motion (kbd "^") nil)
(evil-global-set-key 'motion (kbd "#") 'evil-first-non-blank)

(defvar sujay/leader-map (make-sparse-keymap))
(evil-global-set-key 'motion (kbd "SPC") sujay/leader-map)
(define-key sujay/leader-map (kbd "b") 'consult-buffer)
(define-key sujay/leader-map (kbd "f") 'find-file)
(define-key sujay/leader-map (kbd "s") 'save-buffer)
(define-key sujay/leader-map (kbd "0") 'delete-window)
(define-key sujay/leader-map (kbd "1") 'sanityinc/toggle-delete-other-windows)
(define-key sujay/leader-map (kbd "2")
  (split-window-func-with-other-buffer 'split-window-vertically))
(define-key sujay/leader-map (kbd "3")
  (split-window-func-with-other-buffer 'split-window-horizontally))
(define-key sujay/leader-map (kbd "o") 'switch-window)
(define-key sujay/leader-map (kbd "a") 'sujay/org-agenda)
(define-key sujay/leader-map (kbd "c") 'sujay/org-capture)
(define-key sujay/leader-map (kbd "g") 'magit)
(define-key sujay/leader-map (kbd "B") 'ibuffer)
(define-key sujay/leader-map (kbd "k") 'kill-this-buffer)
(define-key sujay/leader-map (kbd "m") 'increment-medicine)
(define-key sujay/leader-map (kbd "w") 'push-window-configuration)
(define-key sujay/leader-map (kbd "W") 'pop-window-configuration)

(package! 'evil-mc)
(global-evil-mc-mode)

(global-unset-key (kbd "C-x"))

(provide 'init-vim)
