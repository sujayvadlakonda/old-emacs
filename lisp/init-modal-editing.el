;; -*- lexical-binding: t; -*-

(require-package 'modalka)
(add-hook 'text-mode-hook 'modalka-mode)
(add-hook 'prog-mode-hook 'modalka-mode)
(diminish 'modalka-mode)

(setq-default cursor-type 'bar)
(setq-default modalka-cursor-type 'box)

(define-key global-map (kbd "C-g") 'modalka-mode)

(with-eval-after-load 'modalka
  ;; Insert new text
  (define-key modalka-mode-map (kbd "i") 'modalka-mode)

  ;; Move cursor
  (define-key modalka-mode-map (kbd "k") 'next-line)
  (define-key modalka-mode-map (kbd "l") 'previous-line)
  (define-key modalka-mode-map (kbd "j") 'backward-char)
  (define-key modalka-mode-map (kbd ";") 'forward-char)
  (define-key modalka-mode-map (kbd "u") 'backward-word)
  (define-key modalka-mode-map (kbd "o") 'forward-word)
  (define-key modalka-mode-map (kbd "h") 'back-to-indentation)
  (define-key modalka-mode-map (kbd "e") (lambda ()
                                           (interactive)
                                           (move-end-of-line 1)))
  (define-key modalka-mode-map (kbd "'") (lambda ()
                                           (interactive)
                                           (move-end-of-line 1)))
  ;; Text deletion
  ;; Undo
  ;; Kill Ring
  ;; Region
  ;; Mark
  ;; Smex
  ;; Search in buffer
  ;; Find replace
  ;; File commands (open, close, save)
  ;; Windows
  ;; Buffers
  ;; Quotes and Brackets
  ;; Leaders


  (define-key modalka-mode-map (kbd "v") 'set-mark-command)


  (define-key modalka-mode-map (kbd "b") 'switch-to-buffer)
  (define-key modalka-mode-map (kbd "B") 'ibuffer)
  (define-key modalka-mode-map (kbd "f") 'find-file)
  (define-key modalka-mode-map (kbd "K") (lambda () (interactive)
                                           (kill-buffer nil)))

  (define-key modalka-mode-map (kbd "c") 'sujay/org-capture)
  (define-key modalka-mode-map (kbd "a") 'sujay/org-agenda)
  (define-key modalka-mode-map (kbd "t") 'org-todo)


  (define-key modalka-mode-map (kbd "g") 'magit-status)

  (require-package 'multiple-cursors)
  (define-key modalka-mode-map (kbd "n") 'mc/mark-previous-like-this)
  (define-key modalka-mode-map (kbd "N") 'mc/skip-to-previous-like-this)
  (define-key modalka-mode-map (kbd "m") 'mc/mark-next-like-this)
  (define-key modalka-mode-map (kbd "M") 'mc/skip-to-next-like-this)

  (define-key modalka-mode-map (kbd "0") 'delete-window)
  (define-key modalka-mode-map (kbd "1") 'sanityinc/toggle-delete-other-windows)
  (define-key modalka-mode-map (kbd "2") (split-window-func-with-other-buffer 'split-window-vertically))
  (define-key modalka-mode-map (kbd "3") (split-window-func-with-other-buffer 'split-window-horizontally))
  (define-key modalka-mode-map (kbd "O") 'other-window)


  ;; Undefine keybindings to build muscle memory
  (defun modalka-error ()
    (interactive)
    (ding)
    (message "Control keys are bad you know?"))

  (define-key modalka-mode-map (kbd "C-n") 'modalka-error)
  (define-key modalka-mode-map (kbd "C-p") 'modalka-error)
  (define-key modalka-mode-map (kbd "C-f") 'modalka-error)
  (define-key modalka-mode-map (kbd "C-b") 'modalka-error)
  (define-key modalka-mode-map (kbd "C-x") 'modalka-error)
  (define-key modalka-mode-map (kbd "C-c") 'modalka-error)

  ;; Uncertain if necessary
  (define-key modalka-mode-map (kbd "C-g") 'keyboard-quit)
  )

(require-package 'evil)
