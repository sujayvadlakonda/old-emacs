;;; -*- lexical-binding: t; -*-
;;; All the keybindings

(package! 'multistate)
(multistate-global-mode)

(cl-defun define-state (name &key map (cursor t))
  "Define state while ignoring if state already exists."
  (ignore-errors
    (multistate-define-state name :parent map :cursor cursor)))

(define-state 'motion :map 'multistate-suppress-map)
(define-kbd multistate-motion-state-map "j" 'backward-char)
(define-kbd multistate-motion-state-map "k" 'next-line)
(define-kbd multistate-motion-state-map "l" 'previous-line)
(define-kbd multistate-motion-state-map ";" 'forward-char)

(define-kbd multistate-motion-state-map "z" 'execute-extended-command)

(defvar sujay/leader-map (make-sparse-keymap))
(define-kbd multistate-motion-state-map "SPC" sujay/leader-map)
(define-kbd sujay/leader-map "b" 'consult-buffer)
(define-kbd sujay/leader-map "f" 'find-file)
(define-kbd sujay/leader-map "s" 'save-buffer)
(define-kbd sujay/leader-map "0" 'delete-window)
(define-kbd sujay/leader-map "1" 'sanityinc/toggle-delete-other-windows)
(define-kbd sujay/leader-map "2" (split-window-func-with-other-buffer 'split-window-vertically))
(define-kbd sujay/leader-map "3" (split-window-func-with-other-buffer 'split-window-horizontally))
(define-kbd sujay/leader-map "o" 'switch-window)
(define-kbd sujay/leader-map "a" 'sujay/org-agenda)
(define-kbd sujay/leader-map "c" 'sujay/org-capture)
(define-kbd sujay/leader-map "g" 'magit)
(define-kbd sujay/leader-map "B" 'ibuffer)
(define-kbd sujay/leader-map "k" 'kill-this-buffer)
(define-kbd sujay/leader-map "m" 'increment-medicine)
(define-kbd sujay/leader-map "w" 'push-window-configuration)
(define-kbd sujay/leader-map "W" 'pop-window-configuration)

(define-state 'insert :cursor 'bar)
(define-kbd multistate-insert-state-map "ESC" 'multistate-normal-state)

(define-state 'normal :map 'multistate-motion-state-map)
(define-kbd multistate-normal-state-map "i" 'multistate-insert-state)

(package! 'whole-line-or-region)
(define-kbd multistate-normal-state-map "d" 'whole-line-or-region-kill-region)
(define-kbd multistate-normal-state-map "y" 'whole-line-or-region-kill-ring-save)

(define-kbd multistate-normal-state-map "p" 'yank)
(define-kbd multistate-normal-state-map "u" 'undo)

(package! 'multiple-cursors)
(define-kbd multistate-normal-state-map "n" 'mc/mark-next-like-this)
(define-kbd multistate-normal-state-map "m" 'mc/mark-previous-like-this)
(define-kbd multistate-normal-state-map "*" 'mc/mark-all-like-this)

(package! 'meow)
(require 'meow)
(define-kbd multistate-normal-state-map "w" 'meow-next-word)
(define-kbd multistate-normal-state-map "b" 'meow-back-word)

(define-kbd multistate-normal-state-map "o" (lambda ()
                                              (interactive)
                                              (multistate-insert-state)
                                              (end-of-line)
                                              (newline-and-indent)))

(define-kbd multistate-normal-state-map "O" (lambda ()
                                              (interactive)
                                              (multistate-insert-state)
                                              (beginning-of-line)
                                              (newline-and-indent)
                                              (previous-line)
                                              (indent-for-tab-command)))

(define-kbd multistate-normal-state-map "x" 'delete-char)

(add-hook 'prog-mode-hook 'multistate-normal-state)
(add-hook 'text-mode-hook 'multistate-normal-state)
;; (define-kbd modalka-mode-map "i" (lambda ()
;;                                    (interactive)
;;                                    (deactivate-mark)
;;                                    (modalka-mode -1)))

;; (define-kbd modalka-mode-map "a" (lambda ()
;;                                    (interactive)
;;                                    (modalka-mode -1)
;;                                    (forward-char)))

;; (define-kbd modalka-mode-map "v" 'set-mark-command)
(define-kbd multistate-normal-state-map "V" (lambda ()
                                              (interactive)
                                              (beginning-of-line)
                                              (set-mark-command nil)
                                              (end-of-line)
                                              (forward-char)))

(define-kbd multistate-normal-state-map "A" (lambda ()
                                              (interactive)
                                              (multistate-insert-state)
                                              (end-of-line)))
(define-kbd multistate-normal-state-map "I" (lambda ()
                                              (interactive)
                                              (multistate-insert-state)
                                              (back-to-indentation)))

;; (define-kbd modalka-mode-map "I" (lambda ()
;;                                    (interactive)
;;                                    (modalka-mode -1)
;;                                    (back-to-indentation)))

(define-kbd multistate-normal-state-map "c" (lambda ()
                                              (interactive)
                                              (whole-line-or-region-kill-region 1)
                                              (multistate-insert-state)))
(define-kbd multistate-normal-state-map "C" (lambda ()
                                              (interactive)
                                              (kill-line)
                                              (multistate-insert-state)))

;; (define-kbd modalka-mode-map "gg" 'beginning-of-buffer)
;; (define-kbd modalka-mode-map "G" 'end-of-buffer)
