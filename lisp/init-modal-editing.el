;; -*- lexical-binding: t; -*-

(require-package 'modalka)
(add-hook 'text-mode-hook 'modalka-mode)
(add-hook 'prog-mode-hook 'modalka-mode)

(setq-default cursor-type 'bar)
(setq-default modalka-cursor-type 'box)

(define-key global-map (kbd "C-g") 'modalka-mode)

(require-package 'beacon)
(setq-default beacon-lighter "")
(setq-default beacon-size 20)
(add-hook 'after-init-hook 'beacon-mode)

(with-eval-after-load 'modalka
  (define-key modalka-mode-map (kbd "i") 'modalka-mode)
  (define-key modalka-mode-map (kbd "C-g") 'keyboard-quit)

  (defun modalka-error ()
    (interactive)
    (ding)
    (message "Control keys are bad you know?"))

  (define-key modalka-mode-map (kbd "C-n") 'modalka-error)
  (define-key modalka-mode-map (kbd "C-p") 'modalka-error)
  (define-key modalka-mode-map (kbd "C-f") 'modalka-error)
  (define-key modalka-mode-map (kbd "C-b") 'modalka-error)

  (define-key modalka-mode-map (kbd "k") 'next-line)
  (define-key modalka-mode-map (kbd "l") 'previous-line)
  (define-key modalka-mode-map (kbd "j") 'backward-char)
  (define-key modalka-mode-map (kbd ";") 'forward-char)

  (define-key modalka-mode-map (kbd "C-x") 'modalka-error)
  (define-key modalka-mode-map (kbd "C-c") 'modalka-error)

  (define-key modalka-mode-map (kbd "b") 'switch-to-buffer)
  (define-key modalka-mode-map (kbd "B") 'ibuffer)
  (define-key modalka-mode-map (kbd "f") 'find-file)
  (define-key modalka-mode-map (kbd "K") (lambda () (interactive)
                                           (kill-buffer nil)))

  (define-key modalka-mode-map (kbd "c") 'sujay/org-capture)
  (define-key modalka-mode-map (kbd "a") (lambda () (interactive)
                                           (if (and (boundp 'org-agenda-buffer)
                                                    org-agenda-buffer)
                                               (switch-to-buffer org-agenda-buffer)
                                             (org-agenda nil "g"))))
  (define-key modalka-mode-map (kbd "t") 'org-todo)

  (define-key modalka-mode-map (kbd "e") (lambda () (interactive) (move-end-of-line 1)))

  (define-key modalka-mode-map (kbd "g") 'magit-status)

  (define-key modalka-mode-map (kbd "0") 'delete-window)
  (define-key modalka-mode-map (kbd "1") 'sanityinc/toggle-delete-other-windows)
  (define-key modalka-mode-map (kbd "2") (split-window-func-with-other-buffer 'split-window-vertically))
  (define-key modalka-mode-map (kbd "3") (split-window-func-with-other-buffer 'split-window-horizontally))
  (define-key modalka-mode-map (kbd "o") 'other-window))
