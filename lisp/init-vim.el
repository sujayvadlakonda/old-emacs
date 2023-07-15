(require-package 'ryo-modal)

;; Enter vim
(define-key global-map (kbd "C-g") 'ryo-modal-mode)

(defun vim-vertical-split ()
  (interactive)
  (split-window-func-with-other-buffer 'split-window-vertically))

(defun vim-horizontal-split ()
  (interactive)
  (split-window-func-with-other-buffer 'split-window-horizontally))

(defun vim-delete-line ()
  (whole-line-or-region-kill-region))

(ryo-modal-keys
 ("C-g" keyboard-quit)

 ("i" ignore :exit t)
 ("a" forward-char :exit t)

 ("j" backward-char)
 ("k" next-line)
 ("l" previous-line)
 (";" forward-char)

 ("d" whole-line-or-region-kill-region)

 ("u" undo)

 ("SPC"
  (("b" consult-buffer)
   ("f" find-file)
   ("s" save-buffer)
   ("0" delete-window)
   ("1" sanityinc/toggle-delete-other-windows)
   ("2" vim-vertical-split)
   ("3" vim-horizontal-split)
   ("o" switch-window)
   ("a" sujay/org-agenda)
   ("c" sujay/org-capture)
   ("g" magit)
   ("B" ibuffer)
   ("k" kill-this-buffer)
   ("m" increment-medicine)
   ("w" push-window-configuration)
   ("W" pop-window-configuration))))

;; Modal editing
;; (require-package 'meow)

;; (defun meow-setup ()
;;   (meow-normal-define-key
;;    '("j" . meow-left)
;;    '("k" . meow-next)
;;    '("l" . meow-prev)
;;    '(";" . meow-right)

;;    '("d" . meow-delete)
;;    '("s" . whole-line-or-region-kill-region)
;;    '("y" . whole-line-or-region-kill-ring-save)
;;    '("p" . meow-yank)
;;    '("P" . meow-yank-pop)

;;    '("u" . meow-undo)

;;    '("x" . meow-line)
;;    '("w" . meow-mark-word)
;;    '("e" . meow-next-word)
;;    '("b" . meow-back-word)
;;    '("W" . meow-mark-symbol)
;;    '("E" . meow-next-symbol)
;;    '("B" . meow-back-symbol)

;;    '("c" . meow-change)

;;    '("0" . meow-expand-0)
;;    '("9" . meow-expand-9)
;;    '("8" . meow-expand-8)
;;    '("7" . meow-expand-7)
;;    '("6" . meow-expand-6)
;;    '("5" . meow-expand-5)
;;    '("4" . meow-expand-4)
;;    '("3" . meow-expand-3)
;;    '("2" . meow-expand-2)
;;    '("1" . meow-expand-1)

;;    '("z" . execute-extended-command))

(provide 'init-vim)
