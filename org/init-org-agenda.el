;; -*- lexical-binding: t -*-

(defun sujay/org-agenda ()
  "If agenda buffer exists, switch to it and refresh. Else create new."
  (interactive)
  (if (and (boundp 'org-agenda-buffer)
           (buffer-live-p org-agenda-buffer))
      (progn
        (switch-to-buffer org-agenda-buffer)
        (org-agenda-redo))
    (org-agenda nil "GTD")))

(setq org-agenda-files '("~/gtd/inbox.org"
                         "~/gtd/gtd.org"
                         "~/gtd/tickler.org"
                         "~/gtd/hygiene.org"))


(setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)")))

(setq org-agenda-custom-commands
      '(("GTD" "GTD"
         ((todo "TODO"
                ((org-agenda-overriding-header "Next Actions")
                 (org-agenda-todo-ignore-scheduled 'future)
                 (org-agenda-todo-ignore-time-comparison-use-seconds t)
                 (org-agenda-skip-function #'next-actions-skip-function)))
          (todo "WAITING"
                ((org-agenda-overriding-header "Waiting")))))))

(defun next-actions-skip-function ()
  (let ((is-top-level (eq 1 (org-current-level)))
        (should-skip nil))
    (when (not is-top-level)
      (save-excursion
        (while (and (not should-skip)
                    (org-goto-sibling :previous))
          (when (string= "TODO" (org-get-todo-state))
            (setq should-skip t)))))
    (when should-skip
      (while (org-goto-sibling))
      (or (outline-next-heading)
          (goto-char (point-max))))))

;; Re-align tags when window shape changes
(with-eval-after-load 'org-agenda
  (add-hook 'org-agenda-mode-hook
            (lambda () (add-hook 'window-configuration-change-hook 'org-agenda-align-tags nil t))))

;; No face for weekends
(setq org-agenda-weekend-days nil)

;; No face for mouse highlighting
(add-hook 'org-agenda-finalize-hook
          (lambda () (remove-text-properties
                 (point-min) (point-max) '(mouse-face t))))

