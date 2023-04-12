;; -*- lexical-binding: t -*-

;; No face for weekends
(setq org-agenda-weekend-days nil)

;; No face for mouse highlighting
(add-hook 'org-agenda-finalize-hook
          (lambda () (remove-text-properties
                 (point-min) (point-max) '(mouse-face t))))
