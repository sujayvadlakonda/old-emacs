;; -*- lexical-binding: t; -*-

(set-frame-font "Fira Code")

(require-package 'default-text-scale)
(autoload 'default-text-scale-increment "default-text-scale")

(defun default-text-scale-set (new-height)
  "Set the default font size."
  (interactive "nNew Height: ")
  (let* ((current-height (face-attribute 'default :height))
         (increment (- new-height current-height)))
    (default-text-scale-increment increment)))

(default-text-scale-set 200)

;; Don't prompt to confirm theme safety. This avoids problems with
;; first-time startup on Emacs > 26.3.
(setq custom-safe-themes t)

(require-package 'modus-themes)

(defvar day-theme 'modus-operandi)
(defvar night-theme 'modus-vivendi)
(defvar day-hour 7)
(defvar night-hour 19)

(defun load-day-or-night-theme ()
  (mapc #'disable-theme custom-enabled-themes)

  (let ((current-hour (string-to-number (format-time-string "%H"))))
    (if (and (>= current-hour day-hour)
             (< current-hour night-hour))
        (load-theme day-theme :no-confirm)
      (load-theme night-theme :no-confirm))))


(let ((24-hours-in-seconds (* 24 60 60)))
  (run-at-time (concat (format "%d" day-hour) ":00") 24-hours-in-seconds #'load-day-or-night-theme)
  (run-at-time (concat (format "%d" night-hour) ":00") 24-hours-in-seconds #'load-day-or-night-theme))

(load-day-or-night-theme)

;; Don't dim in terminal windows. Even with 256 colours it can
;; lead to poor contrast.  Better would be to vary dimmer-fraction
;; according to frame type.
(when (display-graphic-p)
  (require-package 'dimmer)
  (setq-default dimmer-fraction 0.15)
  (dimmer-mode)
  (with-eval-after-load 'dimmer
    (advice-add 'frame-set-background-mode :after (lambda (&rest args) (dimmer-process-all)))))
