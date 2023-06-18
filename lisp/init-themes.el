;;; init-themes.el --- Defaults for themes -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Don't prompt to confirm theme safety. This avoids problems with
;; first-time startup on Emacs > 26.3.
(setq custom-safe-themes t)

(defvar sujay/active-theme nil "The active theme.")

(defun sujay/load-theme (theme)
  "Load a single theme."
  (interactive
   (list
    (intern (completing-read "Load custom theme: "
                             (mapcar #'symbol-name
                                     (custom-available-themes))))))
  (disable-theme 'sujay/active-theme)
  (load-theme theme)
  (setq sujay/active-theme theme))

(require-package 'modus-themes)
(sujay/load-theme 'modus-vivendi-tinted)

(provide 'init-themes)
;;; init-themes.el ends here
