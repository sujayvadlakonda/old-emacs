;;; init-music.el --- Music -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; (require-package 'bongo)
;; (setq bongo-enabled-backends '(mpv))
;; (setq bongo-mode-line-indicator-function 'ignore)
(when (executable-find "mpv")
  (require-package 'mpv)

  (defun sujay/play-on-loop (path)
    "Play a song on loop."
    (interactive
     (list (read-file-name "Pick a song: " "~/audio/")))
    (mpv-play path)
    (mpv-set-property "loop-file" "inf")))


(require-package 'ytdl)
(setq ytdl-music-folder "~/audio/"
      ytdl-always-query-default-filename 'never)

(provide 'init-music)
;;; init-music.el ends here
