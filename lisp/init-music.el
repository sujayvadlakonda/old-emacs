;;; init-music.el --- Music -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (executable-find "mpv")
  (require-package 'mpv)

  (defun sujay/play-on-loop (path)
    "Play a song on loop."
    (interactive
     (list (read-file-name "Pick a song: " "~/audio/")))
    (mpv-volume-set 100)
    (mpv-play path)
    (mpv-set-property "loop-file" "inf")))


(when (executable-find "youtube-dl")
  (require-package 'ytdl)
  (setq ytdl-music-folder "~/audio/"
        ytdl-music-extra-args '("-x" "--audio-format" "best" "--audio-quality" "0")
        ytdl-always-query-default-filename 'never))


(provide 'init-music)
;;; init-music.el ends here
