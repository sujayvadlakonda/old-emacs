;;; init-music.el --- Manage local music collection -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defvar sujay/music-directory "~/audio/"
  "Where I store my music.")

(when (executable-find "mpv")
  (require-package 'mpv)

  (defun sujay/play-on-loop (path)
    "Play a song on loop."
    (interactive
     (list (read-file-name "Pick a song: " sujay/music-directory)))
    (mpv-play path)
    (mpv-set-property "loop-file" "inf")
    (mpv-volume-set 75)))


(when (or (executable-find "youtube-dl")
          (executable-find "yt-dlp"))
  (require-package 'ytdl)
  (setq ytdl-music-folder sujay/music-directory
        ytdl-music-extra-args '("-x" "--audio-format" "best" "--audio-quality" "0")
        ytdl-always-query-default-filename 'never))

(when (executable-find "yt-dlp")
  (setq ytdl-command "yt-dlp"))


(provide 'init-music)
;;; init-music.el ends here
