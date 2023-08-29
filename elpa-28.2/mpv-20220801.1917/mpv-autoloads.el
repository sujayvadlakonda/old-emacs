;;; mpv-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "mpv" "mpv.el" (0 0 0 0))
;;; Generated autoloads from mpv.el

(autoload 'mpv-play "mpv" "\
Start an mpv process playing the file at PATH.

You can use this with `org-add-link-type' or `org-file-apps'.
See `mpv-start' if you need to pass further arguments and
`mpv-default-options' for default options.

\(fn PATH)" t nil)

(autoload 'mpv-play-url "mpv" "\
Start an mpv process playing the video stream at URL.

See `mpv-start' if you need to pass further arguments and
`mpv-default-options' for default options.

\(fn URL)" t nil)

(autoload 'mpv--playlist-append "mpv" "\
Append THING to the current mpv playlist.

If ARGS are provided, they are passed as per-file options to mpv.

\(fn THING &rest ARGS)" nil nil)

(autoload 'mpv-quit "mpv" "\
Exit the current mpv process.

If WATCH-LATER is non-nil, tell mpv store the current playback
position for later.  When called interactively, prompt whether to
do so.

\(fn WATCH-LATER)" t nil)

(autoload 'mpv-kill "mpv" "\
Kill the mpv process." t nil)

(autoload 'mpv-pause "mpv" "\
Pause or unpause playback." t nil)

(autoload 'mpv-insert-playback-position "mpv" "\
Insert the current playback position at point.

When called with a non-nil ARG, insert a timer list item like `org-timer-item'.

\(fn &optional ARG)" t nil)

(autoload 'mpv-seek-to-position-at-point "mpv" "\
Jump to playback position as inserted by `mpv-insert-playback-position'.

This can be used with the `org-open-at-point-functions' hook." t nil)

(autoload 'mpv-speed-set "mpv" "\
Set playback speed to FACTOR.

\(fn FACTOR)" t nil)

(autoload 'mpv-speed-increase "mpv" "\
Increase playback speed by STEPS factors of `mpv-speed-step'.

\(fn STEPS)" t nil)

(autoload 'mpv-speed-decrease "mpv" "\
Decrease playback speed by STEPS factors of `mpv-speed-step'.

\(fn STEPS)" t nil)

(autoload 'mpv-volume-set "mpv" "\
Set playback volume to FACTOR.

\(fn FACTOR)" t nil)

(autoload 'mpv-volume-increase "mpv" "\
Increase playback volume by STEPS factors of `mpv-volume-step'.

\(fn STEPS)" t nil)

(autoload 'mpv-volume-decrease "mpv" "\
Decrease playback volume by STEPS factors of `mpv-volume-step'.

\(fn STEPS)" t nil)

(autoload 'mpv-seek "mpv" "\
Seek to the given (absolute) time in SECONDS.
A negative value is interpreted relative to the end of the file.

\(fn SECONDS)" t nil)

(autoload 'mpv-seek-forward "mpv" "\
Seek forward ARG seconds.
If ARG is numeric, it is used as the number of seconds.  Else each use
of \\[universal-argument] will add another `mpv-seek-step' seconds.

\(fn ARG)" t nil)

(autoload 'mpv-seek-backward "mpv" "\
Seek backward ARG seconds.
If ARG is numeric, it is used as the number of seconds.  Else each use
of \\[universal-argument] will add another `mpv-seek-step' seconds.

\(fn ARG)" t nil)

(autoload 'mpv-revert-seek "mpv" "\
Undo the previous seek command." t nil)

(autoload 'mpv-playlist-next "mpv" "\
Go to the next entry on the playlist." t nil)

(autoload 'mpv-playlist-prev "mpv" "\
Go to the previous entry on the playlist." t nil)

(autoload 'mpv-version "mpv" "\
Return the mpv version string.
When called interactively, also show a more verbose version in
the echo area." t nil)

(register-definition-prefixes "mpv" '("mpv-"))

;;;***

;;;### (autoloads nil nil ("mpv-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; mpv-autoloads.el ends here
