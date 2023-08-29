;;; ytdl-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ytdl" "ytdl.el" (0 0 0 0))
;;; Generated autoloads from ytdl.el

(autoload 'ytdl-select-format "ytdl" "\
Return a format for URL.

\(fn URL)" nil nil)

(autoload 'ytdl-download-eshell "ytdl" "\
Download file from a web server using ytdl in eshell.

Download the file from the provided url into the appropriate
folder location.  Query the download type and use the associated
destination folder and extra arguments, see
`ytdl-add-field-in-download-type-list'." t nil)

(autoload 'ytdl-download "ytdl" "\
Download asynchronously file from a web server." t nil)

(autoload 'ytdl-download-playlist "ytdl" "\
Download asynchronously playlist from a web server." t nil)

(autoload 'ytdl-download-open "ytdl" "\
Download file from a web server using and open it.

If URL is given as argument, then download file from URL.  Else
download the file from the URL stored in `current-ring'.

The file is opened with `ytdl-media-player'." t nil)

(autoload 'ytdl-open-last-downloaded-file "ytdl" "\
Open the last downloaded file in `ytdl-media-player'.

The last downloaded file is stored in
`ytdl--last-downloaded-file-name'." t nil)

(autoload 'ytdl-show-list "ytdl" "\
Open a new buffer and display `ytdl' download list." t nil)

(register-definition-prefixes "ytdl" '("ytdl-"))

;;;***

;;;### (autoloads nil nil ("ytdl-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ytdl-autoloads.el ends here
