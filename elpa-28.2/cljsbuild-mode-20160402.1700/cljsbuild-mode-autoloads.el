;;; cljsbuild-mode-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "cljsbuild-mode" "cljsbuild-mode.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from cljsbuild-mode.el

(autoload 'cljsbuild-mode "cljsbuild-mode" "\
ClojureScript Build mode

This is a minor mode.  If called interactively, toggle the
`Cljsbuild mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `cljsbuild-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(autoload 'cljsbuild-start "cljsbuild-mode" "\
Runs cljsbuild.

\(fn COMMAND)" t nil)

(register-definition-prefixes "cljsbuild-mode" '("cljsbuild-"))

;;;***

;;;### (autoloads nil nil ("cljsbuild-mode-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; cljsbuild-mode-autoloads.el ends here
