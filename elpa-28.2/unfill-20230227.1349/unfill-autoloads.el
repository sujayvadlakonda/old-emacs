;;; unfill-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "unfill" "unfill.el" (0 0 0 0))
;;; Generated autoloads from unfill.el

(autoload 'unfill-paragraph "unfill" "\
Replace newline chars in current paragraph by single spaces.
This command does the inverse of `fill-paragraph'." t nil)

(autoload 'unfill-region "unfill" "\
Replace newline chars in region from START to END by single spaces.
This command does the inverse of `fill-region'.

\(fn START END)" t nil)

(autoload 'unfill-toggle "unfill" "\
Toggle filling/unfilling of the current region.
Operates on the current paragraph if no region is active." t nil)

(define-obsolete-function-alias 'toggle-fill-unfill 'unfill-toggle "0.2")

;;;***

;;;### (autoloads nil nil ("unfill-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; unfill-autoloads.el ends here
