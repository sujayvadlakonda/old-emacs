;;; flymake-actionlint-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "flymake-actionlint" "flymake-actionlint.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from flymake-actionlint.el

(autoload 'flymake-actionlint-load "flymake-actionlint" "\
Load Flymake handler for `actionlint'." t nil)

(autoload 'flymake-actionlint-actions-file-p "flymake-actionlint" "\
Return non-nil iff FILENAME is a yaml file for GitHub Actions.

\(fn FILENAME)" nil nil)

(autoload 'flymake-actionlint-action-load-when-actions-file "flymake-actionlint" "\
Load Flymake handler when current file is yaml file for GitHub Actions." nil nil)

(register-definition-prefixes "flymake-actionlint" '("flymake-actionlint-"))

;;;***

;;;### (autoloads nil nil ("flymake-actionlint-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; flymake-actionlint-autoloads.el ends here
