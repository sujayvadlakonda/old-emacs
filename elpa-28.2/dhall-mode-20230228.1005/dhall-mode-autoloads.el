;;; dhall-mode-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "dhall-mode" "dhall-mode.el" (0 0 0 0))
;;; Generated autoloads from dhall-mode.el

(autoload 'dhall-mode "dhall-mode" "\
Major mode for editing Dhall files.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.dhall\\'" . dhall-mode))

(register-definition-prefixes "dhall-mode" '("dhall-"))

;;;***

;;;### (autoloads nil nil ("dhall-mode-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; dhall-mode-autoloads.el ends here
