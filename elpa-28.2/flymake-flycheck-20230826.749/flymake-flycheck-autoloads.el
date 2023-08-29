;;; flymake-flycheck-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "flymake-flycheck" "flymake-flycheck.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from flymake-flycheck.el

(autoload 'flymake-flycheck-all-available-diagnostic-functions "flymake-flycheck" "\
Return a list of diagnostic functions for all usable checkers.
These might end up providing duplicate functionality, e.g. both
dash and bash might be used to check a `sh-mode' buffer if both are
found to be installed.

Usually you will want to use `flymake-flycheck-all-chained-diagnostic-functions' instead." nil nil)

(autoload 'flymake-flycheck-all-chained-diagnostic-functions "flymake-flycheck" "\
Return a list of diagnostic functions for the current checker chain." nil nil)

(autoload 'flymake-flycheck-auto "flymake-flycheck" "\
Activate all available flycheck checkers in the current buffer." nil nil)

(autoload 'flymake-flycheck-diagnostic-function-for "flymake-flycheck" "\
Wrap CHECKER to make a `flymake-diagnostics-functions' backend.

\(fn CHECKER)" nil nil)

(register-definition-prefixes "flymake-flycheck" '("flymake-flycheck-"))

;;;***

;;;### (autoloads nil nil ("flymake-flycheck-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; flymake-flycheck-autoloads.el ends here
