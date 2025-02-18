;;; bundler-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "bundler" "bundler.el" (0 0 0 0))
;;; Generated autoloads from bundler.el

(autoload 'bundle-open "bundler" "\
Queries for a gem name and opens the location of the gem in dired.

\(fn GEM-NAME)" t nil)

(autoload 'bundle-console "bundler" "\
Run an inferior Ruby process in the context of the current bundle." t nil)

(autoload 'bundle-check "bundler" "\
Run bundle check for the current bundle." t nil)

(autoload 'bundle-install "bundler" "\
Run bundle install for the current bundle." t nil)

(autoload 'bundle-update "bundler" "\
Run bundle update for the current bundle.

\(fn &optional UPDATE-CMD-ARGS)" t nil)

(autoload 'bundle-exec "bundler" "\


\(fn COMMAND)" t nil)

(autoload 'bundle-gemfile "bundler" "\
Set BUNDLE_GEMFILE environment variable.

\(fn &optional GEMFILE)" t nil)

(autoload 'bundle-outdated "bundler" "\
List installed gems with newer versions available." t nil)

(autoload 'bundle-major-version "bundler" "\
Returns the bundler major version. If no version is available it returns nil." nil nil)

(autoload 'bundle-show "bundler" "\
Shows all gems that are part of the bundle, or the path to a given gem." t nil)

(autoload 'bundle-version "bundler" "\
Prints version information." t nil)

(register-definition-prefixes "bundler" '("bundle-" "run-bundled-command"))

;;;***

;;;### (autoloads nil nil ("bundler-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; bundler-autoloads.el ends here
