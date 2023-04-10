;; -*- lexical-binding: t -*-

(setq debug-on-error nil)

(defun require-init (feature)
  "Require features without provide"
  (unless (featurep feature)
    (load (symbol-name feature))
    (provide feature)))


(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require-init 'init-benchmarking)

(defconst *is-a-mac* (eq system-type 'darwin))


(setq gc-cons-threshold (* 128 1024 1024))
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold (* 20 1024 1024))))

(setq custom-file null-device)
(require-init 'init-utils)
(require-init 'init-site-lisp)
(require-init 'init-package)
(require-init 'init-exec-path)

(require 'init-preload-local nil t)

;; Load configs for specific features and modes
(require-package 'diminish)
(require-package 'command-log-mode)

(require-init 'init-frame-hooks)
(require-init 'init-xterm)
(require-init 'init-themes)
(require-init 'init-osx-keys)
(require-init 'init-gui-frames)
(require-init 'init-dired)
(require-init 'init-isearch)
(require-init 'init-grep)
(require-init 'init-uniquify)
(require-init 'init-ibuffer)
(require-init 'init-flymake)
(require-init 'init-eglot)

(require-init 'init-recentf)
(require-init 'init-minibuffer)
(require-init 'init-hippie-expand)
(require-init 'init-corfu)
(require-init 'init-windows)
(require-init 'init-sessions)
(require-init 'init-mmm)

(require-init 'init-editing-utils)
(require-init 'init-whitespace)

(require-init 'init-vc)
(require-init 'init-darcs)
(require-init 'init-git)
(require-init 'init-github)

(require-init 'init-projectile)

(require-init 'init-compile)
(require-init 'init-crontab)
(require-init 'init-textile)
(require-init 'init-markdown)
(require-init 'init-csv)
(require-init 'init-erlang)
(require-init 'init-javascript)
(require-init 'init-php)
(require-init 'init-org)
(require-init 'init-nxml)
(require-init 'init-html)
(require-init 'init-css)
(require-init 'init-haml)
(require-init 'init-http)
(require-init 'init-python)
(require-init 'init-haskell)
(require-init 'init-elm)
(require-init 'init-purescript)
(require-init 'init-ruby)
(require-init 'init-rails)
(require-init 'init-sql)
(require-init 'init-ocaml)
(require-init 'init-j)
(require-init 'init-nim)
(require-init 'init-rust)
(require-init 'init-toml)
(require-init 'init-yaml)
(require-init 'init-docker)
(require-init 'init-terraform)
(require-init 'init-nix)
(maybe-require-package 'nginx-mode)
(require-init 'init-java)

;; (require-init 'init-paredit)
(require-init 'init-lisp)
(require-init 'init-emacs-lisp)
(require-init 'init-slime)
(require-init 'init-clojure)
(require-init 'init-common-lisp)


(require-init 'init-spelling)

(require-init 'init-misc)

(require-init 'init-folding)
(require-init 'init-dash)
(require-init 'init-ledger)

(require-init 'init-keyboard)
(require-init 'init-auto-save)
(require-init 'init-music)
(require-init 'init-mode-line)
(require-init 'init-abbrev)
(require-init 'init-file)
(require-init 'init-scratch)

;; Extra packages which don't require any configuration
(require-package 'sudo-edit)
(require-package 'gnuplot)
(require-package 'lua-mode)
(require-package 'htmlize)
(when *is-a-mac*
  (require-package 'osx-location))
(maybe-require-package 'dotenv-mode)
(maybe-require-package 'shfmt)
(require-package 'titlecase)

(require-package 'uptimes)
(setq-default uptimes-keep-count 200)
(require 'uptimes)

(require-init 'init-direnv)
(global-eldoc-mode -1)

;; Allow access from emacsclient
(require 'server)
(unless (server-running-p)
  (server-start))

;; Locales (setting them earlier in this file doesn't work in X)
(require-init 'init-locales)

;; Allow users to provide an optional "init-local" containing personal settings
(require 'init-local nil t)

(setq confirm-kill-processes nil)
(setq kill-buffer-query-functions
      (delq 'process-kill-buffer-query-function kill-buffer-query-functions))


(provide 'init)
