(setq debug-on-error nil)

(defun require-init (feature)
  "Require features without provide"
  (unless (featurep feature)
    (load (symbol-name feature))
    (provide feature)))


(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'init-benchmarking)

(defconst *is-a-mac* (eq system-type 'darwin))


(setq gc-cons-threshold (* 128 1024 1024))
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold (* 20 1024 1024))))

(setq custom-file null-device)
(require-init 'init-utils)
(require 'init-site-lisp)
(require 'init-package)
(require 'init-exec-path)

(require 'init-preload-local nil t)

;; Load configs for specific features and modes
(require-package 'diminish)
(require-package 'command-log-mode)

(require 'init-frame-hooks)
(require 'init-xterm)
(require 'init-themes)
(require 'init-osx-keys)
(require 'init-gui-frames)
(require 'init-dired)
(require 'init-isearch)
(require 'init-grep)
(require 'init-uniquify)
(require 'init-ibuffer)
(require 'init-flymake)
(require 'init-eglot)

(require 'init-recentf)
(require 'init-minibuffer)
(require 'init-hippie-expand)
(require 'init-corfu)
(require 'init-windows)
(require 'init-sessions)
(require 'init-mmm)

(require 'init-editing-utils)
(require 'init-whitespace)

(require 'init-vc)
(require 'init-darcs)
(require 'init-git)
(require 'init-github)

(require 'init-projectile)

(require 'init-compile)
(require 'init-crontab)
(require 'init-textile)
(require 'init-markdown)
(require 'init-csv)
(require 'init-erlang)
(require 'init-javascript)
(require 'init-php)
(require 'init-org)
(require 'init-nxml)
(require 'init-html)
(require 'init-css)
(require 'init-haml)
(require 'init-http)
(require 'init-python)
(require 'init-haskell)
(require 'init-elm)
(require 'init-purescript)
(require 'init-ruby)
(require 'init-rails)
(require 'init-sql)
(require 'init-ocaml)
(require 'init-j)
(require 'init-nim)
(require 'init-rust)
(require 'init-toml)
(require 'init-yaml)
(require 'init-docker)
(require 'init-terraform)
(require 'init-nix)
(maybe-require-package 'nginx-mode)
(require 'init-java)

(require 'init-paredit)
(require 'init-lisp)
(require 'init-emacs-lisp)
(require 'init-slime)
(require 'init-clojure)
(require 'init-common-lisp)
(require 'init-help)

(require 'init-spelling)

(require 'init-misc)

(require 'init-folding)
(require 'init-dash)
(require 'init-ledger)

(require 'init-keyboard-translations)
(require 'init-auto-save)
(require 'init-music)
(require 'init-mode-line)
(require 'init-abbrev)
(require 'init-file)
(require 'init-scratch)

;; Extra packages which don't require any configuration
(require-package 'sudo-edit)
(require-package 'gnuplot)
(require-package 'lua-mode)
(require-package 'htmlize)
(when *is-a-mac*
  (require-package 'osx-location))
(maybe-require-package 'dotenv-mode)
(maybe-require-package 'shfmt)
(require-package 'restart-emacs)
(require-package 'titlecase)

(when (maybe-require-package 'uptimes)
  (setq-default uptimes-keep-count 200)
  (add-hook 'after-init-hook (lambda () (require 'uptimes))))

(require 'init-direnv)
(global-eldoc-mode -1)

;; Allow access from emacsclient
(add-hook 'after-init-hook
          (lambda ()
            (require 'server)
            (unless (server-running-p)
              (server-start))))

;; Locales (setting them earlier in this file doesn't work in X)
(require 'init-locales)

;; Allow users to provide an optional "init-local" containing personal settings
(require 'init-local nil t)

(setq confirm-kill-processes nil)
(setq kill-buffer-query-functions
      (delq 'process-kill-buffer-query-function kill-buffer-query-functions))


(provide 'init)
