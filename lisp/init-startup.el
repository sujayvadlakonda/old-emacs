;;; -*- lexical-binding: t; -*-
;;; Hacks to reduce startup time

(unless after-init-time
  (setq gc-cons-threshold (* 128 1024 1024)))
(add-hook 'after-init-hook
          (lambda () (setq gc-cons-threshold (* 20 1024 1024))))


(defvar file-name-handler-alist-old file-name-handler-alist)
(unless after-init-time
  (setq file-name-handler-alist nil))
(add-hook 'after-init-hook
          #'(lambda ()
              (setq file-name-handler-alist file-name-handler-alist-old)))
