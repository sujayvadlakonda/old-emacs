;; -*- lexical-binding: t; -*-

;; create unique key events for some key combinations
(define-key input-decode-map [?\C-i] [C-i])
(define-key input-decode-map [?\C-m] [C-m])

;; personal keyboard translations
(define-key key-translation-map (kbd "C-h") (kbd "DEL"))
