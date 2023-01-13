;;; init-abbrev.el --- Configure abbreviations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq save-abbrevs nil)

;; Common Abbreviations
(define-abbrev text-mode-abbrev-table "w" "with")
(define-abbrev text-mode-abbrev-table "wo" "without")
(define-abbrev text-mode-abbrev-table "bw" "between")
(define-abbrev text-mode-abbrev-table "tmrw" "tomorrow")

;; My Abbreviations
(define-abbrev text-mode-abbrev-table "inv" "investigate")
(define-abbrev text-mode-abbrev-table "oa" "online assessment")

;; Typos
(define-abbrev text-mode-abbrev-table "ot" "to")

(add-hook 'text-mode-hook 'abbrev-mode)

(provide 'init-abbrev)
;;; init-abbrev.el ends here
