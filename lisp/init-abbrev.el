;;; init-abbrev.el --- Configure abbreviations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq save-abbrevs nil)

;; Common Abbreviations
(define-abbrev text-mode-abbrev-table "w" "with")
(define-abbrev text-mode-abbrev-table "wo" "without")
(define-abbrev text-mode-abbrev-table "bw" "between")
(define-abbrev text-mode-abbrev-table "tmrw" "tomorrow")
(define-abbrev text-mode-abbrev-table "sun" "sunday")
(define-abbrev text-mode-abbrev-table "mon" "monday")
(define-abbrev text-mode-abbrev-table "tue" "tuesday")
(define-abbrev text-mode-abbrev-table "wed" "wednesday")
(define-abbrev text-mode-abbrev-table "thu" "thursday")
(define-abbrev text-mode-abbrev-table "fri" "friday")
(define-abbrev text-mode-abbrev-table "sat" "saturday")

;; My Abbreviations
(define-abbrev text-mode-abbrev-table "inv" "investigate")
(define-abbrev text-mode-abbrev-table "oa" "online assessment")

;; Typos
(define-abbrev text-mode-abbrev-table "ot" "to")

(add-hook 'text-mode-hook 'abbrev-mode)

(provide 'init-abbrev)
;;; init-abbrev.el ends here
