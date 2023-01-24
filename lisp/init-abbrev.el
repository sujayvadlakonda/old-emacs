;;; init-abbrev.el --- Configure abbreviations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq save-abbrevs nil)

;; Common Abbreviations
(define-abbrev text-mode-abbrev-table "w" "with")
(define-abbrev text-mode-abbrev-table "wo" "without")
(define-abbrev text-mode-abbrev-table "bw" "between")
(define-abbrev text-mode-abbrev-table "tmrw" "tomorrow")
(define-abbrev text-mode-abbrev-table "ppl" "people")
(define-abbrev text-mode-abbrev-table "hw" "homework")
(define-abbrev text-mode-abbrev-table "yt" "youtube")

;; My Abbreviations
(define-abbrev text-mode-abbrev-table "apt" "apartment")
(define-abbrev text-mode-abbrev-table "appt" "appointment")
(define-abbrev text-mode-abbrev-table "bb" "basketball")
(define-abbrev text-mode-abbrev-table "vb" "volleyball")
(define-abbrev text-mode-abbrev-table "tt" "table tennis")
(define-abbrev text-mode-abbrev-table "oo" "object oriented")
(define-abbrev text-mode-abbrev-table "ood" "object oriented design")
(define-abbrev text-mode-abbrev-table "oop" "object oriented programming")
(define-abbrev text-mode-abbrev-table "ml" "machine learning")
(define-abbrev text-mode-abbrev-table "inv" "investigate")
(define-abbrev text-mode-abbrev-table "oa" "online assessment")
(define-abbrev text-mode-abbrev-table "gb" "grand blue")
(define-abbrev text-mode-abbrev-table "q" "question")
(define-abbrev text-mode-abbrev-table "y" "why")

;; Typos
(define-abbrev text-mode-abbrev-table "ot" "to")

(add-hook 'text-mode-hook 'abbrev-mode)
(with-eval-after-load 'abbrev
  (diminish 'abbrev-mode))

(provide 'init-abbrev)
;;; init-abbrev.el ends here
