;; -*- lexical-binding: t -*-

;; Do not create files to keep track of files
(setq-default create-lockfiles nil
              auto-save-default nil
              make-backup-files nil)

;; Save to file when idle for 2 seconds
;; Might be dangerous, but waiting to take the first bullet
(setq auto-save-visited-interval 2)
(auto-save-visited-mode)

;; Update buffer when underlying file changes
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)
(global-auto-revert-mode)
(with-eval-after-load 'autorevert
  (diminish 'auto-revert-mode))

(defun create-missing-directories ()
  "Create any missing directories of the visited file."
  (let ((target-directory (file-name-directory buffer-file-name)))
    (unless (file-exists-p target-directory)
      (make-directory target-directory t))))

(add-to-list 'find-file-not-found-functions #'create-missing-directories)
