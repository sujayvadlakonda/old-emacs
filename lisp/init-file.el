;;; init-file.el --- Configure Emacs file handling -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun create-missing-directories ()
  "Create any missing directories of the visited file."
  (let ((target-directory (file-name-directory buffer-file-name)))
    (unless (file-exists-p target-directory)
      (make-directory target-directory t))))

(add-to-list 'find-file-not-found-functions #'create-missing-directories)

(provide 'init-file)
;;; init-file.el ends here
