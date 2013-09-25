;;; anything-git.el --- Show git files in anything

;; Author: Michael Rowe
;; Uploaded: Ben Grabham
;; Version: 1.1.1

(defvar anything-c-source-git-project-files-cache nil "(path signature cached-buffer)")
(defvar anything-c-source-git-project-files
  '((name . "Files from Current GIT Project")
    (init . (lambda ()
              (let* ((top-dir (file-truename (magit-get-top-dir (if (buffer-file-name)
                                                                    (file-name-directory (buffer-file-name))
                                                                  default-directory))))
                     (default-directory top-dir)
                     (signature (magit-rev-parse "HEAD")))

                (unless (and anything-c-source-git-project-files-cache
                             (third anything-c-source-git-project-files-cache)
                             (equal (first anything-c-source-git-project-files-cache) top-dir)
                             (equal (second anything-c-source-git-project-files-cache) signature))
                  (if (third anything-c-source-git-project-files-cache)
                      (kill-buffer (third anything-c-source-git-project-files-cache)))
                  (setq anything-c-source-git-project-files-cache
                        (list top-dir
                              signature
                              (anything-candidate-buffer 'global)))
                  (with-current-buffer (third anything-c-source-git-project-files-cache)
                    (dolist (filename (mapcar (lambda (file) (concat default-directory file))
                                              (magit-git-lines "ls-files")))
                      (insert filename)
                      (newline))))
                (anything-candidate-buffer (third anything-c-source-git-project-files-cache)))))

    (type . file)
    (candidates-in-buffer)))

(defun git-load ()
   (interactive)
   (anything-other-buffer
    'anything-c-source-git-project-files
    "*git-load*"))

;;; anything-git.el ends here
