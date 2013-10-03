;;; java-file-create.el --- automatically insert contents of empty java files

;; Copyright (C) 2009 Nathaniel Flath <flat0103@gmail.com>

;; Author: Nathaniel Flath <flat0103@gmail.com>
;; URL: http://github.com/nflath/java-file-create
;; Version: 1.0

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file provides initial creation of .java files.  When a .java file is
;; open with no class definied, it will insert a package and class declaration
;; based on the filename.  The package is the relative path from the parent
;; 'src' directory

;;; Installation

;; To use this package, just put the following in your .emacs file:
;; (require 'java-file-create)

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(defun after-last (regexp string)
  "Returns the part of the string after the last occurrence of regexp."
  (let ((index (string-match regexp string)))
    (if index
        (after-last regexp (substring string (match-end 0) (length string)))
      string)))

(defun insert-java-class-template ()
  "Insert a template for a java class."
  (interactive)
  (end-of-buffer)
  (let ((start (string-match "src/\\(.*\\)" default-directory)))
    (when start
      (insert (concat "package "
                      (replace-regexp-in-string "/" "." (substring default-directory
                                                                   (+ 4 start)
                                                                   (1- (length default-directory))))
                      ";\n\n")))
    (skeleton-insert '(nil "public class " str " { \n\n}")
                     nil
                     (replace-regexp-in-string "\\.java" "" (after-last "/" (buffer-file-name))))))

(add-hook 'java-mode-hook (lambda ()  (if (not (string-match "class" (buffer-string)))
                                     (insert-java-class-template))))

(provide 'java-file-create)
;;; java-file-create.el ends here
