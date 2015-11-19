(add-to-list 'load-path "~/.emacs.d/lisp/")
(require 'packages-list)

(package-list-packages)
(package-menu-mark-upgrades)
(package-menu-execute t)
