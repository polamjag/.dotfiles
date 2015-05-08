;; c.f. http://rubikitch.com/2015/01/04/esup/
(setq gc-cons-threshold 1073741824)

;;;; common load-path
(add-to-list 'load-path "~/.emacs.d/lisp/")


;;;; load packages
(require 'packages-list)


;;;; load local config
(if (file-readable-p "~/.emacs.d/lisp/env.el")
    (load-file "~/.emacs.d/lisp/env.el"))


;;;; initialize major modes
(require 'major-modes)
(if (window-system)
    (require 'eshell-config))


;;;; window and appearance preferences
(require 'appearance-common)
(if (window-system)
    (require 'appearance-window))
(require 'modeline)

;;;; global non-major and/or minor-modes
(require 'misc-common)


;;;; keybindings
(require 'keybinds)

(setq gc-cons-threshold 8388608)
