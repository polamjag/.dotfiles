(provide 'packages-list)

(require 'cl)

(defvar installing-package-list
  '(
    markdown-mode
    haskell-mode
    yaml-mode
    open-junk-file
    all
    auto-complete
    ac-helm
    flymake
    flymake-easy
    flymake-haskell-multi
    flymake-php
    flymake-ruby
    flyphpcs
    flycheck
    flycheck-tip
    flex-autopair
    egg
    el-get
    magit
    caml
    helm
    helm-ag
    helm-descbinds
    helm-flymake
    helm-git
    helm-git-grep
    helm-rb
    highlight-symbol
    java-file-create
    multiple-cursors
    php-mode
    popup
    popwin
    quickrun
    rainbow-delimiters
    ruby-block
    ruby-end
    ruby-mode
    smartrep
    expand-region
    auto-highlight-symbol
    tuareg
    undo-tree
    web-mode
    whitespace-cleanup-mode
    yasnippet
    ))

(let ((not-installed (loop for x in installing-package-list
                            when (not (package-installed-p x))
                            collect x)))
  (when not-installed
    (package-refresh-contents)
    (dolist (pkg not-installed)
        (package-install pkg))))


;;
(require 'el-get)
(setq el-get-sources
      '(
        (:name anything-git-files
               :type github
               :pkgname "emacs-java/auto-java-complete")
        ))
(el-get 'sync)
