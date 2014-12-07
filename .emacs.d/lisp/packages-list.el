(provide 'packages-list)

(require 'cl)

(defvar installing-package-list
  '(
    ;; package managing
    el-get
    ;; language specific
    markdown-mode
    haskell-mode
    yaml-mode
    cider
    php-mode
    ruby-block
    ruby-end
    ruby-mode
    web-mode
    tuareg
    caml
    java-file-create
    go-mode
    ;; flymake / flycheck
    flymake
    flymake-easy
    flymake-haskell-multi
    flymake-php
    flymake-ruby
    flycheck
    flycheck-tip
    ;; git
    magit
    ;; helm
    helm
    helm-ag
    helm-descbinds
    helm-flymake
    helm-swoop
    helm-git
		helm-ls-git
    helm-git-grep
    helm-rb
    helm-ghq
    ;; highlighting
    highlight-symbol
    flex-autopair
    auto-highlight-symbol
    ;; misc
    dired+
    ctags
    ctags-update
    open-junk-file
    all
    company
    company-c-headers
    multiple-cursors
    smartrep
    expand-region
    popup
    popwin
    quickrun
    rainbow-delimiters
    undo-tree
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

(require 'el-get)
(setq el-get-sources
      '(
        (:name auto-java-complete
               :type github
               :pkgname "emacs-java/auto-java-complete")
        ))
(el-get 'sync)

;; proofgeneral
(if (file-readable-p "/usr/share/emacs/site-lisp/ProofGeneral/generic/proof-site.el")
    (load-file "/usr/share/emacs/site-lisp/ProofGeneral/generic/proof-site.el"))
