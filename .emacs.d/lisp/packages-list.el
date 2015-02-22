(provide 'packages-list)

(require 'cl)

(defvar installing-package-list
  '(
    ;; package managing
    ;el-get
    ;; language specific
    markdown-mode
    markdown-mode+
    haskell-mode
    yaml-mode
    cider
    clojure-mode
    slamhound
    php-mode
    ruby-block
    robe
    enh-ruby-mode
    yard-mode
    web-mode
    jade-mode
    slim-mode
    sass-mode
    less-css-mode
    scss-mode
    js2-mode
    tuareg
    caml
    java-file-create
    go-mode
    lua-mode
    auctex
    auctex-latexmk
    glsl-mode
    ;; flymake / flycheck
    flymake
    flymake-easy
    flymake-haskell-multi
    flymake-go
    flymake-php
    flymake-ruby
    flymake-sass
    flymake-shell
    flymake-yaml
    flycheck
    flycheck-tip
    ;; auto-complete
    auto-complete
    auto-complete-clang
    ac-cider
    ac-c-headers
    ac-dabbrev
    ac-helm
    ac-inf-ruby
    auto-complete-auctex
    go-autocomplete
    org-ac
    ;; eldoc
    c-eldoc
    cljdoc
    go-eldoc
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
    helm-robe
    helm-ghq
    ;; highlighting
    highlight-symbol
    auto-highlight-symbol
    ;; misc
    ddskk
    dired+
    ctags
    ctags-update
    ggtags
    open-junk-file
    all
    multiple-cursors
    smartrep
    expand-region
    popup
    popwin
    quickrun
    rainbow-delimiters
    smartparens
    undo-tree
    anzu
    yasnippet
    wc-mode
    ))

(let ((not-installed (loop for x in installing-package-list
                           when (not (package-installed-p x))
                           collect x)))
  (if (< (length not-installed) 0)
    (package-refresh-contents)
    (dolist (pkg not-installed)
      (progn (package-install pkg)(print not-installed)))))


;; proofgeneral
(if (file-readable-p "/usr/share/emacs/site-lisp/ProofGeneral/generic/proof-site.el")
    (load-file "/usr/share/emacs/site-lisp/ProofGeneral/generic/proof-site.el"))
