(provide 'packages-list)

(require 'cl)

(defvar installing-package-list
  '(
    ;; language specific
    markdown-mode
    haskell-mode
    yaml-mode
    cider
    clojure-mode
    slamhound
    php-mode
    enh-ruby-mode
    ruby-block
    robe
    yard-mode
    web-mode
    jade-mode
    slim-mode
    sass-mode
    scss-mode
    less-css-mode
    js2-mode
    coffee-mode
    tuareg
    go-mode
    lua-mode
    auctex
    auctex-latexmk
    glsl-mode
    ;; flycheck
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
    helm-descbinds
    helm-swoop
    helm-git
    helm-ls-git
    helm-git-grep
    helm-robe
    helm-ghq
    ;; highlighting
    highlight-symbol
    auto-highlight-symbol
    ;; misc
    ddskk
    ctags
    ctags-update
    ggtags
    all
    multiple-cursors
    smartrep
    expand-region
    popwin
    quickrun
    rainbow-delimiters
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
