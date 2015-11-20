(provide 'packages-list)

(require 'cl)
(require 'package)

(setq package-archives
      (append
       '(("melpa"     . "https://melpa.org/packages/")
         ("marmalade" . "https://marmalade-repo.org/packages/")
         ("ELPA"      . "http://tromey.com/elpa/"))
       package-archives))

(package-initialize)

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
    elixir-mode
    alchemist
    yard-mode
    web-mode
    slim-mode
    sass-mode
    scss-mode
    less-css-mode
    js2-mode
    coffee-mode
    erlang
    tuareg
    go-mode
    lua-mode
    auctex
    auctex-latexmk
    glsl-mode
    qml-mode
    ;; flycheck
    flycheck
    flycheck-pos-tip
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

(let ((not-installed
      (loop for x in installing-package-list
            when (not (package-installed-p x))
            collect x)))
  (if (> (length not-installed) 0)
      (progn
        (package-refresh-contents)
        (dolist (pkg not-installed)
          (package-install pkg)))))


;; proofgeneral
(if (file-readable-p "/usr/share/emacs/site-lisp/ProofGeneral/generic/proof-site.el")
    (load-file "/usr/share/emacs/site-lisp/ProofGeneral/generic/proof-site.el"))
