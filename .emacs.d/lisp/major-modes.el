(provide 'major-modes)

;;; ruby
(add-to-list 'auto-mode-alist '("\\.rb$"      . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$"    . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile$"    . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$"    . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Guardfile$"  . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$"   . enh-ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))
(add-hook
 'enh-ruby-mode-hook
 '(lambda ()
    (require 'ruby-block)
    (abbrev-mode 1)
    (electric-indent-mode t)
    (electric-layout-mode t)
    (ruby-block-mode t)
    (setq ruby-block-highlight-toggle t)
    (setq enh-ruby-deep-indent-paren nil)))
(add-hook 'enh-ruby-mode 'yard-mode)
(add-hook 'enh-ruby-mode 'eldoc-mode)


;;; web-mode
(add-to-list 'auto-mode-alist '("\\.html?$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . web-mode))
(add-hook 'web-mode-hook
          '(lambda ()
             (setq web-mode-markup-indent-offset 2
                   web-mode-css-indent-offset 2
                   web-mode-code-indent-offset 2
                   web-mode-script-offset 2
                   web-mode-php-offset 2
                   web-mode-html-offset 2
                   web-mode-style-padding 2
                   web-mode-script-padding 2
                   indent-tabs-mode nil
                   tab-width 2)))


;;; scss
(add-hook 'scss-mode-hook
          (lambda ()
            (setq css-indent-offset 2
                  scss-compile-at-save nil)))


;;; js2-mode
(add-to-list 'auto-mode-alist '("\\.js\\'"   . js2-mode))
(add-hook 'js2-mode-hook
          '(lambda ()
             (setq js2-basic-offset 2
                   js2-bounce-indent-p t)))


;;; shellscript-mode
(setq sh-basic-offset 2
      sh-indentation 2
      sh-indent-for-case-label 0
      sh-indent-for-case-alt '+)


;;; cider / clojure
(setq cider-repl-wrap-history t)
(add-hook 'clojure-mode-hook 'cider-mode)
(require 'ac-cider)
(add-hook 'cider-mode-hook 'ac-flyspell-workaround)
(add-hook 'cider-mode-hook 'ac-cider-setup)
(add-hook 'cider-repl-mode-hook 'ac-cider-setup)
(eval-after-load "auto-complete"
  '(progn
     (add-to-list 'ac-modes 'cider-mode)
     (add-to-list 'ac-modes 'cider-repl-mode)))


;;; markdown-mode / plain text
(add-to-list 'auto-mode-alist '("\\.txt\\'"      . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.text\\'"     . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mkd\\'"      . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'"       . markdown-mode))
(setq markdown-enable-math t)


;;; c
(add-hook 'c-mode-hook
          (lambda ()
            (c-turn-on-eldoc-mode)))


;;; qml-mode
(add-to-list 'auto-mode-alist '("\\.qml$" . qml-mode))
(setq qml-mode-indent-offset 2)
(add-hook 'qml-mode-hook #'rainbow-delimiters-mode)
