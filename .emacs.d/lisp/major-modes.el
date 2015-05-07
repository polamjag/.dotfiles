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
;;; scss
(add-hook 'scss-mode-hook
          (lambda ()
            (setq css-indent-offset 2
                  scss-compile-at-save nil)))


;;;; eshell
(global-set-key (kbd "C-=") '(lambda ()
                               (interactive)
                               (if (string= major-mode "eshell-mode") (previous-buffer) (eshell))))
(defun my-ac-eshell-mode ()
  (progn
    (ac-define-source pcomplete
      '((candidates . pcomplete-completions)))
    (setq ac-sources
          '(ac-source-pcomplete
            ac-source-filename
            ac-source-files-in-current-dir
            ac-source-words-in-buffer
            ac-source-dictionary))
    ))
(add-hook 'eshell-mode-hook
          '(lambda ()
             (progn
               (setq eshell-cmpl-ignore-case t
                     eshell-ask-to-save-history 'always
                     eshell-cmpl-cycle-completions t
                     eshell-hist-ignoredups t)
               (my-ac-eshell-mode)
               (define-key eshell-mode-map (kbd "C-i") 'auto-complete)
               (define-key eshell-mode-map (kbd "<tab>") 'auto-complete)
               (define-key eshell-mode-map "\C-a" 'eshell-bol)
               (define-key eshell-mode-map [up] 'previous-line)
               (define-key eshell-mode-map [down] 'next-line)
               (define-key eshell-mode-map "\C-p" 'eshell-previous-matching-input-from-input)
               (define-key eshell-mode-map "\C-n" 'eshell-next-matching-input-from-input)
               (define-key eshell-mode-map "\C-j" 'eshell-send-input)
               (define-key eshell-mode-map "\C-u" 'eshell-kill-input)
               (define-key eshell-mode-map "\C-r" 'helm-eshell-history)
               ;; aliases
               (require 'cl)
               (dolist
                   (l '(("ls" "ls -CFa")
                        ("l" "ls -CFal")
                        ("a" "cd ../ ;")
                        ("ff" "find-file")
                        ))
                 (add-to-list 'eshell-command-aliases-list l)))))
(add-hook 'eshell-preoutput-filter-functions
          'ansi-color-filter-apply)
