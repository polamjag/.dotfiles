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
    (setq enh-ruby-deep-indent-paren nil)
    (yard-mode)
    (eldoc-mode)))


;;; web-mode
(add-to-list 'auto-mode-alist '("\\.html?$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tt\\'" . web-mode))
(add-hook
 'web-mode-hook
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
(add-hook
 'scss-mode-hook
 (lambda ()
   (setq css-indent-offset 2
         scss-compile-at-save nil)))


;;; js2-mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-hook
 'js2-mode-hook
 '(lambda ()
    (setq js2-basic-offset 2
          js2-bounce-indent-p t)))


;;; shellscript-mode
(add-hook
 'shellscript-mode-hook
 (progn
   (setq sh-basic-offset 2
         sh-indentation 2
         sh-indent-for-case-label 0
         sh-indent-for-case-alt '+)))


;;; cider / clojure
(setq cider-repl-wrap-history t)
(add-hook 'clojure-mode-hook 'cider-mode)
(add-hook
 'cider-mode-hook
 (progn
   (require 'ac-cider)
   (ac-flyspell-workaround)
   (ac-cider-setup)))
(add-hook 'cider-repl-mode-hook 'ac-cider-setup)


;;; markdown-mode / plain text
(add-to-list 'auto-mode-alist '("\\.txt\\'"      . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.text\\'"     . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mkd\\'"      . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'"       . markdown-mode))
(add-to-list 'auto-mode-alist '("README"         . markdown-mode))
(add-hook
 'markdown-mode-hook
 '(lambda ()
    (electric-indent-local-mode -1)
    (setq markdown-enable-math t)))


;;; c
(add-hook
 'c-mode-hook
 (lambda ()
   (c-turn-on-eldoc-mode)))


;;; go
(add-hook
 'go-mode-hook
 '(lambda()
    (require 'go-autocomplete)
    (add-hook 'before-save-hook 'gofmt-before-save)
    (setq indent-tabs-mode t)
    (local-set-key (kbd "M-.") 'godef-jump)
    (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)
    (local-set-key (kbd "C-c i") 'go-goto-imports)
    (local-set-key (kbd "C-c d") 'godoc)))


;;; perl
(add-to-list 'auto-mode-alist '("\\.pl$" . cperl-mode))
(add-to-list 'auto-mode-alist '("\\.pm$" . cperl-mode))
(add-to-list 'auto-mode-alist '("\\.t$" .  cperl-mode))
(add-to-list 'auto-mode-alist '("cpanfile\\.?" .  cperl-mode))
(defun my-cperl-eldoc-documentation-function ()
  "Return meaningful doc string for `eldoc-mode'."
  (car
   (let ((cperl-message-on-help-error nil))
     (cperl-get-help))))
(add-hook
 'cperl-mode-hook
 '(lambda()
    (setq cperl-indent-level 4
          cperl-close-paren-offset -4
          cperl-continued-statement-offset 0
          cperl-indent-parens-as-block t
          cperl-eletric-parens nil
          cperl-tab-always-indent t
          cperl-indent-parens-as-block t
          cperl-indent-subs-specially nil
          cperl-highlight-variables-indiscriminately t)
    (eldoc-mode)
    (set (make-local-variable 'eldoc-documentation-function)
                 'my-cperl-eldoc-documentation-function)
    ))
;; helm-perldoc:setup takes long time on low power platform
(eval-after-load "cperl-mode"
  '(progn
    (helm-perldoc:setup)))

;; auto carton setup
(add-hook 'cperl-mode-hook 'helm-perldoc:carton-setup)


;; from: https://github.com/shibayu36/emacs/blob/master/emacs.d/inits/20-edit-mode-perl.el
(defun chomp (str)
  (replace-regexp-in-string "[\n\r]+$" "" str))
(defun git-project-p ()
  (string=
   (chomp
    (shell-command-to-string "git rev-parse --is-inside-work-tree"))
   "true"))
(defun git-root-directory ()
  (cond ((git-project-p)
         (chomp
          (shell-command-to-string "git rev-parse --show-toplevel")))
        (t
         "")))
(defun run-perl-method-test ()
  (interactive)
  (let ((topdir (git-root-directory))
        (test-method nil))
    (save-excursion
      (when (or
             (re-search-backward "\\bsub\s+\\([_[:alnum:]]+\\)\s*:\s*Test" nil t)
             (re-search-forward "\\bsub\s+\\([_[:alnum:]]+\\)\s*:\s*Test" nil t))
        (setq test-method (match-string 1))))
    (if test-method
        (quickrun
         :source
         `((:command . "prove")
           (:default-directory . ,topdir)
           (:exec . (,(concat "PERL5LIB=lib:local/lib/perl5:t/lib:$PERL5LIB TEST_METHOD=" test-method " %c -v %s")))))

      (quickrun
       :source
       `((:command . "prove")
         (:default-directory . ,topdir)
         (:exec . (("PERL5LIB=lib:local/lib/perl5:t/lib:$PERL5LIB %c -v %s"))))))))

(defun run-perl-test ()
  (interactive)
  (let* ((topdir (git-root-directory)))
    (quickrun :source `((:command . "prove")
                        (:default-directory . ,topdir)
                        (:exec . ("PERL5LIB=lib:local/lib/perl5:t/lib:$PERL5LIB %c -bv --color %s"))))))



;;; qml-mode
(add-to-list 'auto-mode-alist '("\\.qml$" . qml-mode))
(add-hook
 'qml-mode-hook
 (progn
   (setq qml-mode-indent-offset 2)
   (rainbow-delimiters-mode)))
