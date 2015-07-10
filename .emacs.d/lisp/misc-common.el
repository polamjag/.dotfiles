;; -*- coding:utf-8 -*-

(provide 'misc-common)

;;; helm
(require 'helm-config)
(helm-mode t)
(global-set-key (kbd "C-c z") 'helm-resume)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key "\C-c\C-k" 'helm-show-kill-ring)
(define-key isearch-mode-map (kbd "C-o") 'helm-swoop-from-isearch)
(global-set-key "\C-x\C-b" 'helm-mini)
(global-set-key "\C-c\C-s" 'helm-swoop)
(global-set-key "\C-c\C-g" 'helm-git-grep)

;;; ido
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)


;;; quickrun
(global-set-key "\C-xx" 'quickrun)
(global-set-key "\C-xj" 'quickrun-with-arg)


;;; auto-complete
(global-auto-complete-mode 1)
(ac-config-default)
(setq ac-use-menu-map t)
(global-set-key "\C-cc" 'auto-complete-mode)


;;; flycheck
(add-hook 'after-init-hook 'global-flycheck-mode)
(require 'popup)
(require 'flycheck-pos-tip)
(eval-after-load 'flycheck
  '(custom-set-variables
   '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))


;;; eldoc
(setq eldoc-idle-delay 0.4
      eldoc-echo-area-use-multiline-p t)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)


(require 'undo-tree)
(global-undo-tree-mode t)
(global-set-key (kbd "M-/") 'undo-tree-redo)

(require 'expand-region)
(global-set-key (kbd "C-,") 'er/expand-region)
(global-set-key (kbd "C-M-,") 'er/contract-region)

(require 'multiple-cursors)
(global-set-key (kbd "<C-M-return>") 'mc/edit-lines)

(require 'smartrep)
(smartrep-define-key
    global-map "C-." '(("C-n" . 'mc/mark-next-like-this)
                       ("C-p" . 'mc/mark-previous-like-this)
                       ("*"   . 'mc/mark-all-like-this)))

(require 'auto-highlight-symbol)
(global-auto-highlight-symbol-mode t)

(require 'highlight-symbol)
(setq highlight-symbol-colors '("DarkOrange" "DodgerBlue1" "DeepPink1"))
(global-set-key (kbd "<f3>") 'highlight-symbol-at-point)
(global-set-key (kbd "M-<f3>") 'highlight-symbol-remove-all)

(global-anzu-mode +1)
(custom-set-variables
 '(anzu-mode-lighter "")
 '(anzu-deactivate-region t)
 '(anzu-search-threshold 500))
(global-set-key (kbd "M-%") 'anzu-query-replace)
(global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp)


;; magit
(setq magit-auto-revert-mode -1)
(global-set-key "\C-cs" 'magit-status)
(global-set-key "\C-cl" 'magit-log)
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)


;;; ddskk
(setq default-input-method 'japanese-skk
      skk-japanese-message-and-error nil
      skk-show-japanese-menu nil
      skk-show-annotation nil
      skk-status-indicator 'left)
;; indicator
(setq skk-latin-mode-string "[_A]"
      skk-hiragana-mode-string "[あ]"
      skk-katakana-mode-string "[ア]"
      skk-jisx0208-latin-mode-string "[Ａ]"
      skk-jisx0201-mode-string "[_ｱ]"
      skk-indicator-use-cursor-color nil
      skk-show-inline 'vertical)
(setq skk-egg-like-newline t
      skk-auto-insert-paren t)
(when skk-show-inline
  (if (boundp 'skk-inline-show-face)
      (setq
       skk-inline-show-background-color "#2c2c88")))
;; completion
(setq skk-dcomp-activate t
      skk-dcomp-multiple-activate t
      skk-dcomp-multiple-rows 10)

(setq confirm-kill-emacs 'yes-or-no-p)
(setq recentf-max-menu-items 200)
(setq completion-ignore-case t)
(global-auto-revert-mode 1)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq scroll-conservatively 35
      scroll-margin 5
      scroll-step 1)


;;; indent
(setq-default tab-width 2)
(setq-default default-tab-width 2)
(setq-default indent-tabs-mode nil)
(defun electric-indent-ignore-js2 (char)
  "Ignore electric indentation for js2-mode"
  (if (equal major-mode 'js2-mode)
      'no-indent
    nil))
(add-hook 'electric-indent-functions 'electric-indent-ignore-js2)

(custom-set-variables
 '(read-file-name-completion-ignore-case t))

;; parens
(electric-pair-mode 1)
(show-paren-mode)

;; enable some commands
(put 'upcase-region 'disabled nil)

;; create backup file in .emacs.d/backups
(setq-default delete-old-versions t)
(setq make-backup-files t)
(setq backup-directory-alist
      (cons (cons "\\.*$" (expand-file-name "~/.emacs.d/backups"))
            backup-directory-alist))

;; load $PATH from shell
(defun set-exec-path-from-shell-PATH ()
  (interactive)
  "Set up Emacs' `exec-path' and PATH environment variable to match that used by the user's shell."
  (let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))
(unless (eq system-type 'windows-nt) (set-exec-path-from-shell-PATH))


;; c.f. http://rejeep.github.io/emacs/elisp/2010/03/26/rename-file-and-buffer-in-emacs.html
(defun rename-this-buffer-and-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (cond ((get-buffer new-name)
               (error "A buffer named '%s' already exists!" new-name))
              (t
               (rename-file filename new-name 1)
               (rename-buffer new-name)
               (set-visited-file-name new-name)
               (set-buffer-modified-p nil)
               (message "File '%s' successfully renamed to '%s'" name (file-name-nondirectory new-name))))))))
