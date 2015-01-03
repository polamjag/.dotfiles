;;;; common load-path
(add-to-list 'load-path "~/.emacs.d/lisp/")


;;;; Marmalade and auto-install snippets
(require 'package)
(setq package-archives
      (append
       '(("melpa" . "http://melpa.milkbox.net/packages/")
         ("marmalade" . "http://marmalade-repo.org/packages/")
         ("ELPA" . "http://tromey.com/elpa/"))
       package-archives))
(package-initialize)
(require 'packages-list)


;;;; modes initialization
(require 'flymake)
;; flymake for java 
(add-hook 'java-mode-hook 'flymake-mode-on)
(defun my-java-flymake-init ()
  (list "javac" (list (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-with-folder-structure))))
(add-to-list 'flymake-allowed-file-name-masks '("\\.java$" my-java-flymake-init flymake-simple-cleanup))
;;; ruby
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))
(add-hook
 'ruby-mode-hook
 '(lambda ()
    (if (not (null buffer-file-name)) (flymake-mode))))
(require 'ruby-end)
(add-hook 'ruby-mode-hook
  '(lambda ()
    (abbrev-mode 1)
    (electric-indent-mode t)
    (electric-layout-mode t)))
(require 'ruby-block)
(ruby-block-mode t)
(setq ruby-block-highlight-toggle t)
;; flymake for ruby
(defun flymake-ruby-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list "ruby" (list "-c" local-file))))
(push '(".+\\.rb$" flymake-ruby-init) flymake-allowed-file-name-masks)
(push '("Rakefile$" flymake-ruby-init) flymake-allowed-file-name-masks)
(push '("^\\(.*\\):\\([0-9]+\\): \\(.*\\)$" 1 2 nil 3) flymake-err-line-patterns)
;; web-mode
(add-to-list 'auto-mode-alist '("\\.html?$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(defun my-web-mode-hook ()
  "Hooks for web-mode"
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-script-offset 2)
  (setq web-mode-php-offset 2)
  (setq web-mode-html-offset 2)
  (setq web-mode-style-padding 2)
  (setq web-mode-script-padding 2)
  (setq indent-tabs-mode nil)
  (setq tab-width 2))
(add-hook 'web-mode-hook  'my-web-mode-hook)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(defun credmp/flymake-display-err-minibuf () 
  "Displays the error/warning for the current line in the minibuffer"
  (interactive)
  (let* ((line-no             (flymake-current-line-no))
         (line-err-info-list  (nth 0 (flymake-find-err-info flymake-err-info line-no)))
         (count               (length line-err-info-list))
         )
    (while (> count 0)
      (when line-err-info-list
        (let* ((file       (flymake-ler-file (nth (1- count) line-err-info-list)))
               (full-file  (flymake-ler-full-file (nth (1- count) line-err-info-list)))
               (text (flymake-ler-text (nth (1- count) line-err-info-list)))
               (line       (flymake-ler-line (nth (1- count) line-err-info-list))))
          (message "[%s] %s" line text)
          )
        )
      (setq count (1- count)))))


;;;; helm configs
(require 'helm-config)
(helm-mode t)
(global-set-key (kbd "C-c z") 'helm-resume)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key "\C-c\C-k" 'helm-show-kill-ring)
(define-key isearch-mode-map (kbd "C-o") 'helm-swoop-from-isearch)


;;;; auto-complete
(global-auto-complete-mode 1)
(ac-config-default)
(setq ac-use-menu-map t)
(global-set-key "\C-cc" 'auto-complete-mode)


;;;; extending dired
(require 'dired+)
(define-key dired-mode-map (kbd "(") 'dired-hide-details-mode)
(define-key dired-mode-map (kbd ")") 'dired-hide-details-mode)


;;;; set color scheme
(load-theme 'manoj-dark t)
(defvar colorscheme-mode-status "dark")
(defun toggle-colorscheme ()
  "Toggle colorscheme dark or day"
  (interactive)
  (if (string= colorscheme-mode-status "dark")
      (progn
        (load-theme 'adwaita t)
        (setq colorscheme-mode-status "light"))
    (progn (load-theme 'manoj-dark t)
           (setq colorscheme-mode-status "dark"))))
(global-set-key [f9] 'toggle-colorscheme)


;;;; window and appearance preferences
;; font
(defun set-font (font-name size)
  (set-face-attribute 'default nil :family font-name :height size)
  (set-fontset-font nil 'japanese-jisx0208 (font-spec :family font-name))
  (setq face-font-rescale-alist '((font-name . 1.0))))
(if (eq system-type 'gnu/linux) (set-font "Ricty" 105))
(if (eq system-type 'darwin) (set-font "Monaco" 105))
(if (eq system-type 'windows-nt) (set-font "Consolas" 95))
;; user interface
(cond
 ((eq window-system 'x)
  (set-scroll-bar-mode t)
  (tool-bar-mode -1)
  ))
(if (eq system-type 'windows-nt) (tool-bar-mode -1))
(menu-bar-mode -1)
;; disable welcome message
(setq inhibit-startup-message t)
;; enable visual bell (disable beep)
(setq visible-bell t)
;; set opacity of editor window
(set-frame-parameter nil 'alpha 90)
;;
(show-paren-mode)
;; rainbow-delimiters-mode
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
;; make rainbow-delimiters-mode more vivid
(require 'cl-lib)
(require 'color)
(cl-loop
 for index from 1 to rainbow-delimiters-max-face-count do
 (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
   (cl-callf color-saturate-name (face-foreground face) 45)))
;; highlight current line
(defface hlline-face
  '((((class color)
      (background dark))
     (:background "dark slate gray"))
    (((class color)
      (background light))
     (:background  "#98FB98"))
    (t
     ()))
  "*Face used by hl-line.")
(setq hl-line-face 'hlline-face)
(global-hl-line-mode)
;; popwin
(setq pop-up-windows nil)
(require 'popwin)
(setq display-buffer-function 'popwin:display-buffer)
(setq popwin:popup-window-position 'bottom)
(push '("^\*helm .+\*$" :regexp t) popwin:special-display-config)
;;
(if (boundp 'window-system)
  (setq default-frame-alist
    (append (list
             '(top . 60)
             '(left . 140)
             '(width . 80)
             '(height . 35)
             )
            default-frame-alist)))
(setq initial-frame-alist default-frame-alist)
;; Mode line setup
(setq-default
 mode-line-position
 '(
   " "
   ;; Position, including warning for 80 columns
   (:propertize "%4l" face mode-line-position-face)
   (:propertize "/" face mode-line-delim-face-1)
   (:eval
    (number-to-string (count-lines (point-min) (point-max))))
   " "
   (:eval (propertize "%3c" 'face
                      (if (>= (current-column) 80)
                          'mode-line-80col-face
                        'mode-line-position-face)))
   " "
   ))

(setq-default
 mode-line-format
 '("%e"
   mode-line-mule-info
   ;; emacsclient [default -- keep?]
   mode-line-client
   mode-line-remote
   ;evil-mode-line-tag
   mode-line-position
   ; read-only or modified status
   (:eval
    (cond (buffer-read-only
           (propertize " RO " 'face 'mode-line-read-only-face))
          ((buffer-modified-p)
           (propertize " ** " 'face 'mode-line-modified-face))
          (t "  ")))
   " "
   ;; directory and buffer/file name
   (:propertize (:eval (shorten-directory default-directory 30))
                face mode-line-folder-face)
   (:propertize "%b" face mode-line-filename-face)
   ;; narrow [default -- keep?]
   " %n"
   ;; mode indicators: vc, recursive edit, major mode, minor modes, process, global
   (vc-mode vc-mode)
   "  %["
   (:propertize mode-name
                face mode-line-mode-face)
   "%]"
   (:eval (propertize (format-mode-line minor-mode-alist)
                      'face 'mode-line-minor-mode-face))
   "  "
   (:propertize mode-line-process
                face mode-line-process-face)
   "  "
   (global-mode-string global-mode-string)
   ;; "  "
   ;; nyan-mode uses nyan cat as an alternative to %p
   ;; (:eval (when nyan-mode (list (nyan-create))))
   ))
;; Helper function
(defun shorten-directory (dir max-length)
  "Show up to `max-length' characters of a directory name `dir'."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
        (output ""))
    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    (while (and path (< (length output) (- max-length 4)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when path
      (setq output (concat ".../" output)))
    output))
(set-face-attribute 'mode-line nil
                    :foreground "gray80" :background "gray10"
                    :inverse-video nil
                    :weight 'normal
                    :height 120
                    :box '(:line-width 2 :color "gray10" :style nil))
(set-face-attribute 'mode-line-inactive nil
                    :foreground "gray80" :background "gray30"
                    :inverse-video nil
                    :weight 'extra-light
                    :height 120
                    :box '(:line-width 2 :color "gray30" :style nil))
;; Extra mode line faces
(make-face 'mode-line-read-only-face)
(make-face 'mode-line-modified-face)
(make-face 'mode-line-folder-face)
(make-face 'mode-line-filename-face)
(make-face 'mode-line-position-face)
(make-face 'mode-line-mode-face)
(make-face 'mode-line-minor-mode-face)
(make-face 'mode-line-process-face)
(make-face 'mode-line-80col-face)
(make-face 'mode-line-delim-face-1)
(set-face-attribute 'mode-line-read-only-face nil
                    :inherit 'mode-line-face
                    :foreground "#4271ae"
                    :box '(:line-width 2 :color "#4271ae"))
(set-face-attribute 'mode-line-modified-face nil
                    :inherit 'mode-line-face
                    :foreground "#c82829"
                    :background "#ffffff"
                    :box '(:line-width 2 :color "#c82829"))
(set-face-attribute 'mode-line-folder-face nil
                    :inherit 'mode-line-face
                    :weight 'extra-light
                    :height 110
                    :foreground "#888888")
(set-face-attribute 'mode-line-filename-face nil
                    :inherit 'mode-line-face
                    :foreground "#eab700"
                    :weight 'bold)
(set-face-attribute 'mode-line-position-face nil
                    :inherit 'mode-line-face
                    :family "Menlo")
(set-face-attribute 'mode-line-mode-face nil
                    :inherit 'mode-line-face
                    :foreground "#cccccc")
(set-face-attribute 'mode-line-minor-mode-face nil
                    :inherit 'mode-line-mode-face
                    :foreground "gray60"
                    :height 100)
(set-face-attribute 'mode-line-process-face nil
                    :inherit 'mode-line-face
                    :foreground "#718c00")
(set-face-attribute 'mode-line-80col-face nil
                    :inherit 'mode-line-position-face
                    :foreground "black" :background "#eab700")
(set-face-attribute 'mode-line-delim-face-1 nil
                    :inherit 'mode-line-face
                    :foreground "white")
;; hide some minor modes
(setq my/hidden-minor-modes
      '(undo-tree-mode
        helm-mode
        ))
(mapc (lambda (mode)
        (setq minor-mode-alist
              (cons (list mode "") (assq-delete-all mode minor-mode-alist))))
      my/hidden-minor-modes)


;;;; keybindings
;; improve behavior of C-a (Home)
(defun beginning-of-indented-line (current-point)
  (interactive "d")
  (if (string-match
       "^[ \t]+$"
       (save-excursion
         (buffer-substring-no-properties
          (progn (beginning-of-line) (point))
          current-point)))
      (beginning-of-line)
    (back-to-indentation)))
(defun beginning-of-visual-indented-line (current-point)
  (interactive "d")
  (let ((vhead-pos (save-excursion (progn (beginning-of-visual-line) (point))))
        (head-pos (save-excursion (progn (beginning-of-line) (point)))))
    (cond
     ((eq vhead-pos head-pos)
      (if (string-match
           "^[ \t]+$"
           (buffer-substring-no-properties vhead-pos current-point))
          (beginning-of-visual-line)
        (back-to-indentation)))
     ((eq vhead-pos current-point)
      (backward-char)
      (beginning-of-visual-indented-line (point)))
     (t (beginning-of-visual-line)))))
(global-set-key "\C-a" 'beginning-of-visual-indented-line)
(global-set-key "\C-e" 'end-of-visual-line)
;; set C-h as Backspace and M-h as help-command
(global-set-key "\M-?" 'mark-paragraph)
(global-set-key "\C-h" 'delete-backward-char)
(global-set-key "\M-h" 'help-command)
;; newline
(global-set-key "\C-j" 'newline-and-indent)
(global-set-key "\C-m" 'newline)
;; workaround for backspace
(global-set-key [backspace] 'delete-backward-char)
;; C-x l to goto line
(global-set-key "\C-xl" 'goto-line)
;; toggle fullscreen with F11
(defun toggle-fullscreen ()
  "Toggle full screen on X11"
  (interactive)
  (when (eq window-system 'x)
    (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth))))
(global-set-key [f11] 'toggle-fullscreen)
;; disable suspention with C-z
(global-unset-key "\C-z")
(global-set-key "\C-z" 'scroll-down)
;; move for multiple lines
(global-set-key (kbd "M-p") '(lambda () (interactive) (previous-line 5)))
(global-set-key (kbd "M-n") '(lambda () (interactive) (next-line 5)))
;; open buffer list in current pane
(global-unset-key "\C-x\C-b")
;; helm
(global-set-key "\C-x\C-b" 'helm-mini)
(global-set-key "\C-xg" 'helm-ghq)
(global-set-key "\C-c\C-s" 'helm-swoop)
(global-set-key "\C-c\C-g" 'helm-git-grep)
(global-set-key "\C-xx" 'quickrun)
(global-set-key "\C-xj" 'quickrun-with-arg)
;; magit
(global-set-key "\C-cs" 'magit-status)
(global-set-key "\C-cl" 'magit-log)
;;
(defun other-window-or-split (val)
  (interactive)
  (when (one-window-p)
    (split-window-vertically))
  (other-window val))
(global-set-key (kbd "<C-tab>") (lambda () (interactive) (other-window-or-split 1)))
(global-set-key (kbd "<C-S-tab>") (lambda () (interactive) (other-window-or-split -1)))
(global-set-key (kbd "<C-iso-lefttab>") (lambda () (interactive) (other-window-or-split -1)))


;;;; miscellaneous preferences
(setq completion-ignore-case t)
(global-auto-revert-mode 1)
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq-default tab-width 2)
(setq-default default-tab-width 2)
(setq-default indent-tab-mode nil)
(custom-set-variables
 '(read-file-name-completion-ignore-case t))
(require 'flex-autopair)
(flex-autopair-mode t)
(setq flex-autopair-conditions
      `(; Insert matching pair.
        (openp . pair)
        ; Skip self.
        ((and closep
              (eq (char-after) last-command-event)) . skip)
        (closep . self)
        ))
;; create backup file in .emacs.d/backups
(setq-default delete-old-versions t)
(setq make-backup-files t)
(setq backup-directory-alist
      (cons (cons "\\.*$" (expand-file-name "~/.emacs.d/backups"))
            backup-directory-alist))
;; undo-tree
(require 'undo-tree)
(global-undo-tree-mode t)
(global-set-key (kbd "M-/") 'undo-tree-redo)
;; popwin
(require 'popwin)
(setq display-buffer-function 'popwin:display-buffer)
(require 'expand-region)
(require 'multiple-cursors)
(require 'smartrep)
(global-set-key (kbd "C-,") 'er/expand-region)
(global-set-key (kbd "C-M-,") 'er/contract-region)
(global-set-key (kbd "<C-M-return>") 'mc/edit-lines)
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
(defun set-exec-path-from-shell-PATH ()
  (interactive)
  "Set up Emacs' `exec-path' and PATH environment variable to match that used by the user's shell."
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))
(unless (eq system-type 'windows-nt) (set-exec-path-from-shell-PATH))
(global-anzu-mode +1)
(custom-set-variables
 '(anzu-mode-lighter "")
 '(anzu-deactivate-region t)
 '(anzu-search-threshold 1000))
(global-set-key (kbd "M-%") 'anzu-query-replace)
(global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp)


;;;; eshell
(global-set-key (kbd "C-=") '(lambda ()
                               (interactive)
                               (if (string= major-mode "eshell-mode") (previous-buffer) (eshell))
                               ))
(add-hook 'eshell-mode-hook
          '(lambda ()
             (progn
               (setq eshell-cmpl-ignore-case t)
               (setq eshell-ask-to-save-history (quote always))
               (setq eshell-cmpl-cycle-completions t)
               (setq eshell-hist-ignoredups t)
               (define-key eshell-mode-map "\C-a" 'eshell-bol)
               (define-key eshell-mode-map [up] 'previous-line)
               (define-key eshell-mode-map [down] 'next-line)
               (define-key eshell-mode-map "\C-p" 'eshell-previous-matching-input-from-input)
               (define-key eshell-mode-map "\C-n" 'eshell-next-matching-input-from-input)
               (define-key eshell-mode-map "\C-j" 'eshell-send-input)
               )))
;; aliases
(require 'cl)
(dolist
    (l '(("ls" "ls -CFa")
         ("l" "ls -CFal")
         ("a" "cd ../ ;")
         ("ff" "find-file")
         ))
  (add-to-list 'eshell-command-aliases-list l))
(add-to-list 'ac-modes 'eshell-mode)
(defun my-ac-eshell-mode ()
  (setq ac-sources
        '(ac-source-pcomplete
          ac-source-words-in-buffer
          ac-source-dictionary)))
(add-hook 'eshell-mode-hook
          (lambda ()
            (my-ac-eshell-mode)
            (define-key eshell-mode-map (kbd "C-i") 'auto-complete)
            (define-key eshell-mode-map (kbd "TAB") 'auto-complete)))


;;;; load local config
(if (file-readable-p "~/.emacs.d/lisp/env.el")
    (load-file "~/.emacs.d/lisp/env.el"))
