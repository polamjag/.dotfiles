;; init.el - polamjag <s@polamjag.info>
;;
;;   +- common load-path
;;   +- Marmalade and auto-install snippets
;;   +- modes initialization
;;   +- set color scheme
;;   +- window and apperance preferences
;;   +- enable ibus-mozc as emacs-mozc
;;   +- keybindings
;;   `- miscellaneous preferences


;; ================
;; common load-path
;; ================
(add-to-list 'load-path "~/.emacs.d/")


;; ===================================
;; Marmalade and auto-install snippets
;; ===================================
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("ELPA" . "http://tromey.com/elpa/"))
(package-initialize)

(add-to-list 'load-path (expand-file-name "~/.emacs.d/auto-install/"))
(require 'auto-install)
(auto-install-update-emacswiki-package-name t)
(auto-install-compatibility-setup)


;; ====================
;; modes initialization
;; ====================
;; flymake for java 
(require 'flymake)
(add-hook 'java-mode-hook 'flymake-mode-on)
(defun my-java-flymake-init ()
  (list "javac" (list (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-with-folder-structure))))
(add-to-list 'flymake-allowed-file-name-masks '("\\.java$" my-java-flymake-init flymake-simple-cleanup))
;; flymake for ruby
;; Invoke ruby with '-c' to get syntax checking
(defun flymake-ruby-init ()
  (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
         (local-file  (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name))))
    (list "ruby" (list "-c" local-file))))
(push '(".+\\.rb$" flymake-ruby-init) flymake-allowed-file-name-masks)
(push '("Rakefile$" flymake-ruby-init) flymake-allowed-file-name-masks)
(push '("^\\(.*\\):\\([0-9]+\\): \\(.*\\)$" 1 2 nil 3) flymake-err-line-patterns)
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

;; ================
;; set color scheme
;; ================
(load-theme 'manoj-dark t)
(set-face-attribute 'default nil :family "Ricty" :height 120)
(set-fontset-font nil 'japanese-jisx0208 (font-spec :family "Ricty"))
;; set ratio of font between zenkaku and hankaku
(setq face-font-rescale-alist '(("Ricty" . 1.0)))
(defvar colorscheme-mode-status "dark")
(defun toggle-colorscheme ()
  "Toggle colorscheme dark or day"
	(interactive)
  (if (string= colorscheme-mode-status "dark")
			(progn
				(load-theme 'adwaita t)
				(setq colorscheme-mode-status "light"))
		(progn (load-theme 'manoj-dark t)
					 (setq colorscheme-mode-status "dark"))
		)
	)
(global-set-key [f9] 'toggle-colorscheme)



;; =================================
;; window and appearance preferences
;; =================================
;; disable toolbar (buttons on top)
(cond
 ((eq window-system 'x)
  ;; when running on X 
  (set-scroll-bar-mode t) ;; enable X scroll bar
  (tool-bar-mode -1)      ;; disable tool bar
  ))
;; disable menubar
(menu-bar-mode -1)
;; disable welcome message
(setq inhibit-startup-message t)
;; enable visual bell (disable beep)
(setq visible-bell t)
;; set opacity of editor window
(set-frame-parameter nil 'alpha 90)
;; rainbow-delimiters-mode
(require 'rainbow-delimiters)
(global-rainbow-delimiters-mode t)
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
(setq initial-frame-alist default-frame-alist )
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



;; ======================================================
;; enable ibus-mozc as emacs-mozc (Japanese Input Method)
;; ======================================================
(load-file "~/.emacs.d/mozc.el")
(setq default-input-method "japanese-mozc")
(setq mozc-candidate-style 'overlay)
(put 'upcase-region 'disabled nil)


;; ===========
;; keybindings
;; ===========
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
;; set C-h as Backspace and C-? as help-command, and modify M-?, M-h
(global-set-key (kbd "C-?") 'help-command)
(global-set-key (kbd "M-?") 'mark-paragraph)
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "M-h") 'backward-kill-word)
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
;; open buffer list in current pane
(global-unset-key "\C-x\C-b")
(global-set-key "\C-x\C-b" 'buffer-menu)


;; =========================
;; miscellaneous preferences
;; =========================
(setq completion-ignore-case t)
(global-auto-revert-mode 1)
(require 'uniquify)
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq-default tab-width 2)
(setq default-tab-width 2)
(custom-set-variables
 '(read-file-name-completion-ignore-case t))
(require 'flex-autopair)
(flex-autopair-mode t)
(setq flex-autopair-conditions
      `(;; Insert matching pair.
        (openp . pair)
        ;; Skip self.
        ((and closep
              (eq (char-after) last-command-event)) . skip)
        (closep . self)
        ))
;; undo-tree
(require 'undo-tree)
(global-undo-tree-mode t)
(global-set-key (kbd "M-/") 'undo-tree-redo)
;; auto-complete-mode
(require 'auto-complete)
(global-auto-complete-mode t)
;; auto-complete java
(add-to-list 'load-path "~/.emacs.d/auto-java-complete/")
(require 'ajc-java-complete-config)
(add-hook 'java-mode-hook 'ajc-java-complete-mode)
(add-hook 'find-file-hook 'ajc-4-jsp-find-file-hook)

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


;; ============
;; helm configs
;; ============
(require 'helm-config)
(global-set-key (kbd "C-c h") 'helm-mini)
(helm-mode 1)
(global-set-key (kbd "C-c z") 'helm-resume)
(define-key ac-complete-mode-map (kbd "C-:") 'ac-complete-with-helm)
(global-set-key (kbd "C-c j") 'helm-M-x)


