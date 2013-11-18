;; init.el: initial configurator for emacs
;; polamjag <indirectgeeks@gmail.com>
;;
;; a table of contents
;;   +- common load-path
;;   +- Marmalade snippets
;;   +- commands
;;   +- modes
;;   +- set color scheme
;;   +- window and apperance preferences
;;   +- enable ibus-mozc as emacs-mozc
;;   +- keybindings
;;   `- miscellaneous preferences


;; ================
;; common load-path
;; ================
(add-to-list 'load-path "~/.emacs.d/")


;; ==================
;; Marmalade snippets
;; ==================
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

(define-obsolete-variable-alias 'last-command-char 'last-command-event "at least 19.34") 


;; ========
;; commands
;; ========
;;(add-to-list 'load-path (expand-file-name "~/.emacs.d/auto-install/"))
;;(require 'auto-install)
;;(auto-install-update-emacswiki-package-name t)
;;(auto-install-compatibility-setup)


;; =====
;; modes
;; =====
;; php-mode
(autoload 'php-mode "~/.emacs.d/php-mode.el"
   "Editing PHP scripts" t)
(add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))
;; wc-mode
(require 'wc-mode)
;; ocaml-mode
(setq auto-mode-alist
      (cons '("\\.ml[iylp]?\$" . caml-mode) auto-mode-alist))
(autoload 'caml-mode "caml" "Major mode for editing Caml code." t)
(autoload 'run-caml "inf-caml" "Run an inferior Caml process." t)
(if window-system (require 'caml-font))
(setq inferior-caml-program "/usr/local/bin/ocaml")
;; startup/setup anything.el
;;(require 'anything-startup)
;; flymake for java 
(require 'flymake)
(add-hook 'java-mode-hook 'flymake-mode-on)
(defun my-java-flymake-init ()
  (list "javac" (list (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-with-folder-structure))))
(add-to-list 'flymake-allowed-file-name-masks '("\\.java$" my-java-flymake-init flymake-simple-cleanup))


;; ================
;; set color scheme
;; ================
(deftheme molokai
  "Molokai color theme")
(custom-theme-set-faces
 'molokai
 ;; Backgroud, charactors, cursor
 '(cursor ((t (:foreground "#F8F8F0"))))
 '(default ((t (:background "#1B1D1E" :foreground "#F8F8F2"))))
 ;; Selected regions
 '(region ((t (:background "#403D3D"))))
 ;; Mode line
 '(mode-line ((t (:foreground "#F8F8F2" :background "#000000"
                  :box (:line-width 1 :color "#000000" :style released-button)))))
 '(mode-line-buffer-id ((t (:foreground nil :background nil))))
 '(mode-line-inactive ((t (:foreground "#BCBCBC" :background "#333333"
                           :box (:line-width 1 :color "#333333")))))
 ;; Highlight
 '(highlight ((t (:foreground "#000000" :background "#C4BE89"))))
 '(hl-line ((t (:background "#293739"))))
 ;; Names of function
 '(font-lock-function-name-face ((t (:foreground "#FFFFFF"))))
 ;; Names and content of variables
 '(font-lock-variable-name-face ((t (:foreground "#FFFFFF"))))
 '(font-lock-string-face ((t (:foreground "#E6DB74"))))
 ;; Specified keywords
 '(font-lock-keyword-face ((t (:foreground "#F92672"))))
 ;; Boolean
 '(font-lock-constant-face((t (:foreground "#AE81BC"))))
 ;; Parens
 '(show-paren-match-face ((t (:foreground "#1B1D1E" :background "#FD971F"))))
 '(paren-face ((t (:foreground "#A6E22A" :background nil))))
 ;; Comments
 '(font-lock-comment-face ((t (:foreground "#c4c1bD"))))
 ;; CSS
 '(css-selector ((t (:foreground "#66D9EF"))))
 '(css-property ((t (:foreground "#FD971F"))))
 ;; nXML-mode
 ;; Names of tag
 '(nxml-element-local-name ((t (:foreground "#F92672"))))
 ;; Attributes
 '(nxml-attribute-local-name ((t (:foreground "#66D9EF"))))
 ;; Tag delimiters
 '(nxml-tag-delimiter ((t (:foreground "#A6E22A"))))
 ;; DOCTYPE declaration
 '(nxml-markup-declaration-delimiter ((t (:foreground "#74715D"))))
 ;; dired
 '(dired-directory ((t (:foreground "#A6E22A"))))
 '(dired-symlink ((t (:foreground "#66D9EF"))))
 ;; MMM-mode
 '(mmm-default-submode-face ((t (:foreground nil :background "#000000")))))
;; font family / font face
(set-face-attribute 'default nil :family "Ricty" :height 120)
(set-fontset-font nil 'japanese-jisx0208 (font-spec :family "Ricty"))
;; set ratio of font between zenkaku and hankaku
;;(setq face-font-rescale-alist '(("Ricty" . 1.2)))
;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))
(provide-theme 'molokai)


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
;; disable welcome message
(setq inhibit-startup-message t)
;; enable visual bell (disable beep)
(setq visible-bell t)
;; set opacity of editor window
(set-frame-parameter nil 'alpha 90)
;; display line number
(setq linum-delay t)
(defadvice linum-schedule (around my-linum-schedule () activate)
  (run-with-idle-timer 0.2 nil #'linum-update-current))
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
;; popwin: improve behaviour of popup buffer
(setq pop-up-windows nil)
(require 'popwin nil t)
(when (require 'popwin nil t)
  (setq anything-samewindow nil)
  (setq display-buffer-function 'popwin:display-buffer)
  (push '("anything" :regexp t :height 0.5) popwin:special-display-config)
  (push '("*Completions*" :height 0.4) popwin:special-display-config)
  (push '("*compilation*" :height 0.4 :noselect t :stick t) popwin:special-display-config)
  )
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
    :foreground "gray90")
(set-face-attribute 'mode-line-filename-face nil
    :inherit 'mode-line-face
    :foreground "#eab700"
    :weight 'bold)
(set-face-attribute 'mode-line-position-face nil
    :inherit 'mode-line-face
    :family "Menlo")
(set-face-attribute 'mode-line-mode-face nil
    :inherit 'mode-line-face
    :foreground "white")
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

;;(auto-complete-mode)
(require 'auto-complete)
(global-auto-complete-mode t)

 (add-to-list 'load-path "~/.emacs.d/auto-java-complete/")
 (require 'ajc-java-complete-config)
 (add-hook 'java-mode-hook 'ajc-java-complete-mode)
 (add-hook 'find-file-hook 'ajc-4-jsp-find-file-hook)
 


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["#2e3436" "#a40000" "#4e9a06" "#c4a000" "#204a87" "#5c3566" "#729fcf" "#eeeeec"])
 '(custom-enabled-themes (quote (wheatgrass))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

