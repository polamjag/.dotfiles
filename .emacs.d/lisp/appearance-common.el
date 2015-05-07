(provide 'appearance-common)

;; color scheme
(load-theme 'manoj-dark t)
(menu-bar-mode -1)
(setq inhibit-startup-message t)
(setq visible-bell t)
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
(setq initial-frame-alist default-frame-alist)
;; popwin
(setq pop-up-windows nil)
(require 'popwin)
(setq display-buffer-function 'popwin:display-buffer)
(setq popwin:popup-window-position 'bottom)
(push '("^\*helm .+\*$" :regexp t) popwin:special-display-config)
