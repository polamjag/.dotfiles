(provide 'appearance-window)

;; font helper
(defun set-font (font-name size)
  (set-face-attribute 'default nil :family font-name :height size)
  (set-fontset-font nil 'japanese-jisx0208 (font-spec :family font-name))
  (setq face-font-rescale-alist '((font-name . 1.0))))
;; set font
(cond
 ((eq system-type 'gnu/linux) (set-font "Ricty" 105))
 ((eq system-type 'darwin)
  (if (find-font (font-spec :name "Ricty"))
      (set-font "Ricty" 130)
    (set-font "Monaco" 105)))
 ((eq system-type 'windows-nt) (set-font "Consolas" 95)))

(set-scroll-bar-mode nil)
(tool-bar-mode -1)
(set-frame-parameter nil 'alpha 90)
