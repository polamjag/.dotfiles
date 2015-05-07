(provide 'keybinds)

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
  (when (or (eq window-system 'x) (eq window-system 'ns))
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
(defun other-window-or-split (val)
  (interactive)
  (when (one-window-p)
    (split-window-vertically))
  (other-window val))
(global-set-key (kbd "C-<tab>") (lambda () (interactive) (other-window-or-split 1)))
(global-set-key (kbd "C-S-<tab>") (lambda () (interactive) (other-window-or-split -1)))
(defun copy-buffer ()
  "Copy entire buffer to clipboard"
  (interactive)
  (progn
    (clipboard-kill-ring-save (point-min) (point-max))
    (message "Copied entire buffer to clipboard")))
(global-set-key (kbd "C-M-y") 'copy-buffer)
;; swap Cmd with Option on Mac
(setq ns-command-modifier (quote meta))
(setq ns-alternate-modifier (quote super))
