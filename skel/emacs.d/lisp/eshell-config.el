(provide 'eshell-config)

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
            ac-source-dictionary))))
(eval-after-load
 "eshell-mode"
 '(lambda ()
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
     (add-to-list 'eshell-command-aliases-list l))))
(add-hook 'eshell-preoutput-filter-functions
          'ansi-color-filter-apply)
