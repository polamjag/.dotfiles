(provide 'modeline-light)


;; hide some minor modes
(setq my/hidden-minor-modes
      '(undo-tree-mode
        helm-mode
        highlight-symbol-mode
        smartparens-mode
        eldoc-mode
        ))
(mapc (lambda (mode)
        (setq minor-mode-alist
              (cons (list mode "") (assq-delete-all mode minor-mode-alist))))
      my/hidden-minor-modes)
