;;; helm-ag-r-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (helm-ag-r-from-git-repo helm-ag-r-current-file
;;;;;;  helm-ag-r helm-ag-r-git-logs helm-ag-r-shell-history) "helm-ag-r"
;;;;;;  "helm-ag-r.el" (21139 64533 731373 27000))
;;; Generated autoloads from helm-ag-r.el

(autoload 'helm-ag-r-shell-history "helm-ag-r" "\
Search shell history(I don't make sure without zsh).

\(fn)" t nil)

(autoload 'helm-ag-r-git-logs "helm-ag-r" "\
Search git's commit log.
This function use OPTIONS to git log command if you are specified

\(fn &optional OPTIONS)" t nil)

(autoload 'helm-ag-r "helm-ag-r" "\
The helm-ag-r find something by ag program.
Default is `default-directory variable
 (i.e. current directory).  the FILE-OR-DIRECTORY is passed to ag's [PATH].
If you set the SOURCE argument, override helm-ag-r-source variable by
 your specified source.(but not delete original source)

\(fn &optional FILE-OR-DIRECTORY SOURCE BUFFER)" t nil)

(autoload 'helm-ag-r-current-file "helm-ag-r" "\
Search from current-file.

\(fn)" t nil)

(autoload 'helm-ag-r-from-git-repo "helm-ag-r" "\
Search from git repository.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("helm-ag-r-pkg.el") (21139 64533 745234
;;;;;;  121000))

;;;***

(provide 'helm-ag-r-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; helm-ag-r-autoloads.el ends here
