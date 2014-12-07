(provide 'eshell-aliases)

(dolist (l '(
						 ("ls" "ls -CFa")
						 ("l" "ls -CFal")
						 ))
	(add-to-list 'eshell-command-aliases-list l)
	)
