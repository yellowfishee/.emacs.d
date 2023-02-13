;; 也许是有bug，也许是我没看出来，反正就是不行
;; (add-to-list 'load-path "~/.emacs.d/site-lisp/magit/")

(require 'magit)
(require 'magit-section)

(magit-add-section-hook 'magit-status-sections-hook
			'magit-insert-modules
			'magit-insert-stashes
			'append)
(provide 'init-magit)
