;; ;; 从melpa安装
(add-to-list 'load-path "~/.emacs.d/site-lisp/elisp-tree-sitter/core")
(add-to-list 'load-path "~/.emacs.d/site-lisp/elisp-tree-sitter/lisp")
(add-to-list 'load-path "~/.emacs.d/site-lisp/elisp-tree-sitter/langs")

(require 'tree-sitter)
(require 'tree-sitter-hl)
(require 'tree-sitter-langs)
(require 'tree-sitter-debug)
(require 'tree-sitter-query)

(add-hook 'rust-mode-hook #'tree-sitter-mode)
(add-hook 'rust-mode-hook #'tree-sitter-hl-mode)

(global-tree-sitter-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

(tree-sitter-require 'go)




(provide 'init-tree-sitter)
