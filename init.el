;; -*- lexical-binding: t -*-

(add-to-list 'load-path "~/.emacs.d/lisp/")

(require 'init-basic)
(require 'init-funcs)
(require 'init-packages)
(require 'init-completion)
(require 'init-tools)
(require 'init-ui)
(require 'init-keybindings)
(require 'init-org)
(require 'init-eaf)
(require 'init-dashboard)
(require 'init-posframe)
(require 'init-pyim)
(require 'init-lsp)
(require 'init-tree-sitter)

(setq custom-file (expand-file-name "~/.emacs.d/custom.el"))
(load custom-file 'no-error 'no-message)



(put 'dired-find-alternate-file 'disabled nil)
