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


(setq custom-file (expand-file-name "~/.emacs.d/custom.el"))
(load custom-file 'no-error 'no-message)



