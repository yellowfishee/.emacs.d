;;startupemacsconfiguration

;;loadconfig.orgtoconfig.el
(require 'ob-tangle)
(org-babel-load-file 
    (expand-file-name "config.org" user-emacs-directory))

;; 输入法实在是太占地方了，使用这个方法来加载输入法
(add-to-list 'load-path "~/.emacs.d/lisp/")
;; (add-to-list 'load-path "~/.emacs.d/github")
(require 'init-chinese-flypy)

(server-mode 1)
