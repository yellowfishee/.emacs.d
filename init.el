;;startupemacsconfiguration
(use-package benchmark-init
  :ensure t
  :demand t
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))
;;loadconfig.orgtoconfig.el
(require 'ob-tangle)
(org-babel-load-file
    (expand-file-name "config.org" user-emacs-directory))

;; 输入法实在是太占地方了，使用这个方法来加载输入法
(add-to-list 'load-path "~/.emacs.d/lisp/")
;; (add-to-list 'load-path "~/.emacs.d/github")
(require 'init-chinese-flypy)

(use-package centered-cursor-mode
  :demand
  :config
  ;; Optional, enables centered-cursor-mode in all buffers.
  (global-centered-cursor-mode))

(add-hook 'org-mode-hook 'hl-line-mode)
;; 在org mode中自动换行
(add-hook 'org-mode-hook 'toggle-truncate-lines)


;; (modify-all-frames-parameters
;;  '((right-divider-width . 40)
;;    (internal-border-width . 40)))
;; (dolist (face '(window-divider
;;                 window-divider-first-pixel
;;                 window-divider-last-pixel))
;;   (face-spec-reset-face face)
;;   (set-face-foreground face (face-attribute 'default :background)))
;; (set-face-background 'fringe (face-attribute 'default :background))

 (use-package ox-hugo
   :disabled t
   :ensure t   ;Auto-install the package from Melpa
   :pin melpa  ;`package-archives' should already have ("melpa" . "https://melpa.org/packages/")
   :init
    (setq org-hugo-base-dir "~/blog") ;; 本地网站根目录
    (setq org-hugo-default-section-directory "post")
   :after ox)

(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"                 ;; personal snippets
        ))

(add-to-list 'load-path "~/.emacs.d/github/org-zettel-ref-mode")
(add-to-list 'load-path "~/.emacs.d/github/")
(setq org-zettel-ref-mode-type 'denote)
(setq org-zettel-ref-quick-markup-key "C-c m")
(setq org-zettel-ref-overview-directory "~/denotes/overviews")
(setq org-zettel-ref-reference-folder "~/denotes/ref/")
(setq org-zettel-ref-temp-folder "~/denotes/temp/")

(require 'org-zettel-ref-mode)
(server-mode 1)

