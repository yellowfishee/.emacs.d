;; 拼音和ivy posframe都在这个文件
(use-package posframe)
(use-package ivy-posframe)

(ivy-mode 0)

(use-package ivy-posframe
  :init
  (setq ivy-posframe-display-functions-alist
	'((swiper . ivy-posframe-display-at-frame-center)
	  (complete-symbol . ivy-posframe-display-at-point)
	  (counsel-M-x . ivy-posframe-display-at-frame-center)
	  (counsel-find-file . ivy-posframe-display-at-frame-center)
	  (ivy-switch-buffer . ivy-posframe-display-at-frame-center)
	  (t . ivy-posframe-display-at-frame-center)
	  (counsel-ripgrep . ivy-posframe-display-at-XXX))))


(ivy-posframe-mode 1)


(provide 'init-posframe)
