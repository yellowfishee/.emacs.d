(use-package company
  :bind (:map company-active-map
	      ("C-n" . 'company-select-next)
	      ("C-p" . 'company-select-previous))
  :init
  (global-company-mode t)
  :config
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0))




(setq tab-always-indent 'complete)



(use-package vertico
	      :ensure t)
(vertico-mode t)

(use-package orderless
  :ensure t)
(setq completion-styles '(orderless))

(use-package marginalia :ensure t)
(marginalia-mode t)

(use-package embark :ensure t)

;; vim 
(use-package evil :ensure t)
(evil-mode 1)


(setq prefix-help-command 'embark-prefix-help-command)

(use-package consult)

(add-to-list 'process-coding-system-alist '("es" gbk . gbk))
(setq consult-locate-args (encode-coding-string "es.exe -i -p -r" 'gbk))


(eval-after-load 'consult
  (progn
      (setq 
        consult-narrow-key "<"
        consult-line-numbers-widen t
        consult-async-min-input 2
        consult-async-refresh-delay  0.15
        consult-async-input-throttle 0.2
        consult-async-input-debounce 0.1)
    ))


(use-package embark-consult)
(use-package wgrep)
(setq wgrep-auto-save-buffer t)

(eval-after-load 'consult
  '(eval-after-load 'embark
     '(progn
	(require 'embark-consult)
	(add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode))))

;; 文件管理
(use-package dirvish)

;; (use-package dashboard)

(provide 'init-completion)
