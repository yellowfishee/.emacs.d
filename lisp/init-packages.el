(setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
			 ("nongnu" . "http://mirrors.ustc.edu.cn/elpa/nongnu/")))
(package-initialize) ;; You might already have this line

(setq package-check-signature nil) ;; 个别时候出现签名校验失败

(require 'package)
(require 'use-package)

;; init package manager
(unless (bound-and-true-p package--initialized)
  (package-initialize)
 
(package-initialize)
 
;; refresh software origin
(unless package-archive-contents)
  (package-refresh-contents))
 
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
 
(setq use-package-always-ensure t)
(setq use-package-always-defer t)
(setq use-package-always-demand nil)
(setq use-package-expand-minimally t)
(setq use-package-verbose t)

 
(provide 'init-packages)
