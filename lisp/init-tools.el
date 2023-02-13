(add-to-list 'load-path (expand-file-name "~/.emacs.d/awesome-tab"))

(require 'awesome-tab)

(awesome-tab-mode t)

(defun awesome-tab-buffer-groups ()
"`awesome-tab-buffer-groups' control buffers' group rules.
Group awesome-tab with mode if buffer is derived from `eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
All buffer name start with * will group to \"Emacs\".
Other buffer group by `awesome-tab-get-group-name' with project name."
(list
(cond
    ((or (string-equal "*" (substring (buffer-name) 0 1))
	(memq major-mode '(magit-process-mode
			    magit-status-mode
			    magit-diff-mode
			    magit-log-mode
			    magit-file-mode
			    magit-blob-mode
			    magit-blame-mode)))
    "Emacs")
    ((derived-mode-p 'eshell-mode)
    "EShell")
    ((derived-mode-p 'dired-mode)
    "Dired")
    ((memq major-mode '(org-mode org-agenda-mode diary-mode))
    "OrgMode")
    ((derived-mode-p 'eaf-mode)
    "EAF")
    (t
     (awesome-tab-get-group-name (current-buffer))))))


(use-package pyim)

(defun eh-orderless-regexp (orig_func component)
    (let ((result (funcall orig_func component)))
      (pyim-cregexp-build result)))


  (defun toggle-chinese-search ()
    (interactive)
    (if (not (advice-member-p #'eh-orderless-regexp 'orderless-regexp))
        (advice-add 'orderless-regexp :around #'eh-orderless-regexp)
      (advice-remove 'orderless-regexp #'eh-orderless-regexp)))

  (defun disable-py-search (&optional args)
    (if (advice-member-p #'eh-orderless-regexp 'orderless-regexp)
        (advice-remove 'orderless-regexp #'eh-orderless-regexp)))

  ;; (advice-add 'exit-minibuffer :after #'disable-py-search)
(add-hook 'minibuffer-exit-hook 'disable-py-search)

;;modeline上显示我的所有的按键和执行的命令
(use-package keycast)
(add-to-list 'global-mode-string '("" keycast-mode-line))
;; (keycast-mode t)


;; 工具
(use-package restart-emacs)

(use-package org-download)
;;; org-download
(require 'org-download)
(setq org-download-screenshot-method "d:/Snipaste/Snipaste.exe snip")
; (setq org-download-method 'directory)
(setq-default org-download-heading-lvl nil)
(setq-default org-download-image-dir "./images")
(defun dummy-org-download-annotate-function (link)
  "")

;; (use-package org-download)
;; (require org-download)
;; (setq org-download-screenshot-method "d:/Snipaste/Snipaste.exe snip")
;; (setq-default org-download-image-dir "./images")

;; (use-package org-download
;;     :bind ("C-S-y" . org-download-clipboard)
;;     :config
;;     (defun org-download-clipboard ()
;;       "Save the captured image from clipboard to file, and insert into buffer. Or org-download-yank."
;;       (interactive)
;;       (let ((link "http://images2015.cnblogs.com/blog/717724/201703/717724-20170318145647104-177813066.jpg"))
;;         (if (eq 0 (shell-command (format "\"c:/Program\sFiles/ImageMagick/convert.exe\" clipboard:myimage %s" link) "*screenshot2file*" "*screenshot2file*"))
;;             (org-download-image link)
;;           (org-download-yank)))))


(setq org-download-annotate-function
      #'dummy-org-download-annotate-function)



(provide 'init-tools)
