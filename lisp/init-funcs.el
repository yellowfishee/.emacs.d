;;REPL read eval print loop

;;  Happy Hacking

;; 快速打开配置文件
(defun open-init-file()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun embark-export-write ()
  "Export the current vertico results to a writable buffer if possible.
Supports exporting consult-grep to wgrep, file to wdeired, and consult-location to occur-edit"
  (interactive)
  (require 'embark)
  (require 'wgrep)
  (pcase-let ((`(,type . ,candidates)
               (run-hook-with-args-until-success 'embark-candidate-collectors)))
    (pcase type
      ('consult-grep (let ((embark-after-export-hook #'wgrep-change-to-wgrep-mode))
                       (embark-export)))
      ('file (let ((embark-after-export-hook #'wdired-change-to-wdired-mode))
               (embark-export)))
      ('consult-location (let ((embark-after-export-hook #'occur-edit-mode))
                           (embark-export)))
      (x (user-error "embark category %S doesn't support writable export" x)))))


(defun consult-directory-externally (file)
  "Open FILE externally using the default application of the system."
  (interactive "fOpen externally: ")
  (if (and (eq system-type 'windows-nt)
           (fboundp 'w32-shell-execute))
      (shell-command-to-string (encode-coding-string (replace-regexp-in-string "/" "\\\\"
            (format "explorer.exe %s" (file-name-directory (expand-file-name file)))) 'gbk))
    (call-process (pcase system-type
                    ('darwin "open")
                    ('cygwin "cygstart")
                    (_ "xdg-open"))
                  nil 0 nil
                  (file-name-directory (expand-file-name file)))))




;;打开当前文件的目录
(defun my-open-current-directory ()
  (interactive)
  (consult-directory-externally default-directory))


;; 注释选中行
(defun my-comment-or-uncomment-region (beg end &optional arg)  
  (interactive (if (use-region-p)  
                   (list (region-beginning) (region-end) nil)  
                 (list (line-beginning-position)  
                       (line-beginning-position 2))))  
  (comment-or-uncomment-region beg end arg)  
)  


;; ;; 开启代理
;; (defun +yefi/toggle-proxy()
;;   (interactive)
;;   (if (null url-proxy-services)
;;       (progn
;; 	(set q url-proxy-services
;; 	     '(("http", "127.0.0.1:7890")
;; 	       ("https", "127.0.0.1:7890")))
;; 	(message "proxy started"))
;;     (setq url-proxy-services nil)
;;     (message "proxy closed")))

(defun +yefi/toggle-proxy ()
  (interactive)
  (if (null url-proxy-services)
      (progn
        (setq url-proxy-services
              '(("http" . "127.0.0.1:7890")
                ("https" . "127.0.0.1:7890")))
        (message "proxy started."))
    (setq url-proxy-services nil)
    (message "proxy closed.")))

(provide 'init-funcs)
