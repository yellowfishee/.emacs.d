(defun my-setup-org-roam ()
  "设置 org-roam 的相关配置，仅在 org-roam 可用时执行。"
  (when (require 'org-roam nil 'noerror)  ;; 检查 org-roam 是否存在
    (setq org-roam-capture-templates
          '(("d" "default" plain "%?"
             :target (file+head "${slug}-%<%Y%m%d%H%M%S>.org"
                                "#+title: ${title}\n")
             :unnarrowed t)
            ("r" "draft" plain "%?"
             :target (file+head "Draft-%<%Y%m%d%H%M%S>.org"
                                "#+title: Draft\n")
             :unnarrowed t)))

    (add-hook 'org-roam-capture-before-finalize 'my-org-roam-capture-set-slug)))

(my-setup-org-roam)  ;; 在启动时调用设置函数

(provide 'zettel-card-notes-org-roam)
