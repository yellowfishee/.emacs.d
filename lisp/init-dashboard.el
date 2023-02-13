(require 'dashboard)
(dashboard-setup-startup-hook)
;; Or if you use use-package
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))

(setq dashboard-center-content t)

(setq dashboard-show-shortcuts nil)

(defun dashboard-insert-custom (list-size)
  (insert "Custom text"))
(add-to-list 'dashboard-item-generators  '(custom . dashboard-insert-custom))
(add-to-list 'dashboard-items '(custom) t)

(setq dashboard-item-names '(("Recent Files:" . "Recently opened files:")
                             ("Agenda for today:" . "Today's agenda:")
                             ("Agenda for the coming week:" . "Agenda:")))

(setq dashboard-set-heading-icons t)
(setq dashboard-set-navigator t)


         
;; Format: "(icon title help action face prefix suffix)"
(setq dashboard-navigator-buttons
      `(;; line1
        ((,(all-the-icons-octicon "mark-github" :height 1.1 :v-adjust 0.0)
         "codexc.cn"
         "Browse homepage"
         (lambda (&rest _) (browse-url "http://codexc.cn")))
        ("★" "Star" "Show stars" (lambda (&rest _) (show-stars)) warning)
        ("?" "" "?/h" #'show-help nil "<" ">"))))

(setq dashboard-startup-banner "~/.emacs.d/elpa/dashboard-20230126.2032/banners/head.jpg")

(message "%s" (dashboard-choose-banner))


(provide 'init-dashboard)
