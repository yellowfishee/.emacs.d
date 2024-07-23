(defun my/set-font-size ()
  (interactive) 
  (cond (my/is-termux
          (setq my/font-height 125
                my/latex-preview-scale 1.25))
        (t (pcase (display-pixel-height)
              ((pred (> 999))
              (setq my/font-height 130
                    my/latex-preview-scale 1.3))
              ((pred (> 1300))
              (setq my/font-height 160
                    my/latex-preview-scale 1.6))
              (_ (setq my/font-height 180
                      my/latex-preview-scale 1.8))))))