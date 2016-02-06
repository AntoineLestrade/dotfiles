;;; init-themes.el --- .
;;; Commentary:
;;; Code:
;;(require-package 'color-theme-sanityinc-solarized)
;;(require-package 'color-theme-sanityinc-tomorrow)
(require-package 'spacemacs-theme)
;; TODO: TEST
(setq-default custom-enabled-themes '(sanityinc-tomorrow-night))
;; ;(setq-default custom-enabled-themes '(sanityinc-tomorrow-day))

 (defun reapply-themes ()
   "Forcibly load the themes listed in `custom-enabled-themes'."
   (dolist (theme custom-enabled-themes)
     (unless (custom-theme-p theme)
       (load-theme theme)))
   (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes))))

 (add-hook 'after-init-hook 'reapply-themes)
(provide 'init-themes)
;;; init-themes.el ends here

;; Local variables:
;; no-byte-compile: t
;; End:
