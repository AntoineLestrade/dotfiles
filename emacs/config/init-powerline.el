;;(when (bound-and-true-p loc-enable-packages)
;;  (require-package 'telephone-line)
;;  (require 'telephone-line)
;;  (require 'telephone-line-config)
;;  (telephone-line-evil-config))
;;  (setq telephone-line-evil-use-short-tag t)
(when (bound-and-true-p loc-enable-packages)
  (require-package 'spaceline)
  (require 'spaceline-config)
  (spaceline-spacemacs-theme)
  )

(provide 'init-powerline)
;;; init-powerline.el ends here
