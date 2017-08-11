;;; init-company.el --- .
;;; Commentary:
;;; Code:

(when (bound-and-true-p loc-enable-packages)
  (when (maybe-require-package 'company)
    (add-hook 'after-init-hook 'global-company-mode)
    (with-eval-after-load 'company
      (diminish 'company-mode "CMP"))
    (when (maybe-require-package 'company-quickhelp)
      ;;  (define-key company-quickhelp-mode-map (kbd "M-h") nil))
      (add-hook 'after-init-hook 'company-quickhelp-mode)))
  (setq company-tootltip-flip-when-above t))
(provide 'init-company)
;;; init-company ends here

;;; Local Variables:
;;; no-byte-compile: t
;;; End:
