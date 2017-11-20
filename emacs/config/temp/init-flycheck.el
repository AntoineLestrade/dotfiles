;;; init-flycheck.el --- Fetch flycheck package
;;;
;;; Commentary:
;;;  Fetch the flychek package and enable it
;;;  Checkers are added in language related configuration files
;;; Code:
(when (bound-and-true-p loc-enable-packages)
  (when (maybe-require-package 'flycheck)
    (add-hook 'after-init-hook #'global-flycheck-mode)
    (setq flycheck-check-syntax-automatically '(save idle-change mode-enabled) ;Do not enable for newline
	  flycheck-idle-delay 0.8
	  flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list
	  )))

(provide 'init-flycheck)
;;; init-flycheck.el ends here
