;;; init-flycheck.el --- .
;;; Commentary:
;;; Code:

(require-package 'flycheck)
(add-hook 'after-init-hook 'global-flycheck-mode)

(setq flycheck-check-syntax-automatically '(save idle-change mode-enabled)
      flycheck-idle-change-delay 0.8)
(setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)

(provide 'init-flycheck)
;;; init-flycheck.el ends here
