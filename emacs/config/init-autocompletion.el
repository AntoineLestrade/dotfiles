;;; init-autocompletion.el --- My Emacs config: Autocompletion
;;; Commentary:
;; Initialize and configure auto-completion packages
;;; Code:

(use-package ycmd
  :idle
  (ycmd-setup)
  :ensure t)

(use-package company-ycmd
  :idle
  (company-ycmd-setup)
  :ensure t)

(use-package company
  :idle
  (progn
    (defgroup dotemacs-company nil
      "Configuration options for company-mode."
      :group 'dotemacs
      :prefix 'dotemacs-company)
    (defcustom dotemacs-company/ycmd-server-command nil
      "The path to the ycmd package."
      :group 'dotemacs-company)
    (require 'company)
    (setq company-idle-delay 0.2)
    (setq company-minimum-prefix-length 1)
    (setq company-show-numbers t)
    (setq company-tooltip-limit 20)

    (setq company-dabbrev-downcase nil)
    (setq company-dabbrev-ignore-case t)

    (setq company-dabbrev-code-ignore-case t)
    (setq company-dabbrev-code-everywhere t)

    (setq company-etags-ignore-case t)

    (unless (face-attribute 'company-tooltip :background)
      (set-face-attribute 'company-tooltip nil :background "black" :foreground "gray40")
      (set-face-attribute 'company-tooltip-selection nil :inherit 'company-tooltip :background: "gray15")
      (set-face-attribute 'company-preview nil :background "black")
      (set-face-attribute 'company-preview-common nil :inherit 'company-preview :foreground "gray40")
      (set-face-attribute 'company-scrollbar-bg nil :inherit 'company-tooltip :background "gray20")
      (set-face-attribute 'company-scrollbar-fg nil :background "gray40"))

    (setq company-global-modes
          '(not
            eshell-mode comint-mode org-mode erc-mode))

    (defadvice company-complete-common (around advice-for-company-complete-common activate)
      (when (null (yas-expand))
        ad-do-it))

    (global-company-mode)
    )
  :idle
  :ensure t)

(provide 'init-autocompletion)
;;; init-autocompletion.el ends here.
