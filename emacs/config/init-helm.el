;;; init-helm.el --- Emacs configuration: Initialize Helm and some Helm source
;;; Commentary:
;;; Code:
(use-package helm
  ;;:commands (helm-M-x helm-mode)
  :bind (("M-x" . helm-M-x))
  :config
  (progn
    (helm-autoresize-mode t)
    ;;(use-package helm-config :ensure t)
    ;;(use-package helm-files :ensure t)
    ;;(use-package helm-help :ensure t)
    ;;(use-package helm-projectile :ensure t)
    ;;(use-package helm-adaptative
    ;;  :commands (helm-adaptative-mode)
    ;;  :ensure t)
    (setq
     ;; Prevent escaping from minibuffer during helm session (default t)
     helm-prevent-escaping-from-minibuffer nil
     ;; Max length of buffer names before truncate
     helm-buffer-max-length 50)
;;    (use-package helm-imenu
;;      :bind ("M-o M-i" . helm-imenu)
;;      :ensure t)
    )
  :ensure t)
(helm-mode 1)
;; Maybe duplicated with AG
(use-package helm-ag
  :commands
  (helm-ag
   helm-ag-this-file
   helm-do-ag
   helm-ag-pop-stack
   helm-ag-clear-stack)
  :ensure t)


(provide 'init-helm)
;;; init-helm.el ends here
