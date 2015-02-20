;;; init-helm.el --- Emacs configuration: Initialize Helm and some Helm source
;;; Commentary:
;;; Code:
(use-package helm
  :commands (helm-M-x helm-mode)
  :bind (("M-x" . helm-M-x))
  :init
  (progn
    (setq
     ;; Prevent escaping from minibuffer during helm session (default t)
     helm-prevent-escaping-from-minibuffer nil
     ;; Max length of buffer names before truncate
     helm-buffer-max-length 50
     )
    (use-package helm-imenu
      :bind (("M-o M-i" . helm-imenu))
      :config
      ))
  :config
  (progn
    (helm-autoresize-mode t)
    (use-package helm-config)
    (use-package helm-files)
    (use-package helm-help)
    (use-package helm-projectile)
    (use-package helm-adaptative
      :commands (helm-adaptative-mode))
    )
  :idle (helm-mode 1)
  :ensure t)

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
