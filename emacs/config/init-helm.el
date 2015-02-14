;;; init-helm.el --- Emacs configuration: Initialize Helm and some Helm source
;;; Commentary:
;;; Code:
(use-package helm
  :commands helm-mode
  :config
  (global-set-key (kbd "M-x") 'helm-M-x)
  (helm-autoresize-mode t)
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
