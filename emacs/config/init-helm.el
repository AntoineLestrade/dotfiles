(use-package helm
  :commands helm-mode
  :config
  (global-set-key (kbd "M-x") 'helm-M-x)
  (helm-autoresize-mode t)
  :idle (helm-mode 1)
  :ensure t)

(provide 'init-helm)
