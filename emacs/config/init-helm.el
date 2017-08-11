;;; init-helm --- Configure helm -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (bound-and-true-p loc-enable-packages)
  (require-package 'helm)
  (require 'helm-config)
  (when (bound-and-true-p loc-enable-projectile)
    (require-package 'helm-projectile)
    (require 'helm-projectile))

  ;; Keybindings
  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (global-set-key (kbd "M-x") 'helm-M-x)
  ;;(global-set-key (kbd "C-x C-m") 'helm-M-x)
  (global-set-key (kbd "C-y") 'helm-show-kill-ring)
  (global-set-key (kbd "C-x b") 'helm-mini) ; replace *helm-mode-switch-buffer*, adds recentf files
  (global-set-key (kbd "C-x C-f") 'helm-find-files)

  (helm-mode 1)
  )

(provide 'init-helm)
;;; init-helm.el ends here
