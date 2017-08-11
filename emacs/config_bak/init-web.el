

(use-package helm-css-scss
  :config
  (progn
    (setq helm-css-scss-insert-close-comment-depth 2
          helm-css-scss-split-with-multiple-windows nil
          helm-css-scss-split-direction 'split-window-horizontally)

    (dolist ($hook '(css-mode-hook scss-mode-hook less-css-mode-hook))
      (add-hook
       $hook (lambda ()
               (local-set-key (kbd "M-s-i") 'helm-css-scss)
               (local-set-key (kbd "M-s-I") 'helm-css-scss-back-to-last-point))))

    (define-key isearch-mode-map (kbd "M-s-i") 'helm-css-scss-from-isearch)
    (define-key helm-css-scss-map (kbd "s-i") 'helm-css-scss-multi-from-helm-css-scss))
  :ensure t)

(provide 'init-web)
