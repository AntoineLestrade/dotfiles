(use-package web-mode
  :commands web-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.cshtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  :ensure t)

(use-package less-css-mode
  :mode "\\.less\\'"
  :ensure t)

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
