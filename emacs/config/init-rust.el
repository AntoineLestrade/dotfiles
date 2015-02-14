;;; init-rust.el --- My Emacs : rust configurration
;;
;;; Commentary:
;; Some basic configuration for rust programming;
;;; Code:

(use-package rust-mode
  :mode "\\.rs\\'"
  :ensure t)

(use-package flycheck-rust
;  :mode ("\\.rs\\'"  flycheck-mode)
;  :config
                                        ;  (flycheck-rust-setup)
  :init
  (eval-after-load 'flycheck
    '(add-hook 'flycheck-mode-hook #'flycheck-rust-setup))
  :ensure t)

 (provide 'init-rust)
;;; init-rust.el ends here
