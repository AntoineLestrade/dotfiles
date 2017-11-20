;;; init-rust.el --- Emacs configuration for Rust development
;;;
;;; Author: Antoine Lestrade <antoine.lestrade@gmail.com>
;;; Commentary:
;;; Some basic configuration for rust development:
;;;    - Rust syntax
;;;    - Rust flycheck support
;;;    - Racer: rust autocompletion backend
;;;    - Rust-playground: scratchpad for rust :)
;;; Code:
(when (bound-and-true-p loc-enable-packages)
  (maybe-require-package 'toml-mode)
  (when (maybe-require-package 'rust-mode)
    (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode)))
  (when (maybe-require-package 'flycheck-rust)
    (with-eval-after-load 'flycheck
      (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)))
  (when (maybe-require-package 'racer)
    (add-hook 'rust-mode-hook #'racer-mode)
    (add-hook 'rust-mode-hook #'eldoc-mode))
  (maybe-require-package 'rust-playground))

(provide 'init-rust)
;;; init-rust.el ends here
