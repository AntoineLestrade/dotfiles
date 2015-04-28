;;; init-rust.el --- My emacs : rust configuration
;;;
;;; Commentary:
;;; Some basic configuration for rust programming.
;;;     - Rust syntax
;;;     - Rust flycheck support
;;;     - Racer: rust autocompletion backend
;;;         Only for `company' completion system
;;;         Not already in melpa
;;; Code
(require-package 'rust-mode)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
(require-package 'flycheck-rust)
(with-eval-after-load 'flycheck
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(add-to-list 'load-path (expand-file-name "racer/editors" current-config-dir))

(add-hook 'company-mode-hook
          (lambda ()
            (require 'racer)
            (setq racer-cmd (expand-file-name "racer/target/release/racer" current-config-dir))
            (setq racer-rust-src-path "d:/ProgramFiles/Rust_src/src")))

(provide 'init-rust)
;;; init-rust.el ends here
