;;; init-rust.el --- My emacs : rust configuration
;;;
;;; Commentary:
;;; Some basic configuration for rust programming.
;;;     - Rust syntax
;;;     - Rust flycheck support
;;;     - Racer: rust autocompletion backend
;;;         Only for `company' completion system
;;;         Not already in melpa
;;; Code:
(require-package 'rust-mode)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
(require-package 'flycheck-rust)
(with-eval-after-load 'flycheck
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

;(with-eval-after-load 'rust-mode
;  (lambda ()
;    (add-to-list 'load-path (expand-file-name "racer/editors/emacs" current-config-dir))
;    (require 'racer)
;    (setq racer-cmd (expand-file-name "racer/target/release/racer.exe" current-config-dir))
;    (setq racer-rust-path "d:/ProgramFiles/Rust_src/src")
                                        ;    ))
;;(if (executable-find "racer")
;;    (require-package 'racer)
;;  (add-hook 'rust-mode-hook
;;            (lambda()
;;              #'racer-activate
;;              #'racer-turn-on-eldoc)
;;            )
;;  )
(require-package 'racer)
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)


;;(add-hook 'company-mode-hook
;;          (lambda ()
;;            (require 'racer)
;;            (setq racer-cmd (expand-file-name "racer/target/release/racer.exe" current-config-dir))
;;            (setq racer-rust-src-path "d:/ProgramFiles/Rust_src/src")))

(provide 'init-rust)
;;; init-rust.el ends here

;; Local variables:
;; no-byte-compile: t
;; End:
