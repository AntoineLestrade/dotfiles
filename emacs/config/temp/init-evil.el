;;; init-evil.el --- .
;;; Commentary:
;;; Code:

(when (bound-and-true-p loc-enable-packages)
  (require-package 'evil)
  (require 'evil)
  ;; Use g-; and g-, to go to recent changes
  (require-package 'goto-chg)
  (require-package 'evil-surround)
  ;; enable searching visual selection with *
  (require-package 'evil-visualstar)
  ;; Vim style numeric incrementing and decrementing
  ;;(require-package 'evil-number)

  (require 'evil-visualstar)

  (evil-mode 1)
  (global-evil-surround-mode 1)
)

(provide 'init-evil)
;;; init-evil.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
