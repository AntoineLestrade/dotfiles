;;; init-vc.el --- My emacs config: VC Configuration
;;;
;;; Commentary:
;;;    Some Basic configuration related to versionning control systems
;;; Code:

(require-package 'diff-hl)
(add-hook 'dired-mode-hook 'diff-hl-dired-mode)
(global-diff-hl-mode +1)

(provide 'init-vc)
;;; init-vc.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
