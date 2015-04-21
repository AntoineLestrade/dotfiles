;;; init-dired.el --- .
;;; Commentary:
;;; Code:
(require-package 'dired+)
(require-package 'dired-sort) ;; improve emacs sorting capabilities

(setq-default diredp-hide-detils-initially-flag nil)
(setq dired-dwim-target t)

(with-eval-after-load 'dired
  (require 'dired+)
  (require 'dired-sort)
  (when (fboundp 'global-dired-hide-details-mode)
    (global-dired-hide-details-mode -1))
  (setq dired-recursive-deletes 'top) ;; maybe 'always ?
  (define-key dired-mode-map [mouse-2] 'dired-find-file)
  (add-hook 'dired-mode-hook
            (lambda ()
              ;; (guide-key/add-local-guide-key-sequence "%")
              (diff-hl-dired-mode))))

(provide 'init-dired)
;;; init-dired.el ends here
