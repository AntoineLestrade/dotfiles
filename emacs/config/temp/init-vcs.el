;;; init-vcs.el --- VCS Related configuration
;;; Commentary:
;;; Code:

(when (bound-and-true-p loc-enable-packages)
  (when (maybe-require-package 'diff-hl)
    (add-hook 'dired-mode-hook #'diff-hl-dired-mode)
    (global-diff-hl-mode +1))
  (when (maybe-require-package 'magit)
    ()))

(provide 'init-vcs)
;;; init-vcs.el ends here
