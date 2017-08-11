;;; init-projectile.el --- Emacs projectile configuration
;;; Comment:
;;; Code:
(when (and (bound-and-true-p loc-enable-packages)
	   (maybe-require-package 'projectile))
  (setq projectile-cache-file (expand-file-name "projectile.cache" loc-cache-dir))
  (setq projectile-known-projects-file (expand-file-name "projectile-bookmarks.eld" loc-cache-dir))

  ;;(when (eq system-type 'windows-nt)
  ;;  (setq projectile-indexing-method 'alien))
  (projectile-global-mode t))
(provide 'init-projectile)
;;; init-projectile.el ends here
