(use-package projectile
  :ensure t
  :commands (projectile-mode
             projectile-find-file
             projectile-global-mode
             projectile-project-p
             projectile-sort-by-recentf-first
             projectice-sort-by-recently-active-first)
;;  :bind (("C-x p f" . projectile-find-file)
;;         ("C-x p F" . projectile-find-file-ignored)
;;         ("C-x p d" . projectile-find-dir) ; Maybe use C-x d
;;         ("C-x p c" . projectile-switch-project)
;;         ("C-x p b" . projectile-switch-to-buffer)
;;         ("C-x p a" . projectile-ag))
  :diminish ""
  :config
  (progn
    (use-package helm-projectile
      :ensure t
      :commands (helm-projectile)
      :bind ("C-x p h" . helm-projectile))

    (defun projectile-find-file-ignored ()
      "Projectile find file without ignore."
      (interactive)
      (let ((projectile-git-command "git ls-files -zco"))
        (call-interactively 'projectile-find-file)))

    (projectile-global-mode 1)))
(provide 'init-projectile)
