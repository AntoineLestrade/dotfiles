(require-package 'magit)
(require-package 'git-commit-mode)
(require-package 'git-rebase-mode)
(require-package 'gitignore-mode)
(require-package 'gitconfig-mode)
(require-package 'git-messenger)
(require-package 'git-timemachine)

(setq-default
 magit-diff-refine-hunk t
 magit-completing-read-function 'magit-ido-completing-read)
(setq magit-last-seen-setup-instructions "1.4.0")

(global-set-key [(meta f12)] 'magit-status)

(require-package 'fullframe)
(with-eval-after-load 'magit
  (define-key magit-status-mode-map (kbd "C-M-<up>") 'magit-goto-parent-section)
  (fullframe magit-status magit-mode-quit-window)
  )

(add-hook 'git-commit-mode-hook 'goto-address-mode)
;;(with-eval-after-load 'session
;;  (add-to-list 'session-mode-disable-list 'git-commit-mode))

;;(with-eval-after-load 'magit
;;  diminish 'magit-auto-revert-mode)

(require-package 'git-messenger)
(global-set-key (kbd "C-x v p") #'git-messenger:popup-message)

(provide 'init-git)
;;; init-git.el ends here
