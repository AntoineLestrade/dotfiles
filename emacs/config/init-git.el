(use-package git-gutter
  :init
  (global-git-gutter-mode)
  :ensure t)

;;; MAGIT {{{
;;;;;; https://github.com/magit/magit
;;;;;; dependencies: git-commit-mode git-rebase-mode
(use-package magit
  :ensure t)
;;; }}}
;(use-package egg
;  :ensure t)

;;;;;; https://github.com/magit/git-modes
(use-package gitignore-mode
  :ensure t)
(use-package gitconfig-mode
  :ensure t)
;;;;;; https://github.com/pidu/git-timemachine
(use-package git-timemachine
  :commands git-timemachine
  :ensure t)
;;;;;; https://github.com/syohex/emacs-git-messenger
;;;;;; Key binding when popup is open
;;;;;;    M-w  |  Copy commit message
;;;;;;    c    |  Copy commit id
;;;;;;    d    |  View git diff
;;;;;;    s    |  git show --stat
;;;;;;    S    |  git show --stat -p
;;;;;;    q    |  quit
(use-package git-messenger
  :bind ("C-x v p" . git-messenger:popup-message)
  :config
  (progn (git-messenger:show-detail t))
  :ensure t)

(provide 'init-git)
