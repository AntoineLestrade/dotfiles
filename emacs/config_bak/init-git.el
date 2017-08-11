(use-package git-gutter
  :init
  (global-git-gutter-mode)
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
