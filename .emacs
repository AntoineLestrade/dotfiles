(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries, like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

;; use-package + Manage packages
(if (not (package-installed-p 'use-package))
    (progn
      (package-refresh-contents)
      (package-install 'use-package)))
;; https://github.com/jwiegley/use-package
(require 'use-package)

(use-package xahk-mode
  :mode ".ahk$"
  ;:commands xahk-mode
  ;:init
  ;(add-to-list 'auto-mode-alist '("\\.ahk$" . xahk-mode))
  :ensure t)

;; GIT {{{
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
(use-package egg
  :ensure t)

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

;; }}}
(use-package emmet-mode
  :ensure t)

(use-package ace-jump-mode
  :bind ("C-c SPC" . ace-jump-mode)
  :ensure t)

(use-package monokai-theme
  :ensure t)

(use-package undo-tree
  :init
  (global-undo-tree-mode)
  :ensure t)


; Disable toolbar
(tool-bar-mode -1)
