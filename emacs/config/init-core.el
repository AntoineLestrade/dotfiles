;;; init-core.el --- My Emacs config: Customize core options
;;; Commentary:
;;; Code:
;; Correctly manage tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 8)

(setq require-final-newline t)

;; When a selection exists, any key should delete it
(delete-selection-mode t)

;; Store all backup and autosave files in cache dir
(setq backup-directory-alist
      `((".*" . ,(concat loc-cache-dir "backups")))
      auto-save-file-name-transform
      `((".*" ,(concat loc-cache-dir "backups") t))
      auto-save-list-file-prefix
      (concat loc-cache-dir "auto-save-list/save-"))

(global-auto-revert-mode t)

;; Ediff
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)


(use-package ag
  :commands (ag-dired ag-dired-regexp ag-project-dired ag-project-dired-regexp)
  :ensure t)

;; Libraries
(use-package flycheck
  :ensure t)

(provide 'init-core)
;;; init-core.el ends here
