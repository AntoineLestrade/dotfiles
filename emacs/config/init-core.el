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

(use-package diminish :ensure t)

(use-package ido
  :commands ido-mode
  :init
  (progn
    (setq
     ido-enable-tramp-completion t
     ido-enable-flex-matching t
     ido-create-new-buffers 'always
     ido-use-filename-at-point nil
     ido-enable-dot-prefix t
     ido-max-prospects 50
     ido-auto-merge-work-directories-length -1
     ido-ignore-buffers '("\\` " "*Ido Completions*" "*helm*")
     ido-ignore-files '("\\`#" "\\`.#" "\\`\\.\\./" "\\`\\./")
     ido-ignore-extensions t
     ido-default-buffer-method 'selected-window
     ido-default-file-mdethod 'selected-window
     ido-save-directory-list-file (expand-file-name
                                   "ido.last" loc-data-dir))
    ;;(ido-mode 'both)
    ;;(ido-everywhere 1)
    )
  :config
  (progn
    (use-package ido-vertical-mode
      :ensure t
      :commands (turn-on-ido-vertical
                 ido-vertical-mode)
      :init
      (progn
        (turn-on-ido-vertical)))
    (use-package flx-ido
      :ensure t
      :init
      (progn
        (flx-ido-mode 1))
      )

    ))

(use-package ag
  :commands (
             ag
             ag-dired
             ag-dired-regexp
             ag-files
             ag-project-files
             ag-project-dired
             ag-project-dired-regexp)
  :init
  (progn
    (setq ag-highlight-search t
          ag-arguments (list "--smart-case" "--nogroup" "--column" "-M 190" "--")))
  ;; maybe use wgrep-ag
  :ensure t)

;; Libraries
;;(use-package flycheck
;;  :defer t
;;  :ensure t)

;;(use-package fringe-helper :ensure t :defer t)
(use-package flycheck
  :ensure t
  :commands (flycheck-mode global-flycheck-mode)
  :diminish (;;(global-flycheck-mode . "fc")
   (flycheck-mode . "fc"))
  :bind (("M-o e" . flycheck-list-errors))
  :init
  (progn
    (setq
    flycheck-completion-system 'ido)
    )
  :config
  (progn
    (use-package helm-flycheck
      :ensure t
      :commands (helm-flycheck)
      :init
      (progn
        (bind-key "C-c ! h" 'helm-flycheck flycheck-mode-map)))
    ;;(when (fboundp 'define-fringe-bitmap)
    ;;  (require 'fringe-helper)
    ;;  (fringe-helper-define 'vertical-wave-bitmap '(center repeat)
    ;;                        "...XXX."
    ;;                        "...XXX."
    ;;                        "..XXX.."
    ;;                        "..XXX..")
    ;;  (flycheck-define-error-level 'error
    ;;    :severity 100
    ;;    :overlay-category 'flycheck-error-overlay
    ;;    :fringe-bitmap 'vertical-wave-bitmap
    ;;    :finge-face 'flycheck-fringe-error
    ;;    :error-list-face 'flycheck-error-list-error)
    ;;  (flycheck-define-error-level 'warning
    ;;    :severity 10
    ;;    :overlay-category 'flycheck-error-overlay
    ;;    :fringe-bitmap 'vertical-wave-bitmap
    ;;    :fringe-face 'flycheck-fringe-warning
    ;;    :error-list-face 'flycheck-error-list-warning)
    ;;  (flycheck-define-error-level 'info
    ;;    :severity -1
    ;;    :overlay-category 'flycheck-info-overlay
    ;;    :fringe-bitmap 'vertical-wave-bitmap
    ;;    :fringe-face 'flycheck-fringe-info
    ;;    :error-list-face 'flycheck-error-list-info
    ;;    ))
    )
  )

(provide 'init-core)
;;; init-core.el ends here
