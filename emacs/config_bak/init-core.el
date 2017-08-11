;;; init-core.el --- My Emacs config: Customize core options
;;; Commentary:
;;; Code:


;; Increment / decrement number
(defun my-increment-number-decimal (&optional arg)
  "Increment the number forward from point by 'arg'."
  (interactive "p*")
  (save-excursion
    (save-match-data
      (let (inc-by field-width answer)
        (setq inc-by (if arg arg 1))
        (skip-chars-backward "0123456789")
        (when (re-search-forward "[0-9]+" nil t)
          (setq field-width (- (match-end 0) (match-beginning 0)))
          (setq answer (+ (string-to-number (match-string 0) 10) inc-by))
          (when (< answer 0)
            (setq answer (+ (expt 10 field-width) answer)))
          (replace-match (format (concat "%0" (int-to-string field-width) "d")
                                 answer)))))))

(defun my-decrement-number-decimal (&optional arg)
  (interactive "p*")
  (my-increment-number-decimal (if arg (- arg) -1)))

(global-set-key (kbd "<M-up>") 'my-increment-number-decimal)
(global-set-key (kbd "<M-down>") 'my-decrement-number-decimal)


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
