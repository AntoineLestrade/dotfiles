;;; init-editor --- Core editor config
;;; Commentary:
;;; Code:

(when (fboundp 'electric-pair-mode)
  (electric-pair-mode))

(setq-default indent-tabs-mode nil
	      tab-width 8)
(setq-default require-final-newline t)

(global-visual-line-mode t)

;;; ediff
(setq-default ediff-split-window-function 'split-window-horizontally)
(setq-default ediff-window-setup-function 'ediff-setup-windows-plain)

;; When a selection exists, any key should delete it
(delete-selection-mode t)

(global-auto-revert-mode t)

(transient-mark-mode t)

;;;; New lines
(defun myconfig/newline-below ()
  "Move to end of line, enter a new line, and reindent."
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))
(defun myconfig/newline-above ()
  "Move to begin of line, enter a new line, go to this line."
  (interactive)
  (move-beginning-of-line 1)
  (newline-and-indent)
  (forward-line -1))
(global-set-key (kbd "S-<return>") 'myconfig/newline-below)
(global-set-key (kbd "C-S-<return>") 'myconfig/newline-above)

;; Add the ability to cut the current line, without marking it
(defadvice kill-region (before smart-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
                         (list (line-beginning-position)
                               (line-beginning-position 2)))))

(require-package 'browse-kill-ring)
;; (setq browse-kill-ring-separator "\f")
;; (after-load 'page-break-lines
;;   (push 'browse-kill-ring-mode page-break-lines-modes))
(browse-kill-ring-default-keybindings)
(global-set-key (kbd "M-y") 'browse-kill-ring)

;; smart pairing
(require-package 'smartparens)
(require 'smartparens-config)
(setq sp-base-key-bindings 'paredit)
(setq sp-autoskip-closing-pair 'always)
(setq sp-hybrid-kill-entire-symbol nil)
(sp-use-paredit-bindings)
(show-smartparens-global-mode +1)


(require-package 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)


(require-package 'move-dup)
(global-set-key [M-up] 'md/move-lines-up)
(global-set-key [M-down] 'md/move-lines-down)
(global-set-key [M-S-up] 'md/move-lines-up)
(global-set-key [M-S-down] 'md/move-lines-down)

(global-set-key (kbd "C-c p") 'md/duplicate-down)
(global-set-key (kbd "C-c P") 'md/duplicate-up)


(require-package 'highlight-escape-sequences)
(hes-mode)

;;(require-package 'guide-key)
;;(setq guide-key/guide-key-sequence '("C-x" "C-c" "C-x 4" "C-x 5" "C-c ;" "C-c ; f" "C-c ' f" "C-x n" "C-x C-r" "C-x r"))
;;(guide-key-mode 1)
;;(diminish 'guide-key-mode)


(require 'recentf)

(setq recentf-save-file (expand-file-name "recentf" loc-cache-dir)
      recentf-max-saved-items 500
      recentf-max-menu-items 15
      ;; disable recentf-cleanup on Emacs start, because it can cause problems with remote files
      recentf-auto-cleanup 'never)

(recentf-mode +1)


;; HideShow
(diminish 'hs-minor-mode)
(add-hook 'prog-mode-hook (lambda ()
                            (hs-minor-mode 1)))



(provide 'init-editor)
;;; init-editor.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
