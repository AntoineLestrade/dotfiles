;;; init-ui.el --- My emacs config - User Interface settings
;;; Commentary:
;;; Code:

;; Suppress GUI features
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)
;; TODO: to check
(setq inhibit-startup-echo-area-message t)

(setq-default indicate-empty-lines t)

(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; improve scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

(fset 'yes-or-no-p 'y-or-n-p)

(let ((no-border '(internal-border-width . 0)))
  (add-to-list 'default-frame-alist no-border)
  (add-to-list 'initial-frame-alist no-border))

;;(setq frame-title-format
;;      '("" invocation-name " Emacs - " (:eval (if (buffer-file-name)
;;                                                  (abbreviate-file-name (buffer-file-name))
;;                                                "%b"))))
(setq frame-title-format
      '("" invocation-name " Emacs - "(:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

(add-hook 'term-mode-hook
          (lambda ()
            (setq line-spacing 0)))


;;(require-package 'smart-mode-line)
;;(setq sml/no-confirm-load-theme t)
;;;; delegate theming to the currently active theme
;;(setq sml/theme nil)
;;(add-hook 'after-init-hook #'sml/setup)

(provide 'init-ui)
;;; init-ui.el ends here
