;;; init-ui.el --- Emacs config: UI optimizations and tweaks.

(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(menu-bar-mode -1)
(blink-cursor-mode -1)
(setq inhibit-startup-screen t)

;; improve scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; Change frame title
(setq frame-title-format
      '("" invocation-name " Emacs - " (:eval (if (buffer-file-name)
                                                  (abbreviate-file-name (bffer-file-name))
                                                "%b"))))
(load-theme 'zenburn)

(provide 'init-ui)
;;; init-ui.el ends here
