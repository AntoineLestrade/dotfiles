;;; init-ui.el --- My Emacs configuration: initializr UI settings
;;; Commentary:
;;; Code:

(use-package color-theme
  :ensure t
  :config
  (progn
    (use-package color-theme-solarized
      :config (load-theme 'solarized t)
      :ensure t)))
;;(use-package base16-theme :ensure t)
;;(load-theme 'base16-eighties t)

;;(use-package color-theme-solarized
;;(use-package color-theme-sanityinc-solarized
;;  :init
  ;;(add-hook 'after-make-frame-functions
  ;;          (lambda (frame)
  ;;            (set-frame-parameter frame
  ;;                                 'background-mode
  ;;                                 (if (display-graphic-p frame) 'light 'dark))
  ;;            (enable-theme 'solarized)))
  ;;(load-theme 'solarized t)
;;  :ensure t)
;;(require 'color-theme-solarized)
;;(load-theme 'solarized t)
;;(require 'color-theme)
;;(add-to-list 'load-path "~/.emacs.d/config/solarized-theme")
;;(require 'solarized)


;;(use-package color-theme-sanityinc-tomorrow :ensure t)
;;(use-package zenburn-theme
;;  :ensure t)
;;(use-package ujelly-theme :ensure t)
;;(use-package monokai-theme :ensure t)


;; Disable toolbar
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))

(menu-bar-mode -1)
(blink-cursor-mode -1)

(setq inhibit-startup-screen t)
;; improve scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;;(setq-default show-trailing-whitespace t)
;;(setq whitespace-global-modes '(prog-mode fundamental-mode text-mode))
(add-hook 'prog-mode-hook '(lambda()
                             (setq show-trailing-whitespace t)))

(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)
(global-hl-line-mode t)

;; Chage frame title
(setq frame-title-format
      '("" invocatin-name " Emacs - " (:eval (if (buffer-file-name)
						 (abbreviate-file-name (buffer-file-name))
					       "%b"))))

(provide 'init-ui)
;;; init-ui.el ends here
