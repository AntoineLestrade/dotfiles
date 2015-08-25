;;; init-ido.el --- Emacs configuration: Ido and related packages and settings
;;; Commentary:
;;;   Initialize Ido, use Ido everywhere.
;;;      Flx: Flexible matching for Ido
;;;      ido-vertical-mode: show Ido matches vertically.
;;;           We should set some key bindings
;;;      smex: remember recently and most frequently used commands
;;;; Code:
(require 'ido)
(require-package 'ido-vertical-mode)
(ido-mode 1)
(ido-everywhere t)
(setq ido-enable-flex-matching t
      ido-use-filename-at-point 'guess
      ido-auto-merge-work-directories-length 0
      ido-use-virtual-buffers t)

(require 'ido-vertical-mode)
(ido-vertical-mode 1) ;; TODO: Keybindings? https://github.com/creichert/ido-vertical-mode.el

(require-package 'ido-ubiquitous)
(require 'ido-ubiquitous)
(ido-ubiquitous-mode 1)

(require-package 'flx-ido)
(flx-ido-mode +1)
(setq ido-use-faces nil) ;; In order to see flx highlights

(require-package 'smex)
(setq smex-save-file (expand-file-name ".smex-items" loc-cache-dir))
(global-set-key [remap execute-extended-command] 'smex)


(require-package 'idomenu)

;; Allow the same buffer to be open in different frames
(setq ido-default-buffer-method 'selected-window)

;; http://www.reddit.com/r/emacs/comments/21a4p9/use_recentf_and_ido_together/cgbprem
(add-hook 'ido-setup-hook (lambda () (define-key ido-completion-map [up] 'previous-history-element)))


;;;; smex, remember recently and most frequently used commands
;;(require 'smex)
;;(setq smex-save-file (expand-file-name ".smex-items" loc-saves-dir))
;;(smex-initialize)
;;(global-set-key (kbd "M-x") 'smex)
;;(global-set-key (kbd "M-X") 'smex-major-mode-commands)

(provide 'init-ido)
;;; init-ido.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
