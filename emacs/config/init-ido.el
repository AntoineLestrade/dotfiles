(require 'ido)
(ido-mode 1)
(ido-everywhere t)
(setq ido-enable-flex-matching t
      ido-use-filename-at-point 'guess
      ido-auto-merge-work-directories-length 0
      ido-use-virtual-buffers t)

(require-package 'ido-ubiquitous)
(ido-ubiquitous-mode 1)

(require-package 'smex)
(setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory))
(global-set-key [remap execute-extended-command] 'smex)


(require-package 'idomenu)

;; Allow the same buffer to be open in different frames
(setq ido-default-buffer-method 'selected-window)

;; http://www.reddit.com/r/emacs/comments/21a4p9/use_recentf_and_ido_together/cgbprem
(add-hook 'ido-setup-hook (lambda () (define-key ido-completion-map [up] 'previous-history-element)))


;;(my-require-packages '(flx-ido ido-ubiquitous smex))
;;
;;(require 'ido)
;;(require 'ido-ubiquitous)
;;(require 'flx-ido)
;;
;;(setq ido-enable-prefix nil
;;      ido-enable-flex-matching t
;;      ido-create-new-buffer 'always
;;      ido-use-filename-at-point 'guess
;;
;;      ido-save-directory-list-file (expand-file-name "ido.hist" loc-saves-dir)
;;      ido-default-file-method 'selected-window
;;      ;;ido-auto-merge-work-directories-length -1
;;      )
;;
;;(ido-mode +1)
;;(ido-ubiquitous-mode +1)
;;
;;;; smarter fuzzy matching for id
;;(flx-ido-mode +1)
;;;; disable ido faces to see flx highlights
;;(setq ido-use-faces nil)
;;
;;;; smex, remember recently and most frequently used commands
;;(require 'smex)
;;(setq smex-save-file (expand-file-name ".smex-items" loc-saves-dir))
;;(smex-initialize)
;;(global-set-key (kbd "M-x") 'smex)
;;(global-set-key (kbd "M-X") 'smex-major-mode-commands)

(provide 'init-ido)
;;; init-ido.el ends here
