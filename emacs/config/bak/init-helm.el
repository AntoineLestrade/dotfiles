;;; init-helm --- .
;;; Commentary:
;;; Code:

(require-package 'helm)
(require-package 'helm-projectile)
(require 'helm-config)
(require 'helm)
(require 'helm-projectile)




(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-i")  'helm-execute-persistent-action)
(define-key helm-map (kbd "C-z")  'helm-select-action)

(define-key helm-command-map (kbd "o")     'helm-occur)
(define-key helm-command-map (kbd "g")     'helm-do-grep)
(define-key helm-command-map (kbd "C-c w") 'helm-wikipedia-suggest)
(define-key helm-command-map (kbd "SPC")   'helm-all-mark-rings)


(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-buffers-fuzzy-matching           t
      helm-move-to-line-cycle-in-source     t
      helm-ff-search-library-in-sexp        t
      helm-ff-file-name-history-use-recentf t

      helm-scroll-amount                    8

      helm-split-window-in-side-p           t
      )


;; Helm everywhere
(require-package 'helm-ag)
(require-package 'helm-descbinds)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-m") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring) ;; conflics with browse-kill-ring
(global-set-key (kbd "C-x b") 'helm-mini)
;;(global-set-key (kbd "C-x C-b") 'helm-buffers-list) ;; keep ibuffer
(global-set-key (kbd "C-h f") 'helm-apropos)
(global-set-key (kbd "C-h r") 'helm-info-emacs)
(global-set-key (kbd "C-h C-l") 'helm-locate-library)
(global-set-key (kbd "C-c f") 'helm-recentf)

(define-key minibuffer-local-map (kbd "C-c C-l") 'helm-minibuffer-history)
(define-key shell-mode-map (kbd "C-c C-l") 'helm-comint-input-ring)

;; use helm to list eshell history
(add-hook 'eshell-mode-hook
          #'(lambda ()
              (substitute-key-definition 'eshell-list-history 'helm-eshell-history eshell-mode-map)))

(substitute-key-definition 'find-tag 'helm-etags-select global-map)
(setq projectile-completion-system 'helm)
(helm-descbinds-mode)


(provide 'init-helm)
;;; init-helm.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
