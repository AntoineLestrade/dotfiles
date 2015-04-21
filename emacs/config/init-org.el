;;; init-org.el --- Org mode customization
;;; Commentary:
;;; Code:
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)

(setq org-completion-use-ido t)

(provide 'init-org)
;;; init-org.el ends here
