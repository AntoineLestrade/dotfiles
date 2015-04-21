(require-package 'diff-hl)
(add-hook 'prod-mode-hook 'turn-on-diff-hl-mode)
(add-hook 'vc-dir-mode-hook 'turn-on-diff-hl-mode)
;;(global-diff-hl-mode +1)

(provide 'init-vc)
;;; init-vc.el ends here
