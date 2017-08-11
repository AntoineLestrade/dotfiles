(with-eval-after-load 'flycheck
  (add-to-list 'flycheck-checkers 'html-tidy)
  (flycheck-add-mode 'html-tidy 'web-mode))

(provide 'init-flycheck)
;;; init-flycheck.el ends here
