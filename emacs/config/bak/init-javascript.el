(require-package 'json-mode)
(require-package 'js2-mode)

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(with-eval-after-load 'js2-mode
  (add-hook 'js2-mode-hook (lambda ()
                             (setq mode-name "JS2")
                             (js2-imenu-extras-mode +1))))


(require-package 'rainbow-delimiters)
(dolist (hook '(js2-mode-hook js-mode-hook json-mode-hook))
  (add-hook hook 'rainbow-delimiters-mode))

(provide 'init-javascript)
;;; init-javascript.el ends here
