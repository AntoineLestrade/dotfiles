(use-package csharp-mode
  :mode "\\.cs\\'"
  :ensure t)
(use-package omnisharp
  ;;  :disabled (not (enable-omnisharp))
  :if enable-omnisharp
  :commands omnisharp-mode
  :ensure t)
(when 'enable-omnisharp
  (add-hook 'csharp-mode-hook 'omnisharp-mode)
  (defvar omnisharp-server-executable-path
    "c:/NoInstall_Programs/omnisharp/omnisharp.exe"
    "redefine omnisharp path"
    ))

(provide 'init-csharp)
;;; init-csharp.el ends here.
