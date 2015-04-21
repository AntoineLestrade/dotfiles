(require 'uniquify)

;;(setq uniquify-buffer-name-style 'forward)
;;(setq uniquify-separator "/")
;;(setq uniquify-after-kill-buffer-p t)
;;(setq uniquify-ignore-buffers-re "^\\*")

(setq uniquify-buffer-name-style 'reverse) ;; maybe 'forward
(setq uniquify-separator " • ")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")


(provide 'init-uniquify)

