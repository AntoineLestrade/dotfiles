;;; init-uniquify.el --- My emacs config: Uniquify
;;; Commentary:
;;;     This file contains unniquify settings
;;; Code:

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
;;; init-uniquify.el ends here
