;;; init-ahk.el --- My emacs : AutoHotScript configuration
;;;
;;; Commentary:
;;;;; Some basic configuration for ahk scripts.
;;; Code:
(require-package 'xahk-mode)
(add-to-list 'auto-mode-alist '("\\.ahk\\'" . xahk-mode))

(provide 'init-ahk)
;;; init-ahk ends here
