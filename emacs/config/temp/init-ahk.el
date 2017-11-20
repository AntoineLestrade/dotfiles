;;; init-ahk.el --- Emacs configuration for AutoHotScript support
;;;
;;; Commentary:
;;;;; Some basic configuration for ahk scripts.
;;; Code:
(when (bound-and-true-p loc-enable-packages)
  (when (maybe-require-package 'xahk-mode)
    (add-to-list 'auto-mode-alist '("\\.ahk\\'" . xahk-mode))))
(provide 'init-ahk)
;;; init-ahk ends here

;; Local variables:
;;   no-byte-compile: t
;; End:
