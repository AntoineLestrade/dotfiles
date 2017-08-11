;;; init-gherkin.el --- Emacs configuration for features support
;;;
;;; Commentary:
;;; Code:
(when (and (bound-and-true-p loc-enable-packages)
	   (maybe-require-package 'feature-mode))
  (add-to-list 'auto-mode-alist '("\.feature$" . feature-mode)))

(provide 'init-gherkin)
;;; init-gherkin.el ends here

;; Local variables:
;;    no-byte-compile: t
;; End:
