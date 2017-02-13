;;; init-gherkin.el --- My emacs: Gherkin support configuration
;;;
;;; Commentary:
;;; Code:
(require-package 'feature-mode)
(add-to-list 'auto-mode-alist '("\.feature$" . feature-mode))

(provide 'init-gherkin)
;;; init-gherking.el ends here

;; Local variables:
;;    no-byte-compile: t
;; End:
