;;; init-xml.el --- .
;;; Commentary:
;;; Code:
(add-to-list 'auto-mode-alist ("\\.xml\\'" . nxml-mode))
(add-to-list 'auto-mode-alist ("\\.xsd\\'" . nxml-mode))
(add-to-list 'auto-mode-alist ("\\.xslt\\'" . nxml-mode))
(add-to-list 'auto-mode-alist ("\\.svg\\'" . nxml-mode))
(add-to-list 'auto-mode-alist ("\\.rss\\'" . nxml-mode))

(setq magic-mode-alist (cons '("<\\?xml " . nxml-mode) magic-mode-alist))
;; auto-generate closing tag when typing /
(setq nxml-slash-auto-complete-flag t)

(provide 'init-xml)
;;; init-xml.el ends here
