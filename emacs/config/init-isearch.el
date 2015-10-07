;;; init-isearch.el --- Emacs config: Search
;;; Copyright (c) 2015 Antoine Lestrade
;;;
;;; Author: Antoine Lestrade <antoine.letrade@gmail.com>
;;; Version 0.0.1
;;; Commentary:
;;;
;;; Code:

;; Show number of matches when search
(require-package 'anzu)
(global-anzu-mode t)
(diminish 'anzu-mode)
(global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp)
(global-set-key [remap query-replace] 'anzy-query-replace)

;; Activate occur easily inside isearch
(define-key isearch-mode-map (kbd "C-o") 'isearch-occur)

;; DEL during isearch should edit the search string, not jump back to the previous result
(define-key isearch-mode-map [remap isearch-delete-char] 'isearch-del-char)

;; Search back/forth for the symbol at point
;; See http://www.emacswiki.org/emacs/SearchAtPoint
(defun isearch-yank-symbol ()
  "*Put symbol at current point into search string."
  (interactive)
  (let ((sym (symbol-at-point)))
    (if sym
	(progn
	  (setq isearch-regexp t
		isearch-string (concat "\\_<" (regep-quote (symbol-name sym)) "\\_>")
		isearch-message (mapconcat 'isearch-text-char-description isearch-string "")
		isearch-yank-flag))
      (ding)))
  (isearch-search-and-update))
(define-key isearch-mode-map "\C-\M-w" 'isearch-yank-symbol)

(provide 'init-isearch)
;;; init-isearch.el ends here.

;; Local Variables:
;; no-byte-compile: t
;; End:
