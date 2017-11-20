;;; init-ibuffer.el --- Cusomize Emacs ibuffer
;;; Commentary:
;;; Code:

(when (and (bound-and-true-p loc-enable-packages)
	   (maybe-require-package 'ibuffer-vc))
  (add-hook 'ibuffer-hook
	    '(lambda ()
	       (ibuffer-vc-set-filter-groups-by-vc-root))))

(add-hook 'ibuffer-hook
	  '(lambda()
	     (unless (eq ibuffer-sorting-mode 'filename/process)
	       (ibuffer-do-sort-by-filename/process))))

(with-eval-after-load 'ibuffer
    (define-ibuffer-column size-h
      (:name "Size" :inline t)
      (cond
       ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
       ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
       (t (format "%8d" (buffer-size))))))

;; Use ibuffer instead of list-buffers
(global-set-key (kbd "C-x C-b") 'ibuffer)

(provide 'init-ibuffer)
;;; init-ibuffer.el ends here
