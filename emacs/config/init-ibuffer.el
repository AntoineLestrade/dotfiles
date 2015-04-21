;;; init-ibuffer.el ---
;;; Commentary:
;;; Code:


;;(require-package 'fullframe)
;;(with-eval-after-load 'ibuffer
;;  (fullframe ibuffer ibuffer-quit))

(require-package 'ibuffer-vc)

(defun ibuffer-set-up-preferred-filters ()
  (ibuffer-vc-set-filter-groups-by-vc-root)
  (unless (eq ibuffer-sorting-mode 'filename/process)
    (ibuffer-do-sort-by-filename/process)))

(add-hook 'ibuffer-hook 'ibuffer-set-up-preferred-filters)
(setq-default ibuffer-show-empty-filter-groups nil)


(with-eval-after-load 'ibuffer
    (define-ibuffer-column size-h
      (:name "Size" :inline t)
      (cond
       ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
       ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
       (t (format "%8d" (buffer-size))))))

(with-eval-after-load 'ibuffer
  (require 'ibuffer-vc))

(global-set-key (kbd "C-x C-b") 'ibuffer)

(provide 'init-ibuffer)
;;; init-ibuffer.el ends here
