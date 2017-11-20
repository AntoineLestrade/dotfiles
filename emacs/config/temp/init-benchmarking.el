;;; init-benchmarking.el --- Module to benchmark emacs initialization steps
;;; Commentary:
;;;  Benchmark time consumed by each 'require' call
;;;  This benchmark is stored in the 'myconfig/require-times' variable
;;; Code:

(defun loc-time-substract-millis (b a)
  "Internal function to get time ellapsed between B and A in milliseconds."
  (* 1000.0 (float-time (time-subtract b a))))

(defvar loc-require-times nil
  "A list of (FEATURE . LOAD-DURATION).
LOAD-DURATION is the time taken in milliseconds to load FEATURE.")

(defadvice require
    (around build-require-times (feature &optional filename noerror) activate)
  "Note in loc-require-times' the time tale to require each feature."
  (let* ((already-loaded (memq feature features))
         (require-start-time (and (not already-loaded) (current-time))))
    (prog1
        ad-do-it
      (when (and (not already-loaded) (memq feature features))
        (add-to-list 'loc-require-times
                     (cons feature
                           (loc-time-substract-millis (current-time)
						     require-start-time))
                     t)))))
;; (defadvice require (around dotemacs activate)
;;   (let ((elapsed)
;; 	(loaded (memq feature features))
;; 	(start (current-time)))
;;     (prog1
;; 	ad-do-it
;;       (unless loaded
;; 	(with-current-buffer (get-buffer-create "*Require Times*")
;; 	  (when (= 0 (buffer-size))
;; 	    (insert "| feature | timestamp | elapsed |\n")
;; 	    (insert "|---------+-----------+---------+\n"))
;; 	  (goto-char (point-max))
;; 	  (setq elapsed (float-time (time-subtract (current-time) start)))
;; 	  (insert (format "| %s | %s | %f |\n"
;; 			  feature
;; 			  (format-time-string "%Y-%m-%d %H:%M:%S.%3N" (current-time))
;; 			  elapsed)))))))
(provide 'init-benchmarking)
;;; init-benchmarking ends here
