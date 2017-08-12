;;; init.el --- Configuration entry point.
;; Copyright (c) 2015-2017 Antoine Lestrade
;;
;; Author: Antoine Lestrade <antoine.lestrade@gmail.com>
;; Version 0.0.1

;;; Commentary:
;;

;;; Code:
;(package-initialize)

;; Define variables for some directories
(defconst loc-config-dir (file-name-directory load-file-name)
  "Directory containing the currently loaded init.el file.")
;;(defvar loc-saves-dir (expand-file-name "savefiles" loc-config-dir))
(defconst loc-cache-dir (expand-file-name "cache" loc-config-dir))
(defconst loc-modules-dir (expand-file-name "config" loc-config-dir)
  "Directory containing extra initialization scripts.")
(add-to-list 'load-path loc-modules-dir)
(when (file-exists-p (expand-file-name "init-local.el" loc-config-dir))
  (load (expand-file-name "init-local.el" loc-config-dir)))

(require 'init-benchmarking)

;; Bootstrap config
(when (bound-and-true-p loc-enable-packages)
  (require 'init-packages)
  (require-package 'diminish))

(require 'init-editor)

(when (bound-and-true-p loc-enable-ahk)
  (require 'init-ahk))
(when (bound-and-true-p loc-enable-vcs)
  (require 'init-vcs))
(when (bound-and-true-p loc-enable-company)
  (require 'init-company))
(when (bound-and-true-p loc-enable-flycheck)
  (require 'init-flycheck))
(when (bound-and-true-p loc-enable-evil)
  (require 'init-evil))
(when (bound-and-true-p loc-enable-helm)
  (require 'init-helm))
(when (bound-and-true-p loc-enable-powershell)
  (require 'init-powershell))
(when (bound-and-true-p loc-enable-gherkin)
  (require 'init-gherkin))
(require 'init-ibuffer)
(when (bound-and-true-p loc-enable-projectile)
  (require 'init-projectile))

(when (bound-and-true-p loc-enable-rust)
  (require 'init-rust))


;; Move config changes in another file
(setq custom-file (expand-file-name "custom.el" loc-config-dir))
(if (file-exists-p custom-file)
    (load custom-file))

(add-hook 'after-init-hook
	  (lambda ()
	    (message "init completed in %.2fms"
		     (loc-time-substract-millis after-init-time before-init-time))))
(provide 'init)
;;; init.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
