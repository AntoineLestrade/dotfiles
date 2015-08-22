;;; init.el --- Configuration entry point.
;; Copyright (c) 2015 Antoine Lestrade
;;
;; Author: Antoine Lestrade <antoine.lestrade@gmail.com>
;; Version 0.0.1

;;; Commentary:
;;

;;; Code:

;; Define variables for some directories

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(defvar current-config-dir (file-name-directory load-file-name)
  "Directory containing the currently loaded init.el file."
  )
(defvar loc-saves-dir (expand-file-name "savefiles" current-config-dir))
(defvar my-modules-dir (expand-file-name "config" current-config-dir)
  "Directory containing extra initialization scripts.")
(defvar loc-cache-dir (expand-file-name "cache" current-config-dir))

(add-to-list 'load-path my-modules-dir)

(require 'init-benchmarking)

;; Bootstrap config
(require 'init-packages)

(require-package 'wgrep) ;; Modify files directry in grep buffer
(require-package 'wgrep-ag)
;; TODO test it
(require-package 'project-local-variables)
(require-package 'diminish)


(require 'init-themes)
(require 'init-ui)
(require 'init-dired)
(require 'init-isearch)
(require 'init-grep)
(require 'init-uniquify)
(require 'init-ibuffer)
(require 'init-flycheck)

(require 'init-ido)
(require 'init-hippie-expand)
(require 'init-company)
(require 'init-windows)

(require 'init-editor)

(require 'init-vc)
(require 'init-git)

(require 'init-web)
(require 'init-javascript)
(require 'init-org)
;;(require 'init-xml)

(require 'init-ahk)
(require 'init-csharp)
(require 'init-powershell)
(require 'init-rust)

(require 'init-programming-misc)

;; Allow access from emacsclient
(require 'server)
(unless (server-running-p)
  (server-start))

;; Reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)



;; Move config changes in another file
(setq custom-file (expand-file-name "custom.el" current-config-dir))
(if (file-exists-p (expand-file-name "custom.el" current-config-dir))
    (load (expand-file-name "custom.el" current-config-dir)))

(add-hook 'after-init-hook
          (lambda ()
            (message "init completed in %.2fms"
                     (myconfig/time-substract-millis after-init-time before-init-time))))

(provide 'init)
;;; init.el ends here
