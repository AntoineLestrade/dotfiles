;;; init.el --- Configuration entry point.
;; Copyright (c) 2015 Antoine Lestrade
;;
;; Author: Antoine Lestrade <antoine.lestrade@gmail.com>
;; Version 0.0.1

;;; Commentary
;;

;;; Code:

;; Define variables for some directories
(defvar current-config-dir (file-name-directory load-file-name))
(defvar loc-saves-dir (expand-file-name "savefiles" current-config-dir))
(defvar my-modules-dir (expand-file-name "config" current-config-dir))
(defvar loc-cache-dir (expand-file-name "cache" current-config-dir))

(add-to-list 'load-path my-modules-dir)

;; Reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)



;; Move config changes in another file
(setq custom-file (expand-file-name "custom.el" current-config-dir))
(if (file-exists-p (expand-file-name "custom.el" current-config-dir))
    (load (expand-file-name "custom.el" current-config-dir)))

(require 'init-packages)
(require 'init-custom)
(require 'init-ui)
(require 'init-core)

(require 'init-editor)

;; packages
(require 'init-ido)
(require 'init-company)

(require 'init-csharp)

(provide 'init)
;;; init.el ends here
