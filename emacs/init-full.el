
(require-package 'wgrep) ;; Modify files directry in grep buffer
(require-package 'wgrep-ag)
;; TODO test it
(require-package 'project-local-variables)

(require 'init-ui)
(require 'init-dired)
(require 'init-isearch)
(require 'init-grep)
(require 'init-flycheck)

(require 'init-hippie-expand)
;;(require 'init-company)
(require 'init-windows)

(require 'init-vc)
(require 'init-git)

(require 'init-web)
(require 'init-javascript)
(require 'init-org)
;;(require 'init-xml)

(require 'init-csharp)

(require 'init-helm)

;; Allow access from emacsclient
(require 'server)
(unless (server-running-p)
  (server-start))

;; Reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)
