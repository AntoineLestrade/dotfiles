(defgroup loc nil
  "Group for init files"
  :group 'local)

(defcustom loc-cache-dir (concat user-emacs-directory ".cache/")
  "Cache directory"
  :group 'loc)

(add-to-list 'load-path (expand-file-name "config" (file-name-directory load-file-name)))

(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries, like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

;; use-package + Manage packages
(if (not (package-installed-p 'use-package))
    (progn
      (package-refresh-contents)
      (package-install 'use-package)))
;; https://github.com/jwiegley/use-package
(require 'use-package)

;; remap windows key
(setq w32-pass-lwindow-key-to-system nil
      w32-lwindow-modifier 'super)

(require 'init-core)
(require 'init-ui)
(require 'init-helm)
(require 'init-git)
(require 'init-web)
(require 'init-ahk)
(require 'init-csharp)
