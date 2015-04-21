;;; init-packages.el --- Emacs config: packages list

(require 'package)



;;; Standard package repositories

;;(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

;; We include the org repository for completeness, but don't normally
;; use it.
;;(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))


;;; Also use Melpa for most packages
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))



;; If gpg cannot be found, signature checking will fail, so we
;; conditionally enable it according to whether gpg is available. We
;; re-run this check once $PATH has been configured
(defun sanityinc/package-maybe-enable-signatures ()
  (setq package-check-signature (when (executable-find "gpg") 'allow-unsigned)))

(sanityinc/package-maybe-enable-signatures)
(with-eval-after-load 'init-exec-path
  (sanityinc/package-maybe-enable-signatures))



;;; On-demand installation of packages

(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (package-install package)
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))


(defun maybe-require-package (package &optional min-version no-refresh)
  "Try to install PACKAGE, and return non-nil if successful.
In the event of failure, return nil and print a warning message.
Optionally require MIN-VERSION.  If NO-REFRESH is non-nil, the
available package lists will not be re-downloaded in order to
locate PACKAGE."
  (condition-case err
      (require-package package min-version no-refresh)
    (error
     (message "Couldn't install package `%s': %S" package err)
     nil)))


;;; Fire up package.el

(setq package-enable-at-startup nil)
(package-initialize)


;;(add-to-list 'package-archives
;;             '("melpa" . "http://melpa.org/packages/"))
;;(add-to-list 'package-archives
;;             '("melpa-stable" . "http://stable.melpa.org/packages/"))
;;
;;(defun require-package (package &optional min-version no-refresh)
;;  "Install given PACKAGE, optionally requiring MIN-VERSION.
;;If NO-REFRESH is non-nil, the available package lists will not be
;;re-downloaded in order to locale PACKAGE."
;;  (if (package-installed-p package min-version)
;;      t
;;    (if (or (assoc package package-archive-contents) no-refresh)
;;        (package-install package)
;;      (progn
;;        (package-refresh-contents)
;;        (require-package package min-version t)))))
;;
;;
;;;; TODO check this
;;(setq package-enable-at-startup nil)
;;(package-initialize)
;;
;; TODO: test this
(require-package 'fullframe)
(fullframe list-packages quit-window)

(require-package 'cl-lib)
(require 'cl-lib)







;;(setq package-user-dir (expand-file-name "elpa" current-config-dir))
;;(package-initialize)
;;
;;(defvar my-packages-list
;;  '(

;;    easy-kill
;;    expand-region ;; Emacs extension to increase selected region by semantic units.
;;    operate-on-number
;;    powershell
;;;;    projectile
;;    smartrep
;;    toml-mode
;;    ))
;;
;;(defun my-require-package (package)
;;  "Install PACKAGE unless already installed."
;;  (unless (memq package my-packages-list)
;;    (add-to-list 'my-packages-list package))
;;  (unless (package-installed-p package)
;;    (package-install package)))
;;
;;(defun my-require-packages (packages)
;;  "Ensure PACKAGES are installed.
;;Missing packages are installed automatically."
;;  (mapc #'my-require-package packages))
;;
;;
;;(defun my-check-packages-installed-p ()
;;  "Check if all packages are installed."
;;  (every #'package-installed-p my-packages-list))
;;
;;(defun my-install-packages ()
;;  "Install all packages listed in `my-packages-list'."
;;  (unless (my-check-packages-installed-p)
;;    (message "%s" "Emacs is now refreshing its package database...")
;;    (package-refresh-contents)
;;    (message "%s" "done.")
;;    (my-require-packages my-packages-list)))
;;
;;(my-install-packages)
;;
;;(defmacro my-auto-install (extension package mode)
;;  "When file with EXTENSION is opened triggers auto-install of PACKAGE.
;;PACKAGE is installed only if not already present. The file is opened in MODE."
;;  `(add-to-list 'auto-mode-alist
;;                `(,extension . (lambda ()
;;                                 (unless (package-installed-p ',package)
;;                                   (package-install ',package))
;;                                 (,mode)))))
;;
;;(defvar my-auto-install-alist
;;  '(("\\.hs\\'" haskell-mode haskell-mode)
;;    ("\\.rs\\'" rust-mode rust-mode)
;;    ))
;;
;;;; build auto-install mappings
;;(mapc
;; (lambda (entry)
;;   (let ((extension (car entry))
;;         (package (cadr entry))
;;         (mode (cadr (cdr entry))))
;;     (unless (package-installed-p package)
;;       (my-auto-install extension package mode))))
;; my-auto-install-alist)
;;
(provide 'init-packages)
;;; init-packages.el ends here
