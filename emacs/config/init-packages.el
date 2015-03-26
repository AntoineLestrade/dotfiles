;;; init-packages.el --- Emacs config: packages list


(require 'cl)
(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

(setq package-user-dir (expand-file-name "elpa" current-config-dir))
(package-initialize)

(defvar my-packages-list
  '(anzu ;; Improve search
    browse-kill-ring ;; Improve kill ring
    diff-hl
    diminish ;; Diminished modes are minor modes with no modeline display
    easy-kill
    expand-region ;; Emacs extension to increase selected region by semantic units.
    operate-on-number
    projectile
    smartparens ;; Minor mode for Emacs that deals with parens pairs and tries to be smart about it.
    smartrep
    zenburn-theme ;; Theme
    ))

(defun my-check-packages-installed-p ()
  "Check if all packages are installed."
  (every #'package-installed-p my-packages-list))

(defun my-require-package (package)
  "Install PACKAGE unless already installed."
  (unless (memq package my-packages-list)
    (add-to-list 'my-packages-list package))
  (unless (package-installed-p package)
    (package-install package)))

(defun my-require-packages (packages)
  "Ensure PACKAGES are installed.
Missing packages are installed automatically."
  (mapc #'my-require-package packages))

(defun my-install-packages ()
  "Install all packages listed in `my-packages-list'."
  (unless (my-check-packages-installed-p)
    (message "%s" "Emacs is now refreshing its package database...")
    (package-refresh-contents)
    (message "%s" "done.")
    (my-require-packages my-packages-list)))

(my-install-packages)

(defmacro my-auto-install (extension package mode)
  "When file with EXTENSION is opened triggers auto-install of PACKAGE.
PACKAGE is installed only if not already present. The file is opened in MODE."
  `(add-to-list 'auto-mode-alist
                `(,extension . (lambda ()
                                 (unless (package-installed-p ',package)
                                   (package-install ',package))
                                 (,mode)))))

(defvar my-auto-install-alist
  '(("\\.hs\\'" haskell-mode haskell-mode)
    ("\\.rs\\'" rust-mode rust-mode)
    ))

;; build auto-install mappings
(mapc
 (lambda (entry)
   (let ((extension (car entry))
         (package (cadr entry))
         (mode (cadr (cdr entry))))
     (unless (package-installed-p package)
       (my-auto-install extension package mode))))
 my-auto-install-alist)

(provide 'init-packages)
;;; init-packages.el ends here
