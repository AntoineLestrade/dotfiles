;;; init-editor --- Core editor config
;;   Copyright (c) 2017 Antoine Lestrade
;;
;;   Author: Antoine Lestrade <antoine.lestrade@gmail.com>
;;   Version 0.0.1
;;
;;; Commentary:
;;    Customize core settings of Emacs
;;; Code:

(setq visible-bell t)

;; modeline settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)


;; smart tab behaviour: first time indent, next time complete:
(setq tab-always-indent 'complete)

;; Store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))


;; disable menu, toolbar
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(blink-cursor-mode -1)

;; Uniquify settings
(require 'uniquify)

(setq uniquify-buffer-name-style 'reverse) ;; maybe 'forward
(setq uniquify-separator " • ")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

(when (and
       (bound-and-true-p loc-enable-packages)
       (maybe-require-package 'page-break-lines))
  (global-page-break-lines-mode 1)
  (diminish 'page-break-lines-mode))

;; Development features
(require 'which-func)
(which-function-mode 1)
(add-hook 'prog-mode-hook
	  '(lambda()
	     (setq show-trailing-whitespace t)))

(when (bound-and-true-p loc-enable-packages)
  (when (maybe-require-package 'which-key)
    (which-key-mode 1)))

;; Key binding for useful highlihg functions
;; Maybe use built-in `hi-lock' if some fonctionnalities are not used
;; [f3] is for macros: do not use it
;;;(when (and (bound-and-true-p loc-enable-packages)
;;;	   (maybe-require-package 'highlight-symbol))
;;;  (require 'highlight-symbol)
;;;  (global-set-key [(control f3)] 'highlight-symbol-at-point)
;;;  (global-set-key [f3] 'highlight-symbol-next)
;;;  (global-set-key [(shift f3)] 'highligh-symbol-prev)
;;;  (global-set-key [(meta f3)] 'highlight-symbol-query-replace))

;; Themes
(when (and (bound-and-true-p loc-enable-packages)
	   (maybe-require-package 'ample-theme))
  ;;(custom-enabled-themes 'ample-flat)
  (add-hook 'after-init-hook (lambda() (load-theme 'ample-flat)))
  )


(provide 'init-editor)
;;; init-editor.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
