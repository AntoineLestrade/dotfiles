;;; init-editor.el --- Emacs config: enhanced core edition.

;;; Code:

; Death to the tabs!  However, tabs historically indent to the next
;; 8-character offset; specifying anything else will cause *mass*
;; confusion, as it will change the appearance of every existing file.
;; In some cases (python), even worse -- it will change the semantics
;; (meaning) of the program.
;;
;; Emacs modes typically provide a standard means to change the
;; indentation width -- eg. c-basic-offset: use that to adjust your
;; personal indentation width, while maintaining the style (and
;; meaning) of any files you load.
(setq-default indent-tabs-mode nil)   ;; don't use tabs to indent
(setq-default tab-width 8)            ;; but maintain correct appearance

(setq require-final-newline t)

;; When a selection exists, any key should delete it
(delete-selection-mode t)

(global-visual-line-mode t)



;; Store all backup and autosave files in cache dir
;;(setq backup-directory-alist
;;      `((".*" . ,(concat loc-cache-dir "backups")))
;;      auto-save-file-name-transform
;;      `((".*" ,(concat loc-cache-dir "backups") t))
;;      auto-save-list-file-prefix
;;      (concat loc-cache-dir "auto-save-list/save-"))


;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(global-auto-revert-mode t)

;; smart tab behaviour: indent or complete
(setq tab-always-indent 'complete)

;; smart pairing
(require 'smartparens-config)
(setq sp-base-key-bindings 'paredit)
(setq sp-autoskip-closing-pair 'always)
(setq sp-hybrid-kill-entire-symbol nil)
(sp-use-paredit-bindings)
(show-smartparens-global-mode +1)

(require 'diminish)

;; Maybe change uniquify settings
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

;; saveplace
(require 'saveplace)
(setq save-place-file (expand-file-name "saveplace" loc-saves-dir))
(setq-default save-place t)

;; recent files
(require 'recentf)
(setq recentf-save-file (expand-file-name "recentf" loc-saves-dir)
      recentf-max-saved-items 500
      recentf-max-menu-items 15
      recentf-auto-cleanup 'never)

(add-to-list 'recentf-exclude "COMMIT_EDITMSG\\'")
(recentf-mode +1)

;; use shift + arrow keys to switch between visible buffers
(require 'windmove)
(windmove-default-keybindings)

(global-hl-line-mode +1)

;; add the ability to cut the current line, without marking it
(defadvice kill-region (before smart-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

(set-default 'imenu-auto-rescan t)

(add-hook 'prog-mode-hook '(lambda()
                             (setq show-trailing-whitespace t)))

;; enable narrowing commands
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;; enable change region case commands
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; enable erase-buffer command
(put 'erase-buffer 'disabled nil)

(require 'expand-region)

(require 'bookmark)
(setq bookmark-default-file (expand-file-name "bookmarks" loc-saves-dir)
      bookmark-save-flag 1)

(require 'projectile)
(setq projectile-cache-file (expand-file-name "projectile-cache" loc-saves-dir))
(projectile-global-mode t)

;; anzu-mode enhances isearch & query-replace by showing total matches and current match position
(require 'anzu)
(diminish 'anzu-mode)
(global-anzu-mode)
(global-set-key (kbd "M-ù") 'anzu-query-replace)
(global-set-key (kbd "C-M-ù") 'anzu-query-replace-regexp)

;; dired - reuse current buffer by pressing 'a'
(put 'dired-find-alternate-file 'disabled nil)

;; always delete and copy recursively
(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'always)

;; if there is a dired buffer displayed in the next window, use its
;; current subdir, instead of the current subdir of this dired buffer
(setq dired-dwim-target t)

(add-hook 'dired-load-hook
          (lambda ()
            (load "dired-x")
            ))

(require 'ediff)
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)


;; smarter kill-ring navigation
(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)
(global-set-key (kbd "M-y") 'browse-kill-ring)

(add-hook 'text-mode-hook 'abbrev-mode)

(require 'whitespace)
(setq whitespace-line-column 80)
(setq whitespace-style '(face tabs empty trailing lines-tail))

(require 'eshell)
(setq eshell-directory-name (expand-file-name "eshell" loc-saves-dir))

(setq semanticdb-default-save-directory
      (expand-file-name "semanticdb" loc-saves-dir))

;; Compilation from Emacs
(defun al-colorize-compilation-buffer ()
  "Colorize a compilation mode buffer."
  (interactive)
  ;; we don't want to mess with child modes such as grep-mode, ack, ag, etc
  (when (eq major-mode 'compilation-mode)
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (point-min) (point-max)))))

(require 'compile)
(setq compilation-ask-about-save nil ; Just save before compiling
      compilation-always-kill t      ; Just kill old compile processes
                                        ; before starting new one
      compilation-scroll-output 'first-error ; Automatically scroll to first
                                        ; error
      )

;; http://stackoverflow.com/a/3072831/355252
(add-hook 'compilation-filter-hook #'al-colorize-compilation-buffer)



(winner-mode +1)

;; diff-hl
(global-diff-hl-mode +1)
(add-hook 'dired-mode-hook 'diff-hl-dired-mode)

;; easy-kill
(global-set-key [remap kill-ring-save] 'easy-kill)
(global-set-key [remap mark-sexp] 'easy-mark)

;; operate-on-number
(require 'operate-on-number)
(require 'smartrep)

(smartrep-define-key global-map "C-c ."
  '(("+" . apply-operation-to-number-at-point)
    ("-" . apply-operation-to-number-at-point)
    ("*" . apply-operation-to-number-at-point)
    ("/" . apply-operation-to-number-at-point)
    ("\\" . apply-operation-to-number-at-point)
    ("^" . apply-operation-to-number-at-point)
    ("<" . apply-operation-to-number-at-point)
    (">" . apply-operation-to-number-at-point)
    ("#" . apply-operation-to-number-at-point)
    ("%" . apply-operation-to-number-at-point)
    ("'" . apply-operation-to-number-at-point)))

(defadvice server-visit-files (before parse-number-in-lines (files proc &optional nowait) activate)
  "Open file with emacsclient with cursors positioned on requested line.
Most of console-based utilities prints filename in format
'filename:linenumber'. So you may wish to open filename in that format.
Just call:

  emacsclient filename:linenumber

and file 'filename' will be opened and cursor set on line 'linenumber'"
  (ad-set-arg 0
              (mapcar (lambda (fn)
                        (let ((name (car fn)))
                          (if (string-match "^\\(.*?\\):\\([0-9]+\\)\\(?::\\([0-9]+\\)\\)?$" name)
                              (cons
                               (match-string 1 name)
                               (cons (string-to-number (match-string 2 name))
                                     (string-to-number (or (match-string 3 name) ""))))
                            fn))) files)))

(provide 'init-editor)

;;; init-editor.el ends here
