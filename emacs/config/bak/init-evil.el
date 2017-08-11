;;; init-evil.el --- .
;;; Commentary:
;;; Code:

(setq evil-emacs-state-cursor  '("red" box))
(setq evil-normal-state-cursor '("gray" box))
(setq evil-visual-state-cursor '("gray" box))
(setq evil-insert-state-cursor '("gray" bar))
(setq evil-motion-state-cursor '("gray" box))

;; prevent esc-key from translating to meta-key in terminal mode
(setq evil-esc-delay 0)



(define-key evil-normal-state-map (kbd "C-A") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C-S-A") 'evil-numbers/dec-at-pt)

(evil-ex-define-cmd "W" 'evil-write-all)
;(evil-ex-define-cmd "Tree" 'speedbar-get-focus)
;(evil-ex-define-cmd "linum" 'linum-mode)
(evil-ex-define-cmd "Align" 'align-regexp)


(defun al/yank-to-end-of-line ()
  "Yank to end of line."
  (interactive)
  (evil-yank (point) (point-at-eol)))
(define-key evil-normal-state-map (kbd "Y") 'al/yank-to-end-of-line)

(defun al/shift-right-visual ()
  "Shift right and restore visual selection."
  (interactive)
  (evil-shift-right (region-beginning) (region-end))
  (evil-normal-state)
  (evil-visual-restore))
(defun al/shift-left-visual ()
  "Shift left and restore visual selection."
  (interactive)
  (evil-shift-left (region-beginning) (region-end))
  (evil-normal-state)
  (evil-visual-restore))
(define-key evil-visual-state-map (kbd ">") 'al/shift-right-visual)
(define-key evil-visual-state-map (kbd "<") 'al/shift-left-visual)


;; Scrolling
(defun al/evil-scroll-down-other-window ()
  "In evil mode, scroll down other window."
  (interactive)
  (scroll-other-window))
(defun al/evil-scroll-up-other-window ()
  "In evil mode, scroll up other window."
  (interactive)
  (scroll-other-window '-))
(define-key evil-normal-state-map (kbd "C-S-d") 'al/evil-scroll-down-other-window)
(define-key evil-normal-state-map (kbd "C-S-u") 'al/evil-scroll-up-other-window)


;; Anzu improvments
(require-package 'evil-anzu)
(require 'evil-anzu)



(provide 'init-evil)
;;; init-evil.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
