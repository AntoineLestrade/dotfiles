;;; init-smartparens.el --- My emacs config: Initialization of smartparens package and options.
;;; Commentary:
;; Inspired (and copied) from https://github.com/thomasf/dotfiles-thomasf-emacs
;;; Code:
(use-package smartparens
  :ensure t
  ;;:pin "melpa-stable"
  :commands (smartparens-mode smartparens-global-mode turn-on-smartparens-mode
                              turn-off-smartparens-mode show-smartparens-mode
                              show-smartparens-global-mode
                              smartparens-global-strict-mode
                              smartparens-strict-mode
                              turn-on-smartparens-strit-mode)
  :diminish ""
  :init
  (progn
    (message "Initializing smartparens...")
    (setq
     sp-show-pair-delay 0.125
     sp-show-pair-from-inside t)
     (smartparens-global-mode t)
     (show-smartparens-global-mode t)
     ;;(hook-into-modes 'turn-on-smartparens-strict-mode my-lisp-mode-hook)
     )
  :config
  (progn
    (setq
     sp-ignore-modes-list '(calc-mode dired-mode ibuffer-mode
                                      minibuffer-inactive-mode sr-mode)
     sp-autoescape-string-quote nil)
    (sp-pair "'" nil :unless '(sp-point-after-word-p))

    (sp-with-modes '(emacs-lisp-mode inferior-emacs-lisp-mode
                                     lisp-interaction-mode scheme-mode
                                     lisp-mode eshell-mode slime-repl-mode
                                     clojure-mode common-lisp-mode)
      (sp-local-pair "'" nil :actions nil)
      (sp-local-pair "`" "'" :when '(sp-in-string-p)))
    (sp-with-modes '(tex-mode plain-tex-mode latex-mode)
      ;; match modes, yay. The :actions are provided automatically if
      ;; these pairs do not have global definition.
      (sp-local-pair "$" "$")
      (sp-local-pair "\\[" "\\]")
      (sp-local-pair "`" "'")
      )

  (sp-with-modes '(sgml-mode html-mode web-mode)
    (sp-local-pair "<" ">")
    (sp-local-tag "<" "<_>" "</_>" :transform 'sp-match-sgml-tags))

  (sp-with-modes '(markdown-mode gfm-mode rst-mode)
    (sp-local-pair "*" "*" :bind "C-*")
    (sp-local-tag "2" "**" "**")
    ;; (sp-local-tag "s" "```scheme" "```")
    (sp-local-tag "<" "<_>" "</_>" :transform 'sp-match-sgml-tags))
  (sp-with-modes '(markdown-mode gfm-mode)
    (sp-local-pair "#" "#" :actions '(wrap))
    (sp-local-pair "_" "_" :actions '(wrap))
    (sp-local-pair "*" "*" :actions '(wrap)))
  (sp-with-modes '(org-mode)
    (sp-local-pair "=" "=" :actions '(wrap))
    (sp-local-pair "/" "/" :actions '(wrap))
    (sp-local-pair "*" "*" :actions '(wrap)))

  (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)))

  ;;(progn
  ;;  (require 'smartparens-config)
  ;;  (setq sp-autoescapte-string-quote nil)
  ;;  (setq sp-autoinsert-quote-if-followed-by-closing-pair nil)
  ;;
  ;;  (sp-use-smartparens-bindings)
  ;;  (electric-pair-mode -1)
  ;;  (smartparens-global-mode t)
  ;;  (setq sp-show-pair-delay 0)
  ;;  (setq sp-show-pair-from-inside t)
  ;;  ;;; disable built-in parenthesis mode)
  ;;  (show-paren-mode -1)
  ;;  (show-smartparens-global-mode t))
  ;;:ensure t)

(provide 'init-smartparens)
;;; init-smartparens.el ends here
