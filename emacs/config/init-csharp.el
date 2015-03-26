(my-require-packages '(csharp-mode))

(require 'csharp-mode)
(require 'speedbar)
(speedbar-add-supported-extension ".cs")
(add-to-list 'speedbar-fetch-etags-parse-list
             '("\\.cs" . speedbar-parse-c-or-c++tag))

(provide 'init-csharp)
