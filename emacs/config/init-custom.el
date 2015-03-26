;;; init-custom.el --- Emacs configuration: Customizable variables

(defgroup al-config nil
  "Group for init files"
  :prefix "al-"
  :group 'convenience)

(defcustom al-enable-omnisharp nil
  "Non-nil values enable Omnisharp package"
  :type 'boolean
  :group 'al-config)

(provide 'init-custom)
;;; init-custom.el ends here
