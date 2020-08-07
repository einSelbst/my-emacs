;;; setup-ui.el --- all things GUI
;;; Commentary:
;;; Code:
(require 'use-package)

(use-package all-the-icons)

(use-package zerodark-theme
  :init (load-theme 'zerodark t)
  :custom
  (zerodark-setup-modeline-format)
  )

(use-package golden-ratio
  :defer t
  :diminish
  :custom
  (golden-ratio-mode t))

(provide 'setup-ui)
;;; setup-ui.el ends here
