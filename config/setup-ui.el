;;; setup-ui.el --- all things GUI
;;; Commentary:
;;; Code:
(require 'use-package)

(use-package all-the-icons)

(use-package all-the-icons-dired
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(use-package all-the-icons-ibuffer)

;; (use-package vscode-dark-plus-theme
;;   :config
;;   (load-theme 'vscode-dark-plus t))

;; (use-package vscode-icon
;;   :ensure t
;;   :commands (vscode-icon-for-file))

;; (use-package zerodark-theme
;;   :init (load-theme 'zerodark t)
;;   :custom
;;   (zerodark-setup-modeline-format)
;;   )

(use-package doom-themes
  :after all-the-icons
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  ;; (doom-themes-neotree-config)
  ;; or for treemacs users
  (doom-themes-treemacs-config)
  (doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  
  
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)
  )

(load-theme 'doom-moonlight t) ;; this is needed altough it shouldn't

(use-package highlight-escape-sequences
  :hook (prog-mode . hes-mode))

(use-package golden-ratio
  :defer t
  :diminish
  :custom
  (golden-ratio-mode t))

(provide 'setup-ui)
;;; setup-ui.el ends here
