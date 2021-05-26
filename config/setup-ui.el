;;; setup-ui.el --- all things GUI
;;; Commentary:
;;; Code:
(require 'use-package)

(use-package all-the-icons)

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-moonlight t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  ;; (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config)
  
  ;; Corrects (and improves) org-mode's native fontification.
  ;; (doom-themes-org-config)
  )

(load-theme 'doom-moonlight t)

(use-package golden-ratio
  :defer t
  :diminish
  :custom
  (golden-ratio-mode t))

(provide 'setup-ui)
;;; setup-ui.el ends here
