;;; init-ivy.el --- ivy, counsel, swiper setup

;;; Commentary:

;; -> https://dev.to/deciduously/how-i-emacs-and-so-can-you-packages-m9p
;; http://oremacs.com/swiper/#installation
;; https://sam217pa.github.io/2016/08/30/how-to-make-your-own-spacemacs/#fnref:3
;; https://writequit.org/denver-emacs/presentations/2017-04-11-ivy.html#fnr.2

;;; Code:
(require 'use-package)

(use-package ivy
  :diminish (ivy-mode . "")
  :init (ivy-mode 1) ; globally at startup
  :bind
  ("C-c C-r" . ivy-resume)
  ("C-x B" . ivy-switch-buffer-other-window)
  :custom
  (ivy-count-format "(%d/%d) ")
  (ivy-use-virtual-buffers t)
  (ivy-display-style 'fancy)
  (ivy-height 20)
  (ivy-re-builders-alist
   '((t . ivy--regex-fuzzy)))
  )

(use-package swiper
  :diminish
  :bind
  ("H-s" . swiper)
  ("C-c S" . swiper)
  ("C-x C-r" . counsel-recentf)
  ("s-E" . counsel-colors-emacs)
  ("s-W" . counsel-colors-web)
  :custom
  (colir-compose-method 'colir-compose-soft-light))

;; Override the basic Emacs commands
;; (use-package counsel
;;   :bind* ; load when pressed
;;   (("M-x"     . counsel-M-x)
;;    ("C-s"     . swiper)
;;    ("C-x C-f" . counsel-find-file)
;;    ("C-x C-r" . counsel-recentf)  ; search for recently edited
;;    ("C-c g"   . counsel-git)      ; search for files in git repo
;;    ("C-c j"   . counsel-git-grep) ; search for regexp in git repo
;;    ("C-c /"   . counsel-ag)       ; Use ag for regexp
;;    ("C-x l"   . counsel-locate)
;;    ("<f1> f"  . counsel-describe-function)
;;    ("<f1> v"  . counsel-describe-variable)
;;    ("<f1> l"  . counsel-find-library)
;;    ("<f2> i"  . counsel-info-lookup-symbol)
;;    ("<f2> u"  . counsel-unicode-char)
;;    ("C-c C-r" . ivy-resume)))     ; Resume last Ivy-based completion

(use-package counsel
  :after ivy
  :bind
  ("M-x"     . counsel-M-x)
  ("H-y"     . counsel-yank-pop)
  ("C-x C-f" . counsel-find-file)
  ("C-c a"   . counsel-rg)
  ("H-h f"  . counsel-describe-function)
  ("H-h v"  . counsel-describe-variable)
  ("H-h l"  . counsel-find-library)
  ("H-h i"  . counsel-info-lookup-symbol)
  ("H-u"    . counsel-unicode-char)
  :custom
  (counsel-find-file-at-point t)
  (enable-recursive-minibuffers t)
  :config
  (counsel-mode))

(use-package ivy-rich
  :after ivy
  :custom
  (ivy-virtual-abbreviate 'full)
  (ivy-rich-switch-buffer-align-virtual-buffer t)
  ;; :config
  :commands ivy-rich-mode
  )

;;(use-package ivy-hydra
;;  :after ivy)

(use-package smex
  ;; :config (smex-initialize))
  :commands smex-initialize)

;; (use-package ivy-view
;;   :bind
;;   ("H-[" . ivy-push-view)
;;   ("H-]" . ivy-pop-view)
;;   ("H-b" . ivy-switch-buffer)
;;   ;; :config
;;   :commands
;;   ivy-use-virtual-buffers)

(use-package all-the-icons-ivy
  :after ivy-mode)

(use-package all-the-icons-ivy-rich
  :after ivy-rich
  :commands all-the-icons-ivy-rich-mode)
  ;; :init (all-the-icons-ivy-rich-mode 1))

(provide 'init-ivy)
;;; init-ivy.el ends here
