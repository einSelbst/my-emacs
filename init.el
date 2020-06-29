;;; package init - Summary,
;;; Commentary:
;; einSelbst Emacs init file
;; https://github.com/einSelbst/my-emacs

;; set defaults
;; https://github.com/angrybacon/dotemacs/blob/master/dotemacs.org#use-better-defaults

(setq-default
 ;; ad-redefinition-action 'accept              ; Silence warnings for redefinition
 ;; auto-window-vscroll nil                     ; Lighten vertical scroll
 ;; confirm-kill-emacs 'yes-or-no-p             ; Confirm before exiting Emacs
 ;; cursor-in-non-selected-windows nil          ; Hide the cursor in inactive windows
 delete-by-moving-to-trash t                 ; Delete files to trash
 ;; display-time-default-load-average nil       ; Don't display load average
 ;; display-time-format "%H:%M"                 ; Format the time string
 fill-column 80                              ; Set width for automatic line breaks
 help-window-select t                        ; Focus new help windows when opened
 indent-tabs-mode nil                        ; Stop using tabs to indent
 inhibit-startup-screen t                    ; Disable start-up screen
 initial-scratch-message ""                  ; Empty the initial *scratch* buffer
 mouse-yank-at-point t                       ; Yank at point rather than pointer
 ;; ns-use-srgb-colorspace nil                  ; Don't use sRGB colors
 recenter-positions '(5 top bottom)          ; Set re-centering positions
 scroll-conservatively most-positive-fixnum  ; Always scroll by one line
 scroll-margin 10                            ; Add a margin when scrolling vertically
 select-enable-clipboard t                   ; Merge system's and Emacs' clipboard
 sentence-end-double-space nil               ; End a sentence after a dot and a space
 show-trailing-whitespace nil                ; Display trailing whitespaces
 tab-width 4                                 ; Set width for tabs
 uniquify-buffer-name-style 'forward         ; Uniquify buffer names
 visible-bell t                              ; Do not emit noise
 window-combination-resize t                 ; Resize windows proportionally
 x-stretch-cursor t                          ; Stretch cursor to the glyph width

 custom-file (expand-file-name "custom.el" user-emacs-directory))  ; load dedicated custom.el
;; https://github.com/angrybacon/dotemacs/blob/master/dotemacs.org#load-customel
(when (file-exists-p custom-file)
  (load custom-file))

(cd "~/")                                    ; Move to the user directory
(delete-selection-mode 1)                    ; Replace region when inserting text
(fset 'yes-or-no-p 'y-or-n-p)                ; Replace yes/no prompts with y/n
(global-auto-revert-mode 1)                  ; Always show the most recent version of a file
(global-hl-line-mode 1)                      ; Highlight current line
(global-subword-mode 1)                      ; Iterate through CamelCase words
;; (global-visual-line-moode 1)                 ; Act on (wrapped) visual, not logical lines
;; (global-highlight-changes-mode 1)
(mouse-avoidance-mode 'banish)               ; Avoid collision of mouse with point
(put 'downcase-region 'disabled nil)         ; Enable downcase-region
(put 'upcase-region 'disabled nil)           ; Enable upcase-region
(set-default-coding-systems 'utf-8)          ; Default to utf-8 encoding
(setq case-fold-search t)                    ; case insensitive search

;; fix dired
(when (eq system-type 'darwin)
  (setq dired-use-ls-dired nil))

;; https://emacs.stackexchange.com/a/437/16535
(defun display-startup-echo-area-message ()
  (message "Wheater you think you can do it or not, you'll always be right!"))

;; https://github.com/plexus/.emacs.d/blob/master/init.d/setup-emacs.el
;; Save all backup file in this directory
;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

(setq backup-by-copying t      ; don't clobber symlinks
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)       ; use versioned backups

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

;; Do save #..# files
(setq auto-save-default t               ; auto-save every buffer that visits a file
      auto-save-timeout 20              ; number of seconds idle time before auto-save (default: 30)
      auto-save-interval 200            ; number of keystrokes between auto-saves (default: 300)
      auto-save-visited-mode t
      delete-auto-save-files t
      create-lockfiles nil
      auto-save-file-name-transforms
      `((".*" ,(expand-file-name (concat user-emacs-directory "autosave")) t)))

;; setup straight.el package manager
;; https://github.com/raxod502/straight.el

(setq
 ;; load-prefer-newer t
 ;; straight-repository-branch "develop"
 ;; straight-check-for-modifications '(find-when-checking)
 straight-use-package-by-default t
 straight-cache-autoloads t
 ;; straight-treat-as-init t
 use-package-always-defer t
 use-package-always-ensure t
 ;; use-package-expand-minimally t
 use-package-verbose t
 use-package-compute-statistics t
 ;; debug-on-error t
 )

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'esup)

;; setup use-package
(straight-use-package 'use-package)


;; Use same paths as user environment on macOS
(use-package exec-path-from-shell
  :if (eq system-type 'darwin)
  :custom
  (exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-variables '("PATH" "MANPATH"))
  :init
  (exec-path-from-shell-initialize))

(use-package async
  :init
  (dired-async-mode
   async-bytecomp-package-mode)
  )

;; remember open buffers
;; https://github.com/angrybacon/dotemacs/blob/master/dotemacs.org#buffers-and-windows
(use-package desktop
  :hook
  (after-init . desktop-read)
  (after-init . desktop-save-mode)
  )

;; save history
;; https://github.com/sboosali/.emacs.d/blob/master/sboo/configuration/10-internal-packages/sboo-savehist.el
;; https://github.com/fasciism/dot-emacs/blob/master/2017-01-13-remembering-history.org

(use-package savehist
  :hook
  (after-init . savehist-mode)
  :config
  (setq-default savehist-file (concat user-emacs-directory "savehist"))
  (when (not (file-exists-p savehist-file))
    (write-file savehist-file))
  (setq savehist-save-minibuffer-history t)
  (setq history-delete-duplicates t)
  (setq history-length 100)
  (put 'minibuffer-history 'history-length 50)
  (put 'evil-ex-history 'history-length 50)
  (put 'kill-ring 'history-length 25))

;; not sure if I need this
(use-package focus-autosave-mode
  :init (focus-autosave-mode)
  :diminish focus-autosave-mode)


;; Make windmove work in Org mode: (untested but left here)
(use-package org
  :config
  (setq org-replace-disputed-keys t)
  (setq org-log-done 'time)
  )


(use-package whitespace-cleanup-mode
  :diminish
  :commands whitespace-cleanup-mode
  ;;:config
  ;;(global-whitespace-cleanup-mode 1)
  )


;;; Byte-compilation
(setq load-prefer-newer t)
(use-package auto-compile
  :config
  (auto-compile-on-save-mode))


;; Pull in ./config/*
(add-to-list 'load-path (expand-file-name "config" user-emacs-directory))

(require 'init-windmove)
(require 'init-ivy)
(require 'init-company)

(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))


(use-package which-key
  :config
  (which-key-mode))


(use-package yasnippet
  :config
;;  (use-package yasnippet-snippets)
  (yas-reload-all))


(require 'init-lsp)

(use-package golden-ratio
  :defer t
  :config
  (golden-ratio-mode 1))

(use-package flycheck
  :defer t
 ;; :diminish
  :init (global-flycheck-mode)
  :custom
  (flycheck-display-errors-delay .3)
  ;;(flycheck-stylelintrc "~/.stylelintrc.json")
  )

(use-package magit
  :config
  (setq magit-push-always-verify nil)
  (setq git-commit-summary-max-length 50)
  :bind
  ("M-g" . magit-status))

(use-package all-the-icons)

(use-package zerodark-theme
;;  :after (all-the-icons flycheck magit)
  :init (load-theme 'zerodark t)
  :config
  (zerodark-setup-modeline-format)
  )

(provide 'init)
;;; init.el ends here
