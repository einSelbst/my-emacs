;;; init.el --- emacs config for (m)einSelbst

;; Copyright © 2013-2020 Phil Hagelberg and contributors

;; Author: einSelbst
;; URL: ;; https://github.com/einSelbst/my-emacs
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Created: 2020-07-01
;; Keywords: convenience

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; einSelbst Emacs init file

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

;; Pull in ./config/*
(add-to-list 'load-path (expand-file-name "config" user-emacs-directory))
;; (push (expand-file-name "config" user-emacs-directory) load-path)

(require 'speed-up)

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
 scroll-bar-mode -1                          ; don't show scroll bars
 select-enable-clipboard t                   ; Merge system's and Emacs' clipboard
 sentence-end-double-space nil               ; End a sentence after a dot and a space
 show-trailing-whitespace nil                ; Display trailing whitespaces
 ;; tab-width 4                                 ; Set width for tabs
 uniquify-buffer-name-style 'forward         ; Uniquify buffer names
 visible-bell t                              ; Do not emit noise
 window-combination-resize t                 ; Resize windows proportionally
 x-stretch-cursor t                          ; Stretch cursor to the glyph width
 custom-file (expand-file-name "custom.el" user-emacs-directory))  ; load dedicated custom.el

;; https://github.com/angrybacon/dotemacs/blob/master/dotemacs.org#load-customel
(when (file-exists-p custom-file)
  (load custom-file))

(cd "~/")                           ; Move to the user directory
(delete-selection-mode 1)           ; Replace region when inserting text
(fset 'yes-or-no-p 'y-or-n-p)       ; Replace yes/no prompts with y/n
(global-auto-revert-mode 1)         ; Always show the most recent version of a file
(global-hl-line-mode 1)             ; Highlight current line
(global-subword-mode 1)             ; Iterate through CamelCase words
;; (global-visual-line-moode 1)     ; Act on (wrapped) visual, not logical lines
;; (global-highlight-changes-mode 1)
(mouse-avoidance-mode 'banish)     ; Avoid collision of mouse with point
(put 'downcase-region 'disabled nil)         ; Enable downcase-region
(put 'upcase-region 'disabled nil)           ; Enable upcase-region
(set-default-coding-systems 'utf-8)          ; Default to utf-8 encoding
(transient-mark-mode t)                      ; Highlight active region
(setq case-fold-search t)                    ; case insensitive search

;; fix dired
(when (eq system-type 'darwin)
  (setq dired-use-ls-dired nil))

;; https://emacs.stackexchange.com/a/437/16535
(defun display-startup-echo-area-message ()
  "Always remember taking care of your thoughts."
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

(require 'setup-package-loading)

;; ----------- GCMH - the Garbage Collector Magic Hack --------
;; https://github.com/emacsmirror/gcmh
;; http://akrl.sdf.org/
;; not sure if this conflicts with my other settings for gc
(setq garbage-collection-messages t)
(use-package gcmh
  :config
  (gcmh-mode 1)
  :custom
  (gcmh-verbose t)
  ;; (gcmh-lows-cons-threshold #x800000)
  (gcmh-high-cons-threshold #x40000000) ; 1GB
  (gcmh-idle-delay 15)
  )

;; Use same paths as user environment on macOS
(use-package exec-path-from-shell
  ;; :if (memq window-system '(mac ns))
  :init
  (exec-path-from-shell-initialize)
  :custom
  (exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-variables '("PATH" "MANPATH"))
)

;; (use-package exec-path-from-shell
;;   :if (eq system-type 'darwin)
;;   :custom
;;   (exec-path-from-shell-check-startup-files nil)
;;   (exec-path-from-shell-variables '("PATH" "MANPATH"))
;;   :config
;;   (exec-path-from-shell-initialize))

(use-package async
  :init
  (dired-async-mode
   async-bytecomp-package-mode)
  )

;; saving files and state
(require 'init-persistency)

;; https://zzamboni.org/post/my-emacs-configuration-with-commentary/
(use-package midnight
  :defer 3
  :config
  (setq midnight-period 7200)
  (midnight-mode 1))

;;; Byte-compilation
(setq load-prefer-newer t)
(use-package auto-compile
  :commands auto-compile-on-save-mode)

(use-package editorconfig
  :custom
  (editorconfig-mode 1))

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

;; load my personal helper functions
(require 'setup-helper)
(require 'init-windmove)
(require 'init-ivy)
(require 'init-company)

(use-package which-key
  :config
  (which-key-mode 1))

(use-package yasnippet
  :diminish yas-minor-mode
  :hook (after-init . yas-global-mode)
  :config (use-package yasnippet-snippets)
  ; (yas-reload-all)
  )

(require 'init-lsp)
(require 'setup-git)  ;; includes magit
(require 'setup-treemacs)

(use-package flycheck
  :init (global-flycheck-mode)
  :custom
  (flycheck-display-errors-delay .3)
  (flycheck-emacs-lisp-load-path 'inherit)
  (flycheck-javascript-eslint-executable "eslint_d")
  ;;(flycheck-stylelintrc "~/.stylelintrc.json")
  )

(require 'setup-ui)
(require 'setup-wakatime)

(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package json-mode)

;; http://ergoemacs.org/emacs/xah-html-mode.html
;; must buy these now
;; (use-package xah-html-mode
  ;; :straight (xah-html-mode :host github :repo "xahlee/xah-html-mode.el"))
;; (use-package xah-replace-pairs)
;; (use-package xah-get-thing)
;; (use-package xah-css-mode)

(use-package htmlize)

(use-package dockerfile-mode)

(use-package terraform-mode)

(use-package yaml-mode)

(use-package web-mode
  ;; :disabled
  :mode (("\\.html?\\'" . web-mode)
         ("\\.css\\'"   . web-mode)
         ("\\.jsx?\\'"  . web-mode)
         ("\\.tsx?\\'"  . web-mode))
  :config
  ;; (setq web-mode-markup-indent-offset ian/indent-width)
  ;; (setq web-mode-code-indent-offset ian/indent-width)
  ;; (setq web-mode-css-indent-offset ian/indent-width)
  (setq web-mode-content-types-alist '(("tsx" . "\\.ts[x]?\\'")))
  (setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))
  (setq lsp-enable-indentation nil)) ;; fix broken indentation in web-mode

;; Minor-mode to automatically fix javascript with eslint_d.
;; https://github.com/aaronjensen/eslintd-fix
(use-package eslintd-fix
  :config
  (add-hook 'web-mode-hook 'eslintd-fix-mode)
  (add-hook 'js2-mode-hook 'eslintd-fix-mode)
  (add-hook 'typescript-mode 'eslintd-fix-mode))

;; silence warning about package cl deprecated
;; https://github.com/kiwanami/emacs-epc/issues/35
(setq byte-compile-warnings '(cl-functions))

(use-package emmet-mode
  :hook (css-mode scss-mode js-mode sgml-mode web-mode html-mode haml-mode nxml-mode rjsx-mode)
  :custom
  (emmet-expand-jsx-className? t)
  (emmet-move-cursor-between-quotes t)
  (emmet-self-closing-tag-style " /") ;; default "/"
;;   ;; only " /", "/" and "" are valid.
;;   ;; eg. <meta />, <meta/>, <meta>
;;   ;; (when (require 'yasnippet nil t)
;;     ;; (add-hook 'emmet-mode-hook #'yas-minor-mode-on))
  )

;;;; some adventures

;; the fancy version of using prettier
(use-package apheleia
  :straight '(apheleia :host github :repo "raxod502/apheleia")
  ;; not really sure what that 'setup-format-buffer-apheleia' is doing
  :init
  (defun setup-format-buffer-apheleia()
    (setq-local format-buffer-fn 'apheleia-format-buffer))
  :hook ((web-mode js-mode scss-mode) . setup-format-buffer-apheleia)
  :config
  (apheleia-global-mode +1))

;; https://github.com/purcell/emacs-shfmt/
(use-package shfmt
  :hook (sh-mode . shfmt-on-save-mode)
  :custom (shfmt-arguments '("-i" "4")))

;; no official emacs support from kite, just see if it still works
;;(load-file "~/.emacs.d/kite.el")

;; (use-package kite
;;   :load-path "site-lisp/"
;;   )


(provide 'init)
;;; init.el ends here
