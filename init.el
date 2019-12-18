;; einSelbst emacs init file
;; https://github.com/einSelbst/my-emacs

;; set defaults
;; https://github.com/angrybacon/dotemacs/blob/master/dotemacs.org#use-better-defaults

(setq-default
 ;; ad-redefinition-action 'accept                   ; Silence warnings for redefinition
 ;; auto-window-vscroll nil                          ; Lighten vertical scroll
 ;; confirm-kill-emacs 'yes-or-no-p                  ; Confirm before exiting Emacs
 ;; cursor-in-non-selected-windows nil               ; Hide the cursor in inactive windows
 delete-by-moving-to-trash t                      ; Delete files to trash
 ;; display-time-default-load-average nil            ; Don't display load average
 ;; display-time-format "%H:%M"                      ; Format the time string
 fill-column 80                                   ; Set width for automatic line breaks
 help-window-select t                             ; Focus new help windows when opened
 indent-tabs-mode nil                             ; Stop using tabs to indent
 inhibit-startup-screen t                         ; Disable start-up screen
 initial-scratch-message ""                       ; Empty the initial *scratch* buffer
 mouse-yank-at-point t                            ; Yank at point rather than pointer
 ;; ns-use-srgb-colorspace nil                       ; Don't use sRGB colors
 recenter-positions '(5 top bottom)               ; Set re-centering positions
 scroll-conservatively most-positive-fixnum       ; Always scroll by one line
 scroll-margin 10                                 ; Add a margin when scrolling vertically
 select-enable-clipboard t                        ; Merge system's and Emacs' clipboard
 sentence-end-double-space nil                    ; End a sentence after a dot and a space
 show-trailing-whitespace nil                     ; Display trailing whitespaces
 tab-width 4                                      ; Set width for tabs
 uniquify-buffer-name-style 'forward              ; Uniquify buffer names
 window-combination-resize t                      ; Resize windows proportionally
 x-stretch-cursor t                               ; Stretch cursor to the glyph width
 
 custom-file (expand-file-name "custom.el" user-emacs-directory))  ; load dedicated custom.el
;; https://github.com/angrybacon/dotemacs/blob/master/dotemacs.org#load-customel
(when (file-exists-p custom-file)(load custom-file))

(cd "~/")                                         ; Move to the user directory
(delete-selection-mode 1)                         ; Replace region when inserting text
(fset 'yes-or-no-p 'y-or-n-p)                     ; Replace yes/no prompts with y/n
(global-subword-mode 1)                           ; Iterate through CamelCase words
(mouse-avoidance-mode 'banish)                    ; Avoid collision of mouse with point
(put 'downcase-region 'disabled nil)              ; Enable downcase-region
(put 'upcase-region 'disabled nil)                ; Enable upcase-region
(set-default-coding-systems 'utf-8)               ; Default to utf-8 encoding


;; straight.el package manager
;; https://github.com/raxod502/straight.el

(setq
 ;; load-prefer-newer t
 ;; straight-repository-branch "develop"
 ;; straight-check-for-modifications '(find-when-checking)
 straight-use-package-by-default t
 ;; straight-cache-autoloads t
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
