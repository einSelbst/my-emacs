;;; init-lsp.el --- Configuration for LSP -*- lexical-binding: t -*-
;; Time-stamp: <2021-02-10 00:05:07 einselbst>

;;; Commentary:

;; lsp-mode:  Emacs client/library for the Language Server Protocol
;; https://emacs-lsp.github.io/lsp-mode/page/installation/
;; https://github.com/emacs-lsp/lsp-mode

;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
;; (setq lsp-keymap-prefix "s-l")

;; Copyright (C) 2016-2021 einSelbst
;; Author: einSelbst <einselbst@mailbox.org>

;;; Code:
(require 'use-package)

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook ((prog-mode . (lambda ()
                        (unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode)
                          (lsp-deferred))))
         ;; (js2-mode . lsp-deferred)
         ;; (js-mode . lsp-deferred)
         ;; (json-mode . lsp-deferred)
         ;; (web-mode . lsp-deferred)
         ;; (typescript-mode-hook . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration)
         (lsp-after-open . lsp-enable-imenu)
         (lsp-after-open . (lambda ()
                            (setq-local company-minimum-prefix-length 1
                                  company-idle-delay 0.0) ;; default is 0.2
                            )))
  :bind (:map lsp-mode-map
              ("C-c C-t" . lsp-describe-thing-at-point))

  :init
  ;; @see https://emacs-lsp.github.io/lsp-mode/page/performance
  ;; Increase max number of bytes read from subprocess in a single chunk.
  (setq read-process-output-max (* 1024 1024) ;; 1MB
        ;; gc-cons-threshold 100000000 ;; 100mb ;; already set in speed-up.el
        lsp-idle-delay 0.5)

  :config
 ;; (require 'lsp-clients)

  (setq lsp-auto-guess-root t ; Detect project root
        lsp-keep-workspace-alive t ; Auto-kill LSP server
        ;; lsp-prefer-capf t
        lsp-enable-indentation t
        lsp-enable-symbol-highlighting t
        lsp-enable-on-type-formatting t
        ;; additional settings
        lsp-enable-semantic-highlighting t
        lsp-modeline-code-actions-mode t
        lsp-headerline-breadcrumb-mode t
        ;; auto restart lsp
        lsp-restart 'auto-restart)
  ;; (setq lsp-prefer-flymake nil)
  )

;; lsp-ui: This contains all the higher level UI modules of lsp-mode, like flycheck support and code lenses.
;; https://github.com/emacs-lsp/lsp-ui
(use-package lsp-ui
  :after lsp-mode flycheck
  :config
  ;; (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  ;; (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  (setq lsp-ui-sideline-enable t
        lsp-ui-sideline-delay 0.4
        lsp-ui-sideline-show-diagnostics t ;; show diagnostics messages in sideline
        lsp-ui-sideline-show-hover t ;; show hover messages in sideline
        lsp-ui-sideline-show-code-actions t ;; show code actions in sideline
        lsp-ui-sideline-update-mode 'line;; When set to 'line' the information will be updated when user changes current line otherwise the information will be updated when user changes current point
        lsp-ui-sideline-ignore-duplicate t

        lsp-ui-doc-enable nil
        lsp-ui-doc-delay 0.6
        lsp-ui-doc-include-signature t
        lsp-ui-doc-position 'top
        ;; lsp-ui-doc-use-childframe t
        
        lsp-eldoc-enable-hover nil ; Disable eldoc displays in minibuffer

        lsp-ui-imenu-enable t

        lsp-ui-flycheck-enable t
        lsp-ui-flycheck-list-position 'right
        lsp-ui-flycheck-live-reporting t

        lsp-ui-peek-enable t
        lsp-ui-peek-always-show t
        lsp-ui-peek-list-width 60
        lsp-ui-peek-peek-height 25
        )
  )
  
;; debugger adapter protocol support for emacs
;; https://github.com/emacs-lsp/dap-mode/
(use-package dap-mode
  :defer 4
  :config
  ;; call dap-hydra when going to the next breakpoint
  (add-hook 'dap-stopped-hook
            (lambda (arg) (call-interactively #'dap-hydra)))
  (add-hook 'dap-mode-hook #'dap-ui-mode) ; use a hook so users can remove it
  (dap-mode 1))

;; load gdb-lldb package
(use-package dap-gdb-lldb
  :defer 5
  :straight nil)

;;   (use-package company-lsp
;;     :requires company
;;     :config
;;     ;; Disable client-side cache because the LSP server does a better job.
;;     ;; from https://www.mortens.dev/blog/emacs-and-the-language-server-protocol/
;;     (setq company-transformers nil
;;           company-lsp-async t
;;           company-lsp-cache-candidates nil
;;           company-minimum-prefix-length 1
;;           company-idle-delay 0.1) ;; default is 0.2
;;     (push 'company-lsp company-backends))

;;   ;;(defun lsp-set-cfg ()
;;   ;;  (let ((lsp-cfg `(:pyls (:configurationSources ("flake8")))))
;;   ;;    (lsp--set-configuration lsp-cfg)))

;;   ;;(add-hook 'lsp-after-initialize-hook 'lsp-set-cfg))

;; ;;(lsp-register-client
;; ;; (make-lsp-client :new-connection (lsp-tramp-connection "pyls")
;; ;;                  :major-modes '(python-mode)
;; ;;                  :remote? t
;; ;;                  :server-id 'pyls-remote))
;; )

;; ;; if you are helm user
;; ;;(use-package helm-lsp :commands helm-lsp-workspace-symbol)

;; if you are ivy user
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)

(provide 'init-lsp)
;;; init-lsp.el ends here
