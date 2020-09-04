;;; init-lsp.el --- something

;;; Commentary:

;; LSP mode
;; https://emacs-lsp.github.io/lsp-mode/page/installation/

;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
;; (setq lsp-keymap-prefix "s-l")

;;; Code:
(require 'use-package)

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (
;;          (c++-mode        . lsp-deferred)
;;          (cmake-mode      . lsp-deferred)
;;          (css-mode        . lsp-deferred)
;;          (go-mode         . lsp-deferred)
;;          (javascript-mode . lsp-deferred)
;;          (python-mode     . lsp-deferred)
;;          (rust-mode       . lsp-deferred)
;;          (web-mode        . lsp-deferred)
;;          (yaml-mode       . lsp-deferred)
;;          (html-mode       . lsp-deferred)
;;          (typescript-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration))
  :init
  ;; Increase max number of bytes read from subprocess in a single chunk.
  (setq read-process-output-max (* 1024 1024)):config
  :config
  (require 'lsp-clients)
  (add-hook 'prog-mode-hook 'lsp)
  ;; optimize performance
  ;; https://emacs-lsp.github.io/lsp-mode/page/performance/
  (setq ;; read-process-output-max (* 1024 1024) ;; 1mb
        ;; gc-cons-threshold 100000000            ;; 100mb ;; already set in speed-up.el
        ;; additional settings
        lsp-enable-semantic-highlighting 1
        lsp-modeline-code-actions-mode 1
        lsp-headerline-breadcrumb-mode 1
        ;; auto restart lsp
        lsp-restart 'auto-restart)
  
  (use-package lsp-ui
   :config
   ;; (setq lsp-ui-sideline-ignore-duplicate t)
   ;; (setq lsp-ui-flycheck-enable nil)
   (add-hook 'lsp-mode-hook 'lsp-ui-mode))

  (use-package company-lsp
    :requires company
    :config
    ;; Disable client-side cache because the LSP server does a better job.
    ;; from https://www.mortens.dev/blog/emacs-and-the-language-server-protocol/
    (setq company-transformers nil
          company-lsp-async t
          company-lsp-cache-candidates nil
          company-minimum-prefix-length 1
          company-idle-delay 0.1) ;; default is 0.2
    (push 'company-lsp company-backends))

  
  ;;(defun lsp-set-cfg ()
  ;;  (let ((lsp-cfg `(:pyls (:configurationSources ("flake8")))))
  ;;    (lsp--set-configuration lsp-cfg)))

  ;;(add-hook 'lsp-after-initialize-hook 'lsp-set-cfg))

;;(lsp-register-client
;; (make-lsp-client :new-connection (lsp-tramp-connection "pyls")
;;                  :major-modes '(python-mode)
;;                  :remote? t
;;                  :server-id 'pyls-remote))

)

;; optionally
(use-package lsp-ui
  :commands lsp-ui-mode
  :after lsp-mode flycheck
  :config
  (setq
   lsp-ui-doc-enable t
   lsp-ui-doc-use-childframe t
   lsp-ui-doc-position 'top
   ;; lsp-ui-doc-include-signature t
   lsp-ui-sideline-enable t
   lsp-ui-sideline-show-code-actions t
   lsp-ui-sideline-show-hover t
   lsp-ui-sideline-update-mode 'line
   lsp-ui-flycheck-enable t
   lsp-ui-flycheck-list-position 'right
   lsp-ui-flycheck-live-reporting t
   lsp-ui-peek-enable t
   lsp-ui-peek-list-width 60
   lsp-ui-peek-peek-height 25
   )
  )


;; if you are helm user
;;(use-package helm-lsp :commands helm-lsp-workspace-symbol)

;; if you are ivy user
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; optionally if you want to use debugger
;;(use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

;; Javascript, Typescript and Flow support for lsp-mode
;; Install: npm i -g javascript-typescript-langserver
;;(use-package lsp-javascript-typescript
;;  :commands lsp-javascript-typescript-enable
;;  :hook ((typescript-mode javascript-mode js2-mode js-mode rjsx-mode) . lsp-javascript-typescript-enable))

(provide 'init-lsp)
;;; init-lsp.el ends here
