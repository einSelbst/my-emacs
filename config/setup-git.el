;;; setup-git.el --- Magit and git modes

;;; Commentary:

;; https://libraries.io/github/svend/dot-emacsd
;; https://github.com/Qinka/coremacs/blob/5df9d574b8001a83f92b41d8963be942f9e5dce6/lisp/tool/vcs.el

;;; Code:
(require 'use-package)

(use-package magit
  :init
  (setq magit-auto-revert-mode nil)  ; We have global-auto-revert mode enabled
  :bind (:map vc-prefix-map
              ("C-x g" . magit-status)
              ("C-x M-g" . magit-dispatch)
              ("C-c M-g" . magit-file-popup))
  :config
  ;; (setq magit-push-always-verify nil)
  (setq git-commit-summary-max-length 50)
  ;; (setq magit-completing-read-function 'magit-ido-completing-read)
  ;; (setq magit-repository-directories '("~/src"))

  ;; (setq magit-save-repository-buffers 'dontask)

  ;; ;; See https://github.com/magit/magit/issues/2265
  ;; ;; and https://debbugs.gnu.org/cgi/bugreport.cgi?bug=7847
  ;; (when (eq window-system 'ns) (setq magit-revision-use-gravatar-kludge t))
  ;; (add-hook 'after-save-hook #'magit-after-save-refresh-status)))
  )

;; Github integration
(use-package magithub
  :after magit
  :custom
  (magithub-api-timeout 10)
  ;; (magithub-pull-request-arguments '("-o"))
  ;; :config
  ;; (magithub-feature-autoinject t)
  ;; (setq magithub-clone-default-directory "~/dev/shane"))
  ;; https://github.com/vermiculus/magithub/issues/308
  ;; (remove-hook 'magit-status-headers-hook #'magithub-maybe-insert-ci-status-header))
)

;; Walk through git revisions of a file
(use-package git-timemachine
  :bind (:map vc-prefix-map
              ("t" . git-timemachine)))

(use-package gitconfig-mode
  :defer)

(use-package gitignore-mode
  :defer t)


(provide 'setup-git)
;;; setup-git.el ends here
