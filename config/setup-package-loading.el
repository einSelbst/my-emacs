;;; setup-package-loading.el --- that's it

;;; Commentary:
;; setup straight.el package manager
;; https://github.com/raxod502/straight.el

;;; Code:

;; must be set before straight.el bootstrap runs
(setq
 ;; load-prefer-newer t
 straight-repository-branch "develop"
 ;; straight-check-for-modifications '(find-when-checking)
 straight-use-package-by-default t
 straight-cache-autoloads t
 ;; straight-treat-as-init t
 straight-vc-git-default-clone-depth 1
 use-package-always-defer t
 ;; use-package-always-ensure t  ;; straight.el says this should be disabled
 ;; use-package-expand-minimally t
 use-package-verbose t
 use-package-compute-statistics t
 ;; debug-on-error t
 )

;; little hack to fix an error in the init file:
;; Cannot open load file: No such file or directory, /Users/einselbst/.emacs.d/straight/repos/straight.el/straight.elc
(setq user-emacs-directory "~/emacs-profiles/my-emacs/")

;; I have an issue with an unavailable package and straight says:
;; "error: Could not find package gitconfig-mode. Updating recipe repositories: (org-elpa melpa gnu-elpa-mirror el-get emacsmirror-mirror) with ‘straight-pull-recipe-repositories’ may fix this"
;; (setq straight-recipe-repositories '(org-elpa melpa gnu-elpa-mirror el-get emacsmirror-mirror straight-pull-recipe-repositories))

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

(require 'straight) ;; does this fix the error message

(straight-use-package 'esup)

;; setup use-package
(straight-use-package 'use-package)
;; (require 'use-package) ;; does this fix the error message?

;; https://github.com/jwiegley/use-package#use-packageel-is-no-longer-needed-at-runtime
(eval-when-compile
  (require 'use-package))
;; (require 'diminish)                ;; if you use :diminish
(require 'bind-key)

;; to use system packages https://github.com/jwiegley/use-package#use-package-ensure-system-package
(use-package use-package-ensure-system-package
  :ensure t)

;; updates GPG keys used by ELPA package manager
;; https://elpa.gnu.org/packages/gnu-elpa-keyring-update.html
;; (use-package gnu-elpa-keyring-update)

(provide 'setup-package-loading)
;;; setup-package-loading.el ends here
