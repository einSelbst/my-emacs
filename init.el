;; einSelbst emacs init file
;; https://github.com/einSelbst/my-emacs

;; load dedicated custom.el
;; https://github.com/angrybacon/dotemacs/blob/master/dotemacs.org#load-customel

(setq-default custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

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
