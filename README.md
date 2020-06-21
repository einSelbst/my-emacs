# my-emacs
repo for emacs 27 with lsp, wip

## Motivation

My Emacs config is a 10+ years mess.
It's safe to say I don't really know what's going on but it
mostly worked "good enough". I'm a long time prelude user and
have been happy with it. However, I never really managed to use
all of it's features as it was a little bit overwhelming. The
thing I struggled most with is typescript/react support and
so this is what I start with.

# Installation

## Emacs

https://github.com/daviderestivo/homebrew-emacs-head

``` shell
brew tap daviderestivo/emacs-head
brew install emacs-head --HEAD --with-cocoa --with-jansson --with-modern-icon-sexy-v1

ln -s /usr/local/opt/emacs-head/Emacs.app /Applications/Emacs-Head.app
```

Shell Output

``` shell
==> emacs-head
Emacs.app was installed to:
  /usr/local/opt/emacs-head
To link the application:
  ln -s /usr/local/opt/emacs-head/Emacs.app /Applications

To have launchd start daviderestivo/emacs-head/emacs-head now and restart at login:
  brew services start daviderestivo/emacs-head/emacs-head
Or, if you don't want/need a background service you can just run:
  emacs
```

## Emacs Profile Switcher

https://github.com/plexus/chemacs

I have this setup already but add a new profile which
is basically this Github repo.

## Package Manager

https://github.com/raxod502/straight.el

## Startup Profiling

https://github.com/jschaf/esup

Usage `M-x esup`

Total User Startup Time: 0.100sec     Total Number of GC Pauses: 2     Total GC Time: 0.026sec
Total User Startup Time: 0.093sec     Total Number of GC Pauses: 2     Total GC Time: 0.028sec
Total User Startup Time: 0.233sec     Total Number of GC Pauses: 3     Total GC Time: 0.041sec
Total User Startup Time: 0.232sec     Total Number of GC Pauses: 3     Total GC Time: 0.045sec

## Changing some default configs via menu

Using a custom file for this to not litter the init file.


## Persistency

- desktop save mode
- savehist

## Locomotion

- windmove

## Convenience

- https://github.com/purcell/whitespace-cleanup-mode
- https://github.com/emacscollective/auto-compile
- helm?
- ido?
- maggit?

## Language support

- https://github.com/jrblevin/markdown-mode
# use-package cheat sheet

```elisp
(use-package color-moccur
  ;; the :commands keyword creates autoloads for those commands and defers loading of the module until they are used
  :commands (isearch-moccur isearch-all)
  ;; to bind a key to primary commands within that module
  :bind (("M-s O" . moccur)
         :map isearch-mode-map
         ("M-o" . isearch-moccur)
         ("M-O" . isearch-moccur-all))
  ;; for keymaps, not functions
  :bind-keymap
  ("C-c p" . projectile-command-map)
  ;; use the :init keyword to execute code before a package is loaded
  :init  
  (setq isearch-lazy-highlight t)
  ;; :config can be used to execute code after a package is loaded
  :config
  (use-package moccur-edit))

(use-package ruby-mode
  :mode "\\.rb\\'"
  :interpreter "ruby")

(use-package ruby-mode
  :mode "\\.rb\\'"
  :interpreter "ruby"
  ;; If you need to silence a missing function warning, you can use :functions
  :functions inf-ruby-keys
  :config
  (defun my-ruby-mode-hook ()
    (require 'inf-ruby)
    (inf-ruby-keys))

  (add-hook 'ruby-mode-hook 'my-ruby-mode-hook))

;; The package is "python" but the mode is "python-mode":
(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode))

;; If you aren't using :commands, :bind, :bind*, :bind-keymap, :bind-keymap*, :mode, :interpreter, or :hook (all of which imply :defer you can still defer loading with the :defer keyword:

(use-package ace-jump-mode
  :defer t
  :init
  (autoload 'ace-jump-mode "ace-jump-mode" nil t)
  (bind-key "C-." 'ace-jump-mode))

;; The :hook keyword allows adding functions onto package hooks. Thus, all of the following are equivalent:

(use-package ace-jump-mode
  :hook prog-mode)

(use-package ace-jump-mode
  :hook (prog-mode . ace-jump-mode))

(use-package ace-jump-mode
  :commands ace-jump-mode
  :init
  (add-hook 'prog-mode-hook #'ace-jump-mode))

;; The :custom keyword allows customization of package custom variables.

(use-package comint
  :custom
  (comint-buffer-maximum-size 20000 "Increase comint buffer size.")
  (comint-prompt-read-only t "Make the prompt read only."))

;; The :custom-face keyword allows customization of package custom faces.

(use-package eruby-mode
  :custom-face
  (eruby-standard-face ((t (:slant italic)))))

;; You can use the :if keyword to predicate the loading and initialization of modules.

;; For example, I only want edit-server running for my main, graphical Emacs, not for other Emacsen I may start at the command line:

(use-package edit-server
  :if window-system
  :init
  (add-hook 'after-init-hook 'server-start t)
  (add-hook 'after-init-hook 'edit-server-start t))

;; In another example, we can load things conditional on the operating system:

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :ensure t
  :config
  (exec-path-from-shell-initialize))

;; The :disabled keyword can turn off a module you're having difficulties with, or stop loading something you're not using at the present time:

(use-package ess-site
  :disabled
  :commands R)

;; using an :after keyword that allows a fairly rich description of the exact conditions when loading should occur. Here is an example:

(use-package hydra
  ;; The :ensure keyword causes the package(s) to be installed automatically if not already present on your system:
  :ensure t
  :load-path "site-lisp/hydra")

(use-package ivy
  :load-path "site-lisp/swiper")

(use-package ivy-hydra
  :after (ivy hydra))

;; use the :defines and :functions keywords to introduce dummy variable and function declarations solely for the sake of the byte-compiler:

(use-package texinfo
  :defines texinfo-section-list
  :commands texinfo-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.texi$" . texinfo-mode)))

;; all that you want to use use-package for is to add a configuration to the eval-after-load hook. In such cases, use the :no-require keyword:

(use-package foo
  :no-require t
  :config
  (message "This is evaluated when `foo' is loaded"))
```
