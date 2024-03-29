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

## Installation

I'm on OSX and there are [a few options](https://www.emacswiki.org/emacs/EmacsForMacOS#toc12) to get emacs. I'm only interested in homebrew based installs and so I have installed some of the following, but I don't have a clear preference yet. For better distinction different icons can be used.

I want to use deamon mode like this:

```shell
brew tap daviderestivo/emacs-head
brew install emacs-head@28 --with-cocoa --with-native-comp --with-modern-icon-elrumo2
ln -s /usr/local/opt/emacs-head@28/Emacs.app /Applications
```

Start deamon

```shell
emacs --with-profile einselbst --daemon
```

Start client

```shell
emacsclient -c -s einselbst
```

but then my desktop save/restore is not working and since my emacs is pretty quick anyway I rather not use deamon mode.

### Emacs 

Default homebrew installation.

``` shell
brew install emacs --use-git-head --with-cocoa --with-gnutls --with-rsvg --with-imagemagick
```

Default homebrew installation for latest beta.

``` shell
brew install emacs --HEAD --use-git-head --with-cocoa --with-gnutls --with-rsvg --with-imagemagick
```
---

### Emacs (cask)

Pure Emacs. Default homebrew cask installation, by David Caldweel.

https://emacsformacosx.com/ 

``` shell
brew cask install emacs
```
---

### Emacs Plus

https://github.com/d12frosted/homebrew-emacs-plus

``` shell
brew tap d12frosted/emacs-plus
brew install emacs-plus
ln -s /usr/local/opt/emacs-plus/Emacs.app /Applications/Emacs-Plus.app
```
---

### Emacs-Head

https://github.com/daviderestivo/homebrew-emacs-head

``` shell
brew tap daviderestivo/emacs-head
brew install emacs-head --HEAD --with-cocoa --with-jansson --with-modern-icon-sexy-v1
ln -s /usr/local/opt/emacs-head/Emacs.app /Applications/Emacs-Head.app
```

<details>
<summary>Shell Output</summary>

```shell
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

</details>

---

### Emacs-Mac

With native GUI support for Mac OS X, by Mitsuharu Yamamoto.

https://bitbucket.org/mituharu/emacs-mac/src/master/README-mac
https://github.com/railwaycat/homebrew-emacsmacport

``` shell
brew tap railwaycat/emacsmacport
brew cask install emacs-mac
ln -s /usr/local/opt/emacs-mac/Emacs.app /Applications/Emacs-Mac.app
```

## Emacs Profile Switcher

To use different emacs configurations side-by-side.

- https://github.com/plexus/chemacs

## Package Management

- https://github.com/raxod502/straight.el
- use-package

<details>
<summary>use-package cheat sheet</summary>

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

</details>


## Startup Profiling

https://github.com/jschaf/esup

Usage `M-x esup`

## Configuration

- Use custom file for changed default configs to not litter the init file.
- [Fixing wrong PATH on osx](https://github.com/purcell/exec-path-from-shell)
- editorconfig

## UI

- doom moonlight theme via [emacs-doom-themes](https://github.com/hlissner/emacs-doom-themes)
- https://github.com/roman/golden-ratio.el

## Persistency

- desktop save mode
- savehist

## Locomotion

- windmove

## Convenience

- https://github.com/purcell/whitespace-cleanup-mode
- https://github.com/emacscollective/auto-compile
- https://github.com/justbur/emacs-which-key/
- Ivy/Counsel/Swiper/Smex
  - https://dev.to/deciduously/how-i-emacs-and-so-can-you-packages-m9p
  - https://writequit.org/denver-emacs/presentations/2017-04-11-ivy.html#fnr.1
  - http://bnbeckwith.com/bnb-emacs/#org8e91110
- Company
- Midnight - auto-close buffers if not edited recently
- Focus-autosave-mode - saves buffer when moving to other buffer
- Magit
- Wakatime

## Language support

- https://github.com/jrblevin/markdown-mode
- https://emacs-lsp.github.io/lsp-mode/

- flycheck
  - https://www.flycheck.org/en/latest/index.html
  - https://www.reddit.com/r/emacs/comments/931la6/tip_how_to_adopt_flycheck_as_your_new_best_friend/

## Web.dev

- lsp
- web-mode
- https://github.com/smihica/emmet-mode

# ToDo

- https://www.emacswiki.org/emacs/EmacsClient
- https://patrickskiba.com/emacs/2019/09/07/emacs-for-react-dev.html

# more ideas

- https://github.com/emacs-tw/awesome-emacs
- https://github.com/Wilfred/helpful
- https://github.com/ACEMerlin/lain-emacs
- https://github.com/emacscollective/no-littering

- https://zzamboni.org/post/my-emacs-configuration-with-commentary/
- https://github.com/daedreth/UncleDavesEmacs

- from reddit https://www.reddit.com/r/emacs/comments/hbti7g/is_there_something_better_for_htmlcss_editing/
  - https://emmet.io/
  - https://github.com/veshboo/emacs/blob/master/lisp/textmodes/mhtml-mode.el
  - https://emacs-lsp.github.io/lsp-mode/page/lsp-html/
  - https://emacs-lsp.github.io/lsp-mode/page/lsp-css/

MacOS Dark Mode detection
- https://github.com/daviderestivo/homebrew-emacs-head#system-appearance-change-support
- https://www.reddit.com/r/emacs/comments/h9zoy9/weekly_tipstricketc_thread/fuzucay/

- https://www.emacswiki.org/emacs/GotoChg
- emmet
  - https://github.com/madintist/emacs-config/blob/e21d3b2127c6946bbc9835728e264b88083bd55b/.emacs.d/config/packages/emmet.el

- js-2, rjsx & lsp https://github.com/markx/emacs.d/blob/4bb3dd529647880e6ee399cb8c45d840d672d6d8/config/init-js.el

- https://github.com/d12frosted/flyspell-correct

# commands

- M-x describe-personal-keybindings - to see all such keybindings you've set throughout your .emacs file
- C-h C-a                           - more info about emacs, the start screen
- if you want to know why something is displayed in a particular color, put the cursor on it and type C-u C-x = (the command what-cursor-position with a prefix argument), which displays lots of information about whatever's under the cursor, including its current face.

Emacs Lisp is extremely well documented. [Thanks!](https://github.com/0x7ffc/lain-emacs)

- C-h k documentation for a key stroke
- C-h f documentation for a function
- C-h v documentation for a variable
- C-h b list of all keybindings available in current buffer
- C-h S search symbol in Emacs manual

### from counsel

- ("M-x"     . counsel-M-x)
- ("C-s"     . swiper)
- ("C-x C-f" . counsel-find-file)
- ("C-x C-r" . counsel-recentf)  ; search for recently edited
- ("C-c g"   . counsel-git)      ; search for files in git repo
- ("C-c j"   . counsel-git-grep) ; search for regexp in git repo
- ("C-c /"   . counsel-ag)       ; Use ag for regexp
- ("C-x l"   . counsel-locate)
- ("C-x C-f" . counsel-find-file)
- ("<f1> f"  . counsel-describe-function)
- ("<f1> v"  . counsel-describe-variable)
- ("<f1> l"  . counsel-find-library)
- ("<f2> i"  . counsel-info-lookup-symbol)
- ("<f2> u"  . counsel-unicode-char)
- ("C-c C-r" . ivy-resume)       ; Resume last Ivy-based completion

### Emmet-mode

- C-j expand snippet (emmet-expand-line)

- Go to Edit Point - Traverse between important code points in HTML:
- <C-M-left> is "Previous Edit Point" (M-x emmet-prev-edit-point)
- <C-M-right> is "Next Edit Point" (M-x emmet-next-edit-point)

## Troubleshooting

- `M-x package-refresh-contents`   if a package won't be loaded
- `M-x flycheck-verify-setup`


