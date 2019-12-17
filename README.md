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
