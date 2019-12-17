# my-emacs
repo for emacs 27 with lsp, wip

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
