# My emacs settings

An ever-changing set of emacs settings. Primary used for [Go](https://golang.org/) software development.

## Setup

To get the configuration:

    git clone --recursive https://github.com/140am/.emacs.d.git

## Install emacs on macOS

I use [Cocoa Emacs](https://emacsformacosx.com/), installed like this:

    brew cask install emacs

Install used packages with `M-x package-install`:

- exec-path-from-shell
- auto-complete
- go-mode
- go-autocomplete

Install used Go development dependencies:

```
go get -u golang.org/x/tools/cmd/...
go get -u github.com/nsf/gocode
go get github.com/rogpeppe/godef
```

Assumes the following env variables are set:

- GOROOT
- GOPATH
- PATH=$PATH:$GOROOT/bin
- PATH=$PATH:$GOPATH/bin
