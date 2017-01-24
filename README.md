# My Emacs configuration

> "Take it as a LISP machine, a working env, a framework to build upon my liking."

Daily used with focus on [Go](https://golang.org/) & Web software development, Markdown text editing and [Org-mode](http://orgmode.org/).

## Features

- [go-mode](https://github.com/dominikh/go-mode.el) for the [Go](https://golang.org/) 1.6+ programming language
  - go fmt on save
  - [go lint](https://github.com/golang/lint) a document with `M-x golint`
  - [godef](https://github.com/rogpeppe/godef) jump to a definition with `M-.`
  - [go eldoc](https://github.com/syohex/emacs-go-eldoc) support to show type and argument information
- [web-mode](http://web-mode.org/) for editing web templates, JS/X, embed CSS/JS in HTML etc
- save the state of Emacs from one session to another
- auto complete, improved file & buffer navigation & [more colors](https://github.com/jordonbiondo/ample-theme#all-three-themes)!

## How does it look?

![Screenshot 1](http://cdn.140.am/i/762d19ab5586dc0c1c36f2588ffbc892.png)

## Setup

To get this Emacs configuration, run in home directory:

    git clone --recursive https://github.com/140am/.emacs.d.git

Done - start Emacs or [Install Emacs](https://github.com/140am/.emacs.d/wiki/Installing-Emacs).

## Usage

- [Keyboard Navigation Shortcuts](https://github.com/140am/.emacs.d/wiki/Keyboard-Navigation)
- [Go Software Development](https://github.com/140am/.emacs.d/wiki/Go-Software-Development)

## Customize

To customize the configuration fork this repo and edit [init.el](init.el).

## Resources

Links I found helpful getting comfortable with Emacs:

- [How to learn Emacs](http://david.rothlis.net/emacs/tutorial.html)
- [How to learn Emacs: A visual tutorial](http://sachachua.com/begin-emacs)

### Go Programming Language

- [Writing Go in Emacs](https://dominik.honnef.co/posts/2013/03/emacs-go-1/) & [part 2](https://dominik.honnef.co/posts/2013/08/emacs-go-2/)
- [Configure Emacs as a Go Editor From Scratch](http://tleyden.github.io/blog/2014/05/22/configure-emacs-as-a-go-editor-from-scratch/)
