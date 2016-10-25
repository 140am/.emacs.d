# My emacs settings

> "Take it as a LISP machine, a working env, a framework to build upon my liking."


## Features

- [go-mode](https://github.com/dominikh/go-mode.el) for the [Go](https://golang.org/) 1.6+ programming language
  - go fmt on save
  - [go lint](https://github.com/golang/lint) a document with `M-x golint`
  - [godef](https://github.com/rogpeppe/godef) jump to a definition with `M-.`
- [MELPA package manager](https://melpa.org/)
- save the state of Emacs from one session to another
- auto complete, improved file & buffer navigation & more colors!

## Setup

To get the configuration, run in home directory:

    git clone --recursive https://github.com/140am/.emacs.d.git

## Install emacs on macOS

I use [Cocoa Emacs](https://emacsformacosx.com/), installed like this:

    brew cask install emacs

## Install used Emacs packages

Using `M-x package-install`:

- exec-path-from-shell
- auto-complete
- go-mode
- go-autocomplete

## Go development dependencies

Install used tooling:

```
go get -u golang.org/x/tools/cmd/...
go get -u github.com/nsf/gocode
go get -u github.com/rogpeppe/godef
go get -u github.com/golang/lint/golint
```

Assumes the following env variables are set:

- GOROOT
- GOPATH
- PATH=$PATH:$GOROOT/bin
- PATH=$PATH:$GOPATH/bin

## Customize

To customize the configuration fork this repo and edit [init.el](init.el).


## 101

Gotchas I experienced while getting familar with Emacs:

- any text you see in an Emac window is part of some buffer
- the *frame* contains a *tool bar*, *window*, *modeline* and *minibuffer*

### Keyboard Navigation

Key abbreviations:

- `M          ` Alt (used to be called Meta)
- `C          ` Control
- `S          ` Shift
- `C-x t      ` means holding both Control and x, release both, and press f

#### Help

- `C-h t      ` tutorial
- `C-h ?      ` usage of `C-h`
- `C-h i      ` info. manuals for installed packages
- `C-h m      ` help on active major mode
- `C-h c      ` view simple help on a specific command key
- `C-h k      ` view documentation on a specific command key
- `C-h f      ` describe a function
- `C-h S      ` describe a function in the Emacs Lisp manual
- `C-h v      ` describe a variable
- `C-h a      ` command apropos. list commands (callable via M-x) by keyword

#### Control

- `C-g        ` stop / abort command
- `C-x        ` character extend (followed by one character)
- `M-x        ` named command extend (followed by a long name)
- `C-u        ` update count (repeat count, command flag). example: C-v / M-v
- `M-:        ` evaluate an expression

#### Files

- `C-x C-f    ` finding a file
- `C-x C-s    ` saving a file

#### Search

- `C-s        ` search forward incremental
- `C-r        ` search backward
- `C-x C-x    ` go back to previous point
- `C-M-%`     ` find and replace one by one

#### Buffers

- `C-x C-b    ` list buffers
- `C-x b      ` switch to a buffer
- `C-x s      ` save open buffers (after confirmation)
- `C-x k      ` kill current buffer

#### View

- `C-v` / `M-v` view next / previous screen
- `C-l        ` clean screen + center around current line

#### Windows

- `C-x 0      ` close current window
- `C-x 1      ` close all but current window
- `C-x 2      ` split current window - horizontally
- `C-x 3      ` split current window - vertically
- `C-x o      ` other window switch
- `C-M-v      ` scroll alternate window

#### Undo / Redo

- `C-\        ` undo text changes

#### Macros

- `C-x (      ` start recording
- `C-x )      ` end record
- `C-x e` / `e` execute macro / repeat execute

#### Navigation

- `M<         ` beginning of file / text
- `M>         ` end of file / text

##### by character

- `C-b / M    ` backward
- `C-f / M    ` forward

##### by line / sentence

- `C-p / M    ` previous line
- `C-a / M    ` beginning line / sentence
- `C-e / M    ` end line / sentence
- `C-n / M    ` next line

#### Marking / Selecting

- `C-<SPC>    ` mark set/unset (select text)

#### Delete

- `<DEL>      ` delete the character just before the cursor
- `C-d        ` delete the next character after the cursor 
- `C-k        ` kill from the cursor position to end of line 
- `M-k        ` kill to the end of the current sentence 
- `M-<DEL>    ` kill the word immediately before the cursor 
- `M-d        ` kill the next word after the cursor 
- `C-w        ` kill text between two positions

#### Yank / Paste

- `C-y        ` yank / paste text
- `M-y        ` yank updates with previous kills

#### Mode - Go Language

- `M-.        ` follow definition


### Extended Commands `M-x`

- `shell      ` opens terminal
- `occur      ` given a regex, create buffer of matches
- `golint     ` run golint on the current file


### Resources

Links I found helpful learning Emacs:

- [How to learn Emacs](http://david.rothlis.net/emacs/tutorial.html)
- [How to learn Emacs: A visual tutorial](http://sachachua.com/begin-emacs)

#### Go Programming Language

- [Writing Go in Emacs](https://dominik.honnef.co/posts/2013/03/emacs-go-1/)
- [Configure Emacs as a Go Editor From Scratch](http://tleyden.github.io/blog/2014/05/22/configure-emacs-as-a-go-editor-from-scratch/)
