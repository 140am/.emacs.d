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

## Keyboard Navigation 101

- `C          ` : often used for basic units (character, line)
- `M          ` : often used for language specific things (word, sentences, paragraph)

### Help

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

### Control

- `C-g        ` stop / abort command
- `C-x        ` character extend (followed by one character)
- `M-x        ` named command extend (followed by a long name)
- `C-u        ` update count (repeat count, command flag). example: C-v / M-v

### Files

- `C-x C-f    ` finding a file
- `C-x C-s    ` saving a file

### Search

- `C-s        ` search forward incremental
- `C-r        ` search backward

### Buffers

- `C-x C-b    ` list buffers
- `C-x b      ` switch to a buffer
- `C-x s      ` save open buffers (after confirmation)
- `C-x k      ` kill current buffer

### View

- `C-v` / `M-v` view next / previous screen
- `C-l        ` clean screen + center around current line

### Windows

- `C-x 0      ` close current window
- `C-x 1      ` close all but current window
- `C-x 2      ` split current window - horizontally
- `C-x 3      ` split current window - vertically
- `C-x o      ` other window switch
- `C-M-v      ` scroll alternate window

### Undo / Redo

- `C-\        ` undo text changes

### Macros

- `C-x (      ` start recording
- `C-x )      ` end record
- `C-x e` / `e` execute macro / repeat execute

### Navigation

- `M<         ` beginning of file / text
- `M>         ` end of file / text

### Text

#### by character

- `C-b / M    ` backward
- `C-f / M    ` forward

#### by line / sentence

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

### Go-Mode

- `M-.        ` follow definition