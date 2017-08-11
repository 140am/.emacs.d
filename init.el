;; The init.el is responsible for installing all used packages.
;; Customizations are defined within the settings/ directory.

;; Save emac sessions.
(desktop-save-mode 1)

;; Save session customizations to file.
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; set PAGER env variable for pagination in shell
(setenv "PAGER" "/bin/cat")

;; don't create ~ suffixed backup files on saves
(setq make-backup-files nil)

;; package.el package manager
(require 'package)
;; package repositories to use
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("tromey" . "http://tromey.com/elpa/") t)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; load packages and init them
(package-initialize)

;; fetch list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

;; list of packages to install
(setq package-list '(use-package))

;; install any not already installed package
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; use system PATH and GOPATH
;; configured in settings/shell-integration.el
(use-package exec-path-from-shell :ensure t)
;; enable auto-completion extension
;; https://github.com/auto-complete/auto-complete
(use-package auto-complete :ensure t)
;; go-mode
(use-package go-mode :ensure t)
(use-package go-eldoc :ensure t)
(use-package go-autocomplete :ensure t)
(use-package tagedit :ensure t)
;; colorful parenthesis matching
(use-package rainbow-delimiters :ensure t)
;; git interface
(use-package magit :ensure t)
;; Enhances M-x to allow easier execution of commands. Provides
;; a filterable list of possible commands in the minibuffer
;; http://www.emacswiki.org/emacs/Smex
(use-package smex :ensure t)
(use-package php-mode :ensure t)
(use-package protobuf-mode :ensure t)
;; makes handling lisp expressions much, much easier
;; Cheatsheet: http://www.emacswiki.org/emacs/PareditCheatsheet
(use-package paredit :ensure t)
;; key bindings and code colorization for Clojure
;; https://github.com/clojure-emacs/clojure-mode
(use-package clojure-mode :ensure t)
;; extra syntax highlighting for clojure
(use-package clojure-mode-extra-font-locking :ensure t)
;; integration with a Clojure REPL
;; https://github.com/clojure-emacs/cider
(use-package cider :ensure t)
;; allow ido usage in as many contexts as possible
(use-package ido-completing-read+ :ensure t)
;; project navigation
(use-package projectile :ensure t)

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package web-mode
  :ensure t
  :mode (("\\.html?\\'" . web-mode)
	 ("\\.jsx\\'" . web-mode)
	 ; ruby on rails templates
	 ("\\.html\\.erb\\'" . web-mode)
	 ; mustache templates
	 ("\\.mustache\\'" . web-mode)
	 ; jinja templates
	 ("\\.jinja\\'" . web-mode)
	 ; asp.net
	 ("\\.as[cp]x\\'" . web-mode)
	 ("\\.[agj]sp\\'" . web-mode)
	 ; php / drupal templates
	 ("\\.tpl\\.php\\'" . web-mode))
  :config (progn
	    (add-hook 'web-mode-hook
		      (lambda ()
			; Auto-pairing			
			(setq web-mode-enable-auto-pairing t)
			; CSS colorization
			(setq web-mode-enable-css-colorization t)
			; HTML element offset indentation
			(setq web-mode-markup-indent-offset 2)
			; CSS offset indentation
			(setq web-mode-css-indent-offset 2)
			; script/code offset indentation			
			(setq web-mode-code-indent-offset 2)
			(setq web-mode-style-padding 2)
			(setq web-mode-script-padding 2)))))

;; Colore theme to use;
(use-package ample-theme :ensure t)

;; Directory containing organized customizations.
(add-to-list 'load-path "~/.emacs.d/settings")

;; Use system environment variables.
(load "shell-integration.el")
;; These customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.
(load "navigation.el")
;; These customizations change the way emacs looks and disable/enable
;; some user interface elements
(load "ui.el")
;; These customizations make editing a bit nicer.
(load "editing.el")
;; Language specific editing customizations.
(load "editing-elisp.el")
(load "editing-go.el")
(load "editing-org.el")
(load "editing-js.el")
(load "editing-php.el")
(load "editing-clojure.el")
;; Hard-to-categorize customizations.
(load "misc.el")
