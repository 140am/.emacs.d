; ui minimalism
(menu-bar-mode -1)

(if (display-graphic-p)
  (progn
    (tool-bar-mode -1)
    (scroll-bar-mode -1)
    (menu-bar-mode 1)
    (setq ns-auto-hide-menu-bar t)
))


; highlight current line
(global-hl-line-mode 1)

; don't create ~ suffixed backup files on saves
(setq make-backup-files nil)

; add MELPA to package manager
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)

; load packages and init them
(package-initialize)

; list of packages to install
(setq package-list '(use-package))

; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

; install additional packages with `use-package`
(use-package exec-path-from-shell
  :ensure t)

(use-package auto-complete
  :ensure t)

(use-package go-mode
  :ensure t)

(use-package go-eldoc
  :ensure t)

(use-package go-autocomplete
  :ensure t)

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

(use-package php-mode
  :ensure t)

(use-package protobuf-mode
  :ensure t)

; set color theme
(use-package ample-theme
  :init (progn (load-theme 'ample t t)
	       (load-theme 'ample-flat t t)
	       (load-theme 'ample-light t t)
	       (enable-theme 'ample))
  :ensure t)

; used packages
(custom-set-variables
 '(package-selected-packages
   (quote
    (php-mode protobuf-mode exec-path-from-shell go-autocomplete go-mode))))
(custom-set-faces
 )

; save emac sessions
(desktop-save-mode 1)

; enable PHP mode based on extension
(autoload 'php-mode "php-mode" "Major mode for editing php code." t)
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))

; set PAGER env variable for pagination in shell
(setenv "PAGER" "/bin/cat")

; use system $PATH
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$"
                          ""
                          (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq eshell-path-env path-from-shell) ; for eshell users
    (setq exec-path (split-string path-from-shell path-separator))))
(when window-system (set-exec-path-from-shell-PATH))

; use system $GOPATH
(defun set-gopath-from-shell-GOPATH ()
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$"
                          ""
                          (shell-command-to-string "$SHELL --login -i -c 'echo $GOPATH'"))))
    (setenv "GOPATH" path-from-shell)))
(when window-system (set-gopath-from-shell-GOPATH))

; go-mode settings
(defun my-go-mode-hook ()
  ; Use goimports instead of go-fmt
  (setq gofmt-command "goimports")
  ; Call Gofmt before saving                                                    
  (add-hook 'before-save-hook 'gofmt-before-save)
  ; Godef jump key binding                                                      
  (local-set-key (kbd "M-.") 'godef-jump))
(add-hook 'go-mode-hook 'my-go-mode-hook)

; init go-eldoc
(add-hook 'go-mode-hook 'go-eldoc-setup)

; auto-complete
(require 'auto-complete-config)
(ac-config-default)

; go-autocomplete
(defun auto-complete-for-go ()
  (auto-complete-mode 1))
(add-hook 'go-mode-hook 'auto-complete-for-go)

(with-eval-after-load 'go-mode
  (require 'go-autocomplete))

; ido-mode for better find file / switch buffer
(ido-mode 1)

; confirm with "y" instead of "yes"
(defalias 'yes-or-no-p 'y-or-n-p)

; perm enable el documentation mode
(eldoc-mode 1)

; improve reading of flow in code
(show-paren-mode 1)

; use functions instead of tags for elisp file lookups
(define-key emacs-lisp-mode-map
  (kbd "M-.") 'find-function-at-point)

; add golint
(add-to-list 'load-path (concat (getenv "GOPATH")  "/src/github.com/golang/lint/misc/emacs"))
(require 'golint)

; org-mode
(require 'org)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cl" 'org-store-link)
(setq org-log-done t)
