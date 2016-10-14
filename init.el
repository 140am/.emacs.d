; register MELPA package directory
(package-initialize)

; set color theme
(load-theme 'leuven t)

; don't create ~ suffixed backup files on saves
(setq make-backup-files nil)

; add MELPA to package manager
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)

; used packages
(custom-set-variables
 '(package-selected-packages (quote (go-autocomplete go-mode exec-path-from-shell))))
(custom-set-faces
 )

; save emac sessions
(desktop-save-mode 1)

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


; go-mode settings
(defun my-go-mode-hook ()
  ; Use goimports instead of go-fmt
  (setq gofmt-command "goimports")
  ; Call Gofmt before saving                                                    
  (add-hook 'before-save-hook 'gofmt-before-save)
  ; Godef jump key binding                                                      
  (local-set-key (kbd "M-.") 'godef-jump))
(add-hook 'go-mode-hook 'my-go-mode-hook)

; auto-complete
(require 'auto-complete-config)
(ac-config-default)

; go-autocomplete
(defun auto-complete-for-go ()
  (auto-complete-mode 1))
(add-hook 'go-mode-hook 'auto-complete-for-go)

(with-eval-after-load 'go-mode
  (require 'go-autocomplete))
