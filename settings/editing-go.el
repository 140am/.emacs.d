;; go-mode settings
(defun my-go-mode-hook ()
  ;; Use goimports instead of go-fmt
  (setq gofmt-command "goimports")
  ;; Call Gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)
  ;; Godef jump key binding
  (local-set-key (kbd "M-.") 'godef-jump))
(add-hook 'go-mode-hook 'my-go-mode-hook)

;; init go-eldoc
(add-hook 'go-mode-hook 'go-eldoc-setup)

;; go-autocomplete
(defun auto-complete-for-go ()
  (auto-complete-mode 1))
(add-hook 'go-mode-hook 'auto-complete-for-go)

(with-eval-after-load 'go-mode
  (require 'go-autocomplete))

;; load go-guru
(add-to-list 'load-path "~/.emacs.d/vendor/go-mode.el")
(require 'go-guru)
;; enable identifier highlighting
(add-hook 'go-mode-hook #'go-guru-hl-identifier-mode)

;; add golint
(add-to-list 'load-path (concat (getenv "GOPATH")  "/src/github.com/golang/lint/misc/emacs"))
(require 'golint)
