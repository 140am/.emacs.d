;; Sets up exec-path-from shell
;; https://github.com/purcell/exec-path-from-shell
(if (display-graphic-p)
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs
   '("PATH"))
  (exec-path-from-shell-copy-envs
   '("GOPATH")))

;; use system $PATH
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$"
                          ""
                          (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq eshell-path-env path-from-shell) ; for eshell users
    (setq exec-path (split-string path-from-shell path-separator))))
(when window-system (set-exec-path-from-shell-PATH))

;; use system $GOPATH
(defun set-gopath-from-shell-GOPATH ()
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$"
                          ""
                          (shell-command-to-string "$SHELL --login -i -c 'echo $GOPATH'"))))
    (setenv "GOPATH" path-from-shell)))
(when window-system (set-gopath-from-shell-GOPATH))
