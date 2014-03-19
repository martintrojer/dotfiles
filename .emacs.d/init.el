;; C-u 0 M-x byte-recompile-directory

(setq inhibit-startup-screen t)

(if window-system
    (progn
      (scroll-bar-mode -1)
      (tool-bar-mode -1)
      (set-face-attribute 'default nil :height 140))
  (menu-bar-mode 0))

;; package stuff
(require 'package)
(add-to-list 'package-archives
             '("marmalade" .
               "http://marmalade-repo.org/packages/"))

(add-to-list 'package-archives
             '("melpa" .
               "http://melpa.milkbox.net/packages/") t)

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defun maybe-install-and-require (p)
  (when (not (package-installed-p p))
    (package-install p))
  (require p))

(maybe-install-and-require 'diminish)

;; Window management / undo stuff
;; C-c right/left
(winner-mode)

;; server
(require 'server)
(unless (server-running-p)
  (server-start))

;; browse-kill-ring
(maybe-install-and-require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)

;; ibuffer over list-buffers
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; undo-tree
(maybe-install-and-require 'undo-tree)
(diminish 'undo-tree-mode "UT")
(global-undo-tree-mode)

;; Magit key binding
(maybe-install-and-require 'magit)
(global-set-key (kbd "C-c C-g") 'magit-status)
(global-set-key (kbd "C-c C-b") 'magit-blame-mode)
(diminish 'magit-auto-revert-mode)

;; yasnippet
(maybe-install-and-require 'yasnippet)
(diminish 'yas-minor-mode " Y")
(maybe-install-and-require 'clojure-snippets)
(yas-global-mode 1)
(add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets")
(yas-load-directory "~/.emacs.d/snippets")

;; git gutter
(maybe-install-and-require 'git-gutter)
(diminish 'git-gutter-mode "GG")
;;(global-git-gutter-mode t)
(global-set-key (kbd "C-x C-g") 'git-gutter:toggle)

;; rgrep key binding
(global-set-key (kbd "C-x M-f") 'rgrep)

;; Golden Ratio
(maybe-install-and-require 'golden-ratio)
(diminish 'golden-ratio-mode "AU")
(golden-ratio-mode 1)
(add-to-list 'golden-ratio-exclude-modes "ediff-mode")

;; IDO
(ido-mode t)
(setq ido-enable-flex-matching t)
(global-set-key "\M-x"
                (lambda ()
                  (interactive)
                  (call-interactively
                   (intern (ido-completing-read "M-x " (all-completions "" obarray 'commandp))))))

;; expand region
(maybe-install-and-require 'expand-region)
(global-set-key (kbd "C-\\") 'er/expand-region)

;; comments
(global-set-key (kbd "C-c ;") 'comment-or-uncomment-region)

;; multiple cursors
(maybe-install-and-require 'multiple-cursors)
(global-set-key (kbd "C-c .") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c ,") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c M-.") 'mc/mark-all-like-this)

;; color-theme
(maybe-install-and-require 'cyberpunk-theme)
(load-theme 'cyberpunk t)

;; auto-complete
(maybe-install-and-require 'auto-complete)
(diminish 'auto-complete-mode)
(require 'auto-complete-config)
(ac-config-default)
(global-auto-complete-mode t)
(setq ac-auto-show-menu t)
(setq ac-dwim t)
(setq ac-use-menu-map t)
(setq ac-delay 0.3)
(setq ac-quick-help-delay 1)
(setq ac-quick-help-height 60)

;; Cider
(maybe-install-and-require 'cider)
(maybe-install-and-require 'cider-tracing)
(diminish 'cider-mode " Cdr")
(setq cider-repl-wrap-history t)
(setq cider-repl-history-size 1000)
(setq cider-repl-history-file "~/.emacs.d/cider-history")

;; subword
(add-hook 'cider-repl-mode-hook 'subword-mode)

;; eldoc
(diminish 'eldoc-mode "ED")
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(add-hook 'cider-repl-mode-hook 'cider-turn-on-eldoc-mode)

;; ac-nrepl
(maybe-install-and-require 'ac-nrepl)
(add-hook 'cider-mode-hook 'ac-nrepl-setup)
(add-hook 'cider-repl-mode-hook 'ac-nrepl-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'cider-repl-mode))

;; paredit
(maybe-install-and-require 'paredit)
(diminish 'paredit-mode "Pe")
(add-hook 'lisp-mode-hook 'paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'scheme-mode-hook 'paredit-mode)
(add-hook 'cider-repl-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook 'paredit-mode)

;; hl-sexp
(maybe-install-and-require 'hl-sexp)
(add-hook 'clojure-mode-hook 'hl-sexp-mode)
(add-hook 'lisp-mode-hook 'hl-sexp-mode)
(add-hook 'scheme-mode-hook 'hl-sexp-mode)
(add-hook 'emacs-lisp-mode-hook 'hl-sexp-mode)

;; idle-highlight-mode
(maybe-install-and-require 'idle-highlight-mode)
(add-hook 'clojure-mode-hook 'idle-highlight-mode)
(add-hook 'lisp-mode-hook 'idle-highlight-mode)
(add-hook 'scheme-mode-hook 'idle-highlight-mode)
(add-hook 'emacs-lisp-mode-hook 'idle-highlight-mode)

;; markdown
(maybe-install-and-require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; restclient
(maybe-install-and-require 'restclient)
(add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode))

;; show-paren-mode
(show-paren-mode)
;; match parens
(setq blink-matching-paren-distance nil)

;; Scheme; gambit / chicken / petite
;;(setq scheme-program-name "gsi -:s,d-")
(setq scheme-program-name "csi")
;;(setq scheme-program-name "petite")

;; Python
(setq python-shell-interpreter "python3")

;; OCaml
(setq save-abbrevs nil)
(diminish 'abbrev-mode)
(maybe-install-and-require 'tuareg)
(maybe-install-and-require 'utop)
(add-hook 'tuareg-mode-hook 'tuareg-imenu-set-imenu)
(setq auto-mode-alist
      (append '(("\\.ml[ily]?$" . tuareg-mode)
                ("\\.topml$" . tuareg-mode))
              auto-mode-alist))
(autoload 'utop-setup-ocaml-buffer "utop" "Toplevel for OCaml" t)
(add-hook 'tuareg-mode-hook 'utop-setup-ocaml-buffer)
(maybe-install-and-require 'merlin)
(diminish 'merlin-mode "MRL")
(add-hook 'tuareg-mode-hook 'merlin-mode)
(setq merlin-use-auto-complete-mode t)
(setq merlin-error-after-save nil)

;; Clojure
(maybe-install-and-require 'clojure-mode)
(maybe-install-and-require 'clojure-test-mode)
(setq auto-mode-alist (cons '("\\.cljs$" . clojure-mode) auto-mode-alist))

(maybe-install-and-require 'clj-refactor)
(diminish 'clj-refactor-mode)
(add-hook 'clojure-mode-hook (lambda ()
                               (clj-refactor-mode 1)
                               (cljr-add-keybindings-with-prefix "C-c C-o")))

(maybe-install-and-require 'align-cljlet)
(global-set-key (kbd "C-c C-a") 'align-cljlet)

(maybe-install-and-require 'slamhound)

;; compojure
(define-clojure-indent
	(defroutes 'defun)
	(GET 2)
	(POST 2)
	(PUT 2)
	(DELETE 2)
	(HEAD 2)
	(ANY 2)
	(context 2))

(maybe-install-and-require 'puppet-mode)
(maybe-install-and-require 'yaml-mode)

;; ------------------------
;; stuff

(global-set-key (kbd "RET") 'newline-and-indent)
(defalias 'yes-or-no-p 'y-or-n-p)

;; flyspell
(require 'flyspell)
(diminish 'flyspell-mode "FP")

;; frame title
(setq frame-title-format "%b")

;; Columns
(column-number-mode t)

;; Auto revert
(global-auto-revert-mode t)

;; scrolling
(setq scroll-step 1)
(setq scroll-error-top-bottom t)

;; truncate lines
(set-default 'truncate-lines t)

;; remove trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; spaces instead of tabs
(setq-default indent-tabs-mode nil)
(setq default-tab-width 2)
(setq tab-width 2)
(setq python-indent 3)
(setq c-basic-offset 3)
(setq c-indent-level 3)
(setq c++-tab-always-indent nil)
(setq js-indent-level 3)

;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name "places" user-emacs-directory))

;; show time
(setq display-time-24hr-format t)
(setq display-time-load-average t)
(display-time)

;; ERC
(setq erc-hide-list '("JOIN" "PART" "QUIT"))
(setq erc-nick "martintrojer")

;; jvm-mode
(maybe-install-and-require 'jvm-mode)
(jvm-mode)

;; XML pretty print
(defun pretty-print-xml-region (begin end)
  (interactive "r")
  (save-excursion
    (nxml-mode)
    (goto-char begin)
    (while (search-forward-regexp "\>[ \\t]*\<" nil t)
      (backward-char) (insert "\n"))
    (indent-region begin end))
  (message "Ah, much better!"))

;; OSX
;; Allow hash to be entered
(when (eq 'darwin system-type)

  (global-set-key (kbd "M-3") '(lambda () (interactive) (insert "#")))

  (defun copy-from-osx ()
    (shell-command-to-string "pbpaste"))

	(defun paste-to-osx (text &optional push)
    (let ((process-connection-type nil))
      (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
        (process-send-string proc text)
        (process-send-eof proc))))

  (unless (getenv "TMUX")
    (setq interprogram-cut-function 'paste-to-osx)
    (setq interprogram-paste-function 'copy-from-osx)))


;; work

(defun current-nrepl-server-buffer ()
  (let ((nrepl-server-buf (replace-regexp-in-string "connection" "server" (nrepl-current-connection-buffer))))
    (when nrepl-server-buf
      (get-buffer nrepl-server-buf))))

(defun clear-buffers ()
  (interactive)

  (cider-find-and-clear-repl-buffer)

  (with-current-buffer "test.log"
    (kill-region (point-min) (point-max))
    (save-buffer))

  (with-current-buffer (current-nrepl-server-buffer)
    (kill-region (point-min) (point-max))))

(global-set-key (kbd "C-c :") '(lambda ()
                                 (interactive)
                                 (clear-buffers)
                                 (clojure-test-run-tests)))
