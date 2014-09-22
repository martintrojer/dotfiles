;; =============================================================
;; prelude

;; C-u 0 M-x byte-recompile-directory

(setq inhibit-startup-screen t)

(if window-system
    (progn
      (scroll-bar-mode -1)
      (tool-bar-mode -1)
      (set-face-attribute 'default nil :height 140))
  (menu-bar-mode 0))

;; =============================================================
;; package

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defun maybe-install-and-require (p)
  (when (not (package-installed-p p))
    (package-install p))
  (require p))

(maybe-install-and-require 'diminish)

;; =============================================================
;; Major modes

;; Clojure
(maybe-install-and-require 'clojure-mode)
(setq auto-mode-alist (cons '("\\.cljs$" . clojure-mode) auto-mode-alist))
(setq inferior-lisp-program "lein repl")
(add-hook 'clojure-mode-hook
          '(lambda ()
             (define-key clojure-mode-map
               "\C-c\C-k"
               '(lambda ()
                  (interactive)
                  (let ((current-point (point)))
                    (goto-char (point-min))
                    (let ((ns-idx (re-search-forward clojure-namespace-name-regex nil t)))
                      (when ns-idx
                        (goto-char ns-idx)
                        (let ((sym (symbol-at-point)))
                          (message (format "Loading %s ..." sym))
                          (lisp-eval-string (format "(require '%s :reload)" sym))
                          (lisp-eval-string (format "(in-ns '%s)" sym)))))
                    (goto-char current-point))))))

;; Tuareg / OCaml
(setq save-abbrevs nil)
(diminish 'abbrev-mode)
(maybe-install-and-require 'tuareg)
(add-hook 'tuareg-mode-hook 'tuareg-imenu-set-imenu)
(setq auto-mode-alist
      (append '(("\\.ml[ily]?$" . tuareg-mode)
                ("\\.topml$" . tuareg-mode))
              auto-mode-alist))

;; markdown
(maybe-install-and-require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; Puppet
(maybe-install-and-require 'puppet-mode)

;; Yaml
(maybe-install-and-require 'yaml-mode)

;; Restclient
(maybe-install-and-require 'restclient)
(add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode))

;; HTML
(setq auto-mode-alist (cons '("\\.mustache$" . html-mode) auto-mode-alist))

;; =============================================================
;; Minor modes

;; Cider
(maybe-install-and-require 'cider)
(diminish 'cider-mode " Cdr")
(setq cider-repl-wrap-history t)
(setq cider-repl-history-size 1000)
(setq cider-show-error-buffer 'except-in-repl)
(setq cider-repl-history-file "~/.emacs.d/cider-history")
(add-hook 'cider-repl-mode-hook 'subword-mode)
(global-set-key (kbd "C-c C-s") 'clojure-toggle-keyword-string)

;; clj-refactor
(maybe-install-and-require 'clj-refactor)
(diminish 'clj-refactor-mode)
(add-hook 'clojure-mode-hook (lambda ()
                               (clj-refactor-mode 1)
                               (cljr-add-keybindings-with-prefix "C-c C-o")))

;; align-cljlet
(maybe-install-and-require 'align-cljlet)
(global-set-key (kbd "C-c C-a") 'align-cljlet)

;; paredit
(maybe-install-and-require 'paredit)
(diminish 'paredit-mode "Pe")
(add-hook 'lisp-mode-hook 'paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'scheme-mode-hook 'paredit-mode)
(add-hook 'cider-repl-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook 'paredit-mode)

;; projectile
(maybe-install-and-require 'projectile)
(setq projectile-mode-line '(:eval (format " P[%s]" (projectile-project-name))))
(add-hook 'clojure-mode-hook 'projectile-mode)
(add-hook 'ruby-mode-hook 'projectile-mode)

;; utop / OCaml
(maybe-install-and-require 'utop)
(autoload 'utop-setup-ocaml-buffer "utop" "Toplevel for OCaml" t)
(add-hook 'tuareg-mode-hook 'utop-setup-ocaml-buffer)

;; merlin / OCaml
(maybe-install-and-require 'merlin)
(diminish 'merlin-mode "MRL")
(add-hook 'tuareg-mode-hook 'merlin-mode)
(setq merlin-use-auto-complete-mode t)
(setq merlin-error-after-save nil)

;; Magit
(maybe-install-and-require 'magit)
(global-set-key (kbd "C-c C-g") 'magit-status)
(global-set-key (kbd "C-c C-b") 'magit-blame-mode)
(diminish 'magit-auto-revert-mode)

;; git gutter
(maybe-install-and-require 'git-gutter)
(diminish 'git-gutter-mode "GG")
;;(global-git-gutter-mode t)
(global-set-key (kbd "C-x C-g") 'git-gutter:toggle)

;; silver searcher
(maybe-install-and-require 'ag)
(setq ag-highlight-search t)
(setq ag-reuse-buffers t)
(defun ag-search (string file-regex directory)
  (interactive (list (read-from-minibuffer "Search string: " (ag/dwim-at-point))
                     (read-from-minibuffer "In filenames matching PCRE: " (ag/buffer-extension-regex))
                     (read-directory-name "Directory: " (ag/project-root default-directory))))
  (ag/search string directory :file-regex file-regex))
(global-set-key (kbd "C-x M-f") 'ag-search)

;; eldoc
(diminish 'eldoc-mode "ED")
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(add-hook 'cider-repl-mode-hook 'cider-turn-on-eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)

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

;; Golden Ratio
(maybe-install-and-require 'golden-ratio)
(diminish 'golden-ratio-mode "AU")
(golden-ratio-mode 1)
(add-to-list 'golden-ratio-exclude-modes "ediff-mode")

;; undo-tree
(maybe-install-and-require 'undo-tree)
(diminish 'undo-tree-mode "UT")
(global-undo-tree-mode)

;; yasnippet
(maybe-install-and-require 'yasnippet)
(diminish 'yas-minor-mode " Y")
(maybe-install-and-require 'clojure-snippets)
(yas-global-mode 1)
(add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets")
(yas-load-directory "~/.emacs.d/snippets")

;; company mode
(maybe-install-and-require 'company)
(diminish 'company-mode)
(add-hook 'after-init-hook 'global-company-mode)

;; browse-kill-ring
(maybe-install-and-require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)

;; multiple cursors
(maybe-install-and-require 'multiple-cursors)
(global-set-key (kbd "C-c .") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c ,") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c M-.") 'mc/mark-all-like-this)

;; IDO
(maybe-install-and-require 'ido-ubiquitous)
(ido-mode t)
(ido-ubiquitous)
(setq ido-enable-flex-matching t)
(global-set-key "\M-x"
                (lambda ()
                  (interactive)
                  (call-interactively
                   (intern (ido-completing-read "M-x " (all-completions "" obarray 'commandp))))))

;; expand region
(maybe-install-and-require 'expand-region)
(global-set-key (kbd "C-\\") 'er/expand-region)

;; yagist
(maybe-install-and-require 'yagist)
(maybe-install-and-require 'kaesar)
(setq yagist-encrypt-risky-config t)

;; flyspell
(require 'flyspell)
(diminish 'flyspell-mode "FP")

;; linum
(if window-system
  (setq linum-format "%d")
  (setq linum-format "%d "))
(setq linum-modes '(clojure-mode emacs-lisp-mode tuareg-mode puppet-mode ruby-mode markdown-mode python-mode js-mode css-mode c-mode-common))
(--each linum-modes (add-hook (intern (s-concat (symbol-name it) "-hook")) 'linum-mode))

;; show time
(setq display-time-24hr-format t)
(setq display-time-load-average t)
(display-time)

;; jvm-mode
(maybe-install-and-require 'jvm-mode)
(jvm-mode)

;; recentf mode
(recentf-mode)
(setq recentf-max-menu-items 25)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

(winner-mode)       ;; C-c right/left
(show-paren-mode)
(global-auto-revert-mode t)
(column-number-mode t)

;; =============================================================
;; Color theme

(if window-system
    (progn
      (maybe-install-and-require 'flatland-theme)
      (load-theme 'flatland t))
  (progn
      (maybe-install-and-require 'cyberpunk-theme)
      (load-theme 'cyberpunk t)))

;; =============================================================
;; Key bindings

;; ibuffer over list-buffers
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; comments
(global-set-key (kbd "C-c ;") 'comment-or-uncomment-region)

;; better search
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

(global-set-key (kbd "RET") 'newline-and-indent)

;; =============================================================
;; Mode Settings

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

;; Scheme; gambit / chicken / petite
;;(setq scheme-program-name "gsi -:s,d-")
(setq scheme-program-name "csi")
;;(setq scheme-program-name "petite")

;; Python
(setq python-shell-interpreter "python3")

;; ERC
(setq erc-hide-list '("JOIN" "PART" "QUIT"))
(setq erc-nick "martintrojer")
(setq erc-server "irc.freenode.org")
(erc-spelling-mode 1)

;; =============================================================
;; Settings

(setq frame-title-format "%b")
(set-default 'truncate-lines t)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq scroll-step 1)
(setq scroll-error-top-bottom t)
(blink-cursor-mode -1)

;; remove trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; uniquify
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; match parens
(setq blink-matching-paren-distance nil)

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

;; server
(require 'server)
(unless (server-running-p)
  (server-start))

;; =============================================================
;; Handy functions

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

;; =============================================================
;; OSX

;; Allow hash to be entered
(when (eq 'darwin system-type)

  (global-set-key (kbd "M-3") '(lambda () (interactive) (insert "#")))

  (maybe-install-and-require 'exec-path-from-shell)
  (exec-path-from-shell-initialize)

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