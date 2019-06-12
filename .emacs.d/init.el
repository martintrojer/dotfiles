;;; init.el -- Martin's emacs config

;;; Commentary:

;;; Code:

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
	     '("melpa" . "https://melpa.org/packages/") t)

(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(when (not (package-installed-p 'use-package))
  (package-install 'use-package))
(require 'use-package)

(use-package diminish
  :ensure t
  :defer nil
  :pin melpa-stable)

(add-to-list 'load-path "~/.emacs.d/site-lisp")

(server-start)

;; =============================================================
;; Major modes

;; objc
(add-to-list 'auto-mode-alist '("\\.m\\'" . c-mode))
(add-to-list 'auto-mode-alist '("\\.mm\\'" . c++-mode))

;; Org
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-cc" 'org-capture)
;; C-c C-l insert link
;; C-c C-o open link
(setq org-cycle-include-plain-lists 'integrate)
(setq org-log-done 'time)
(custom-set-variables
 '(org-agenda-span 'fortnight))

(defun all-tags ()
  (interactive)
  (let ((buf-name "*ALL TAGS*"))
    (with-current-buffer
        (get-buffer-create buf-name)
      (progn
        (read-only-mode -1)
        (erase-buffer)
        (insert (mapconcat 'identity
                           (sort (mapcar
                                  (lambda (tag)
                                    (substring-no-properties (car tag)))
                                  (org-global-tags-completion-table))
                                 'string<)
                           "\n")
                )
        (read-only-mode 1)))
    (switch-to-buffer-other-window buf-name)))

(require 'org-drill)
(use-package org-drill-table
  :ensure t
  :defer t)

(use-package org-bookmark-heading
  :ensure t)

(use-package bm
  :ensure t
  :init
  (if window-system
      (progn
        (setq bm-highlight-style 'bm-highlight-only-fringe)
        (setq bm-marker 'bm-marker-left)))
  :bind
  ("<f2>" . bm-next)
  ("<S-f2>" . bm-toggle))

;; Clojure
(use-package clojure-mode
  :ensure t
  :defer t
  :pin melpa-stable
  :config
  (setq safe-local-variable-values
	(quote
	 ((eval define-clojure-indent
		(snippet
		 (quote defun))
		(template
		 (quote defun)))))))

;; Cider
(use-package cider
  :ensure t
  :defer t
  :pin melpa-stable
  :diminish (cider-mode . "Cdr")
  :config
  (setq cider-repl-history-file "~/.emacs.d/cider-history")
  (setq cider-font-lock-dynamically '(macro core function var))
  (setq cider-repl-use-pretty-printing nil)
  (setq cider-repl-use-clojure-font-lock t)
  (setq cider-repl-result-prefix ";; => ")
  (setq cider-repl-wrap-history t)
  (setq cider-repl-history-size 3000)
  (setq cider-show-error-buffer 'except-in-repl)
  (setq cider-repl-display-help-banner nil)
  (setq cider-inject-dependencies-at-jack-in nil)
  (add-hook 'cider-mode-hook #'eldoc-mode))

;; Go lang
(use-package go-mode
  :ensure t
  :defer t
  :pin melpa-stable)
(use-package go-eldoc
  :ensure t
  :defer t
  :pin melpa-stable)

;; Ocaml
;; opam install tuareg merlin ocp-indent utop ocamlformat
(use-package caml
  :ensure t
  :defer t
  :pin melpa-stable)
(use-package tuareg
  :ensure t
  :defer t
  :pin melpa-stable
  :config
  (load "~/.opam/infer-4.06.1+flambda/share/emacs/site-lisp/tuareg-site-file")
  (add-hook 'tuareg-mode-hook #'electric-pair-local-mode)
  ;; (add-hook 'tuareg-mode-hook 'tuareg-imenu-set-imenu)
  (setq auto-mode-alist
        (append '(("\\.ml[ily]?$" . tuareg-mode)
                  ("\\.topml$" . tuareg-mode))
                auto-mode-alist)))

;; Merlin configuration
(use-package merlin
  :ensure t
  :defer t
  :pin melpa-stable
  :bind (("C-c C-\\" . merlin-pop-stack)
         ("C-c C-o"  . merlin-document)
         ("C-c C-m"  . infer-mk)
         ("C-c C-y"  . infer-analyze))
  :config
  (add-hook 'tuareg-mode-hook 'merlin-mode)
  (add-hook 'reason-mode-hook 'merlin-mode)
  (add-hook 'merlin-mode-hook #'company-mode)
  (setq merlin-error-after-save nil))

(use-package ocp-indent
  :ensure t
  :defer t
  :pin melpa-stable)

;; utop configuration
(use-package utop
  :ensure t
  :defer t
  :config
  (autoload 'utop-minor-mode "utop" "Minor mode for utop" t)
  (add-hook 'tuareg-mode-hook 'utop-minor-mode)
  )

;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line


;; Haskell
;; make sure there is no local ghc in the path!
;; stack setup
;;  stack install hlint hindent hasktags stylish-haskell ghcid intero

(use-package haskell-mode
  :ensure t
  :pin melpa
  :config
  (require 'haskell-interactive-mode)
  (setq haskell-interactive-popup-errors nil
        haskell-process-suggest-hoogle-imports t
        haskell-process-suggest-remove-import-lines t
        haskell-process-auto-import-loaded-modules t
        hindent-reformat-buffer-on-save nil
        haskell-stylish-on-save t
        haskell-tags-on-save nil
        haskell-process-log t
        haskell-process-args-stack-ghci '("--ghci-options=-ferror-spans"))
  (add-hook 'haskell-mode-hook
            '(lambda ()
               (setq-local completion-at-point-functions '(haskell-process-completions-at-point))
               (intero-mode)
               (hindent-mode)
               (haskell-indentation-mode t)
               (flycheck-add-next-checker 'intero '(warning . haskell-hlint))
               ;; (interactive-haskell-mode t)
               ;; (structured-haskell-mode t)
               ))
  (define-key haskell-mode-map "\C-c\C-h" 'hoogle))

(defun intero-repl-add (&optional prompt-options)
  "Add the current file to the target set in the REPL.
If PROMPT-OPTIONS is non-nil, prompt with an options list."
  (interactive "P")
  (save-buffer)
  (let ((file (intero-localize-path (intero-buffer-file-name))))
    (intero-with-repl-buffer prompt-options
      (comint-simple-send
       (get-buffer-process (current-buffer))
       (concat ":add " file)))))

(use-package intero
  :ensure t
  :defer t
  :pin melpa
  :bind (("C-c C-k" . intero-repl-add)))

(use-package hindent
  :ensure t
  :defer t
  :pin melpa-stable)

(use-package shm
  :ensure t
  :defer t
  :pin melpa-stable)

;; Elm
(use-package elm-mode
  :ensure t
  :defer t
  :pin melpa-stable
  :init (setq elm-indent-offset 4))

;; Purescript
(use-package purescript-mode
  :ensure t
  :defer t
  :pin melpa
  :config
  (require 'psc-ide)
  (add-hook 'purescript-mode-hook
            (lambda ()
              (repl-toggle-mode)
              (inferior-psci-mode)
              (psc-ide-mode)
              (flycheck-mode)
              (turn-on-purescript-indentation))))

(use-package psci
  :ensure t
  :defer t
  :pin melpa)

(use-package psc-ide
  :ensure t
  :defer t
  :pin melpa
  :diminish (psc-ide-mode . "Pi"))

(use-package repl-toggle
  :ensure t
  :defer t
  :pin melpa-stable
  :config
  (add-to-list 'rtog/mode-repl-alist '(purescript-mode . psci)))

;; Markdown
(use-package markdown-mode
  :ensure t
  :pin melpa-stable
  :mode "\\.md\\'")

;; Yaml
(use-package yaml-mode
  :ensure t
  :defer t
  :pin melpa-stable)

;; Docker
(use-package dockerfile-mode
  :ensure t
  :defer t
  :pin melpa-stable)

;; Mustache
(use-package mustache-mode
  :ensure t
  :defer t
  :pin melpa-stable
  :mode "\\.mustache\\'")

;; Dired
(use-package dired
  :config
  (setq dired-dwim-target t)
  (setq dired-listing-switches "-alh")
  (define-key dired-mode-map "e" (lambda () (interactive) (eww-open-file (dired-get-file-for-visit))))
  (defun kill-dired-buffers ()
    (interactive)
    (mapc (lambda (buffer)
	    (when (eq 'dired-mode (buffer-local-value 'major-mode buffer))
	      (kill-buffer buffer)))
	  (buffer-list)))
  (add-hook 'dired-mode-hook
	    '(lambda ()
	       (define-key dired-mode-map "\C-x\M-q" 'wdired-change-to-wdired-mode)
	       (define-key dired-mode-map "\C-x\M-f" 'find-name-dired))))

;; =============================================================
;; Minor modes

;; eldoc
(use-package eldoc
  :diminish (eldoc-mode . "ED")
  :config
  (add-hook 'emacs-lisp-mode-hook 'eldoc-mode))

;; Magit
(use-package magit
  :ensure t
  :pin melpa-stable
  :bind (("C-c C-g" . magit-status)
         ("C-c C-b" . magit-blame-mode))
  :config
  (setq magit-last-seen-setup-instructions "1.4.0")
  (setq magit-revert-buffers 'silent)
  (setq magit-diff-refine-hunk t))

(use-package monky
  :ensure t
  :pin melpa)

;; Paredit
(use-package paredit
  :ensure t
  :pin melpa-stable
  :diminish (paredit-mode . "Pe")
  :config
  (add-hook 'lisp-mode-hook 'paredit-mode)
  (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
  (add-hook 'scheme-mode-hook 'paredit-mode)
  (add-hook 'cider-repl-mode-hook 'paredit-mode)
  (add-hook 'clojure-mode-hook 'paredit-mode))

;; Smartparens
(use-package smartparens
  :ensure t
  :pin melpa-stable
  :config
  (sp-use-paredit-bindings)
  (sp-pair "'" nil :actions :rem)
  (add-hook 'haskell-mode-hook 'smartparens-mode)
  (add-hook 'haskell-interactive-mode-hook 'smartparens-mode)
  (add-hook 'ruby-mode-hook 'smartparens-mode)
  (add-hook 'inf-clojure-mode-hook 'smartparens-mode))

;; Company
(use-package company
  :ensure t
  :pin melpa-stable
  :diminish company-mode
  :config
  (global-company-mode))

;; IDO
(use-package ido-completing-read+
  :ensure t
  :pin melpa-stable
  :config
  (ido-mode t)
  (setq ido-enable-flex-matching t)
  (global-set-key "\M-x"
                (lambda ()
                  (interactive)
                  (call-interactively
                   (intern (ido-completing-read "M-x " (all-completions "" obarray 'commandp)))))))

;; Projectile
(use-package projectile
  :ensure t
  :pin melpa-stable
  :config
  (setq projectile-mode-line '(:eval (format " P[%s]" (projectile-project-name))))
  (add-hook 'clojure-mode-hook 'projectile-mode)
  (add-hook 'ruby-mode-hook 'projectile-mode))

;; Yasnippet
(use-package yasnippet
  :ensure t
  :pin melpa-stable
  :diminish (yas-minor-mode . " Y")
  :config
  (yas-global-mode 1)
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets")
  (yas-load-directory "~/.emacs.d/snippets"))

;; Silver searcher
;; (use-package ag
;;   :ensure t
;;   :pin melpa-stable
;;   :bind ("C-x M-f" . mt-ag-search)
;;   :config
;;   (setq ag-highlight-search t)
;;   (setq ag-reuse-buffers t)
;;   ;; (custom-set-variables
;;   ;;  '(ag-project-root-function (lambda (path) default-directory)))
;;   (defun mt-ag-search (string file-regex directory)
;;     (interactive (list (read-from-minibuffer "Search string: " (ag/dwim-at-point))
;; 		       (read-from-minibuffer "In filenames matching PCRE: " (ag/buffer-extension-regex))
;; 		       (read-directory-name "Directory: " (ag/project-root default-directory))))
;;     (ag/search string directory :file-regex file-regex)))

;; Ripgrep
(use-package deadgrep
  :ensure t
  :bind ("C-x M-f" . deadgrep))

;; vdiff
(use-package vdiff
  :ensure t)

;; Linum
(use-package linum
  :config
  (if window-system
      (setq linum-format "%d")
    (setq linum-format "%d "))
  (setq linum-modes '(clojure-mode emacs-lisp-mode tuareg-mode puppet-mode ruby-mode markdown-mode python-mode
                                   go-mode haskell-mode purescripe-mode elm-mode purescript-mode js-mode html-mode css-mode c-mode-common))
  (require 's)
  (--each linum-modes (add-hook (intern (s-concat (symbol-name it) "-hook")) 'linum-mode)))

;; Flycheck
(use-package flycheck
  :ensure t
  :pin melpa-stable
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

;; Flyspell
(use-package flyspell
  :ensure t
  :pin melpa-stable
  :bind (("C-c £" . flyspell-buffer))
  :config
  (add-hook 'org-mode-hook 'flyspell-buffer)
  (add-hook 'org-mode-hook 'flyspell-mode)
  (add-hook 'markdown-mode-hook 'flyspell-buffer)
  (add-hook 'markdown-mode-hook 'flyspell-mode))

;; idle-highlight-mode
(use-package idle-highlight-mode
  :ensure t
  :pin melpa-stable
  :config
  (add-hook 'clojure-mode-hook 'idle-highlight-mode)
  (add-hook 'lisp-mode-hook 'idle-highlight-mode)
  (add-hook 'scheme-mode-hook 'idle-highlight-mode)
  (add-hook 'emacs-lisp-mode-hook 'idle-highlight-mode)
  (add-hook 'haskell-mode-hook 'idle-highlight-mode))

;; Golden ratio
(use-package golden-ratio
  :ensure t
  :pin melpa-stable
  :diminish (golden-ratio-mode . "AU")
  :config
  (golden-ratio-mode 1)
  ;; (setq golden-ratio-auto-scale t)
  (setq split-width-threshold 200)
  (setq split-height-threshold 120)
  (add-to-list 'golden-ratio-exclude-modes "ediff-mode")
  (add-to-list 'golden-ratio-exclude-modes "vdiff-mode"))

;; undo-tree
(use-package undo-tree
  :ensure t
  :diminish (undo-tree-mode . "UT")
  :config
  (global-undo-tree-mode))

;; browse-kill-ring
(use-package browse-kill-ring
  :ensure t
  :pin melpa-stable
  :config
  (browse-kill-ring-default-keybindings))

;; Multiple cursors
(use-package multiple-cursors
  :ensure t
  :pin melpa-stable
  :bind (("C-c ." . mc/mark-next-like-this)
         ("C-c ," . mc/mark-previous-like-this)
         ("C-c M-." . mc/mark-all-like-this)))

;; Expand region
(use-package expand-region
  :ensure t
  :pin melpa
  :bind ("C-\\" . er/expand-region))

;; Yagist
(use-package yagist
  :ensure t
  :defer t
  :pin melpa-stable
  :config
  (setq yagist-encrypt-risky-config t))
(use-package kaesar
  :ensure t
  :pin melpa-stable)

;; Git gutter
(use-package git-gutter
  :ensure t
  :pin melpa-stable
  :diminish (git-gutter-mode . "GG")
  :bind ("C-x C-g" . git-gutter:toggle))

;; inf-ruby
(use-package inf-ruby
  :ensure t
  :pin melpa-stable
  :config
  (add-hook 'ruby-mode-hook 'inf-ruby-minor-mode))

;; Avy
(use-package avy
  :ensure t
  :pin melpa-stable
  :bind (("M-g f" . avy-goto-line)
         ("M-g w" . any-goto-word-1)))

;; JVM
;; (use-package jvm-mode
;;   :ensure t
;;   :pin melpa-stable
;;   :config
;;   (setq jvm-mode-line-string " jvm[%d]")
;;   (jvm-mode)
;;   )

(require 'df-mode "~/.emacs.d/df-mode.el")
(df-mode)

;; buffer-move
(use-package buffer-move
  :ensure t
  :pin melpa-stable
  :bind (("C-c <C-right>" . mt-move-right)
         ("C-c <C-left>" . mt-move-left))
  :config
  (defun mt-move-right ()
    (interactive)
    (buf-move-right) (golden-ratio))
  (defun mt-move-left ()
    (interactive)
    (buf-move-left) (golden-ratio)))

;; align-cljlet
(use-package align-cljlet
  :ensure t
  :config
  (add-hook 'clojure-mode-hook
          '(lambda ()
             (define-key clojure-mode-map "\C-c\C-y" 'align-cljlet))))

;; clj-refactor
(use-package clj-refactor
  :ensure t
  :pin melpa-stable
  :diminish clj-refactor-mode
  :config
  (add-hook 'clojure-mode-hook (lambda ()
                               (clj-refactor-mode 1)
                               (cljr-add-keybindings-with-prefix "C-c C-o"))))

;; scratch buffer
(use-package persistent-scratch
  :ensure t
  :pin melpa
  :config
  (persistent-scratch-setup-default))

;; =============================================================
;; Colors

(use-package zenburn-theme
  :ensure t)

;; =============================================================
;; Misc config

(let ((local-path (expand-file-name "~/.local/bin")))
  (setenv "PATH" (concat local-path ":" (getenv "PATH")))
  (add-to-list 'exec-path local-path))

(setq default-buffer-file-coding-system 'utf-8-unix)

(recentf-mode)
(setq recentf-max-menu-items 25)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

(show-paren-mode)
(global-auto-revert-mode t)
(column-number-mode t)

;; comments
(global-set-key (kbd "C-c ;") 'comment-or-uncomment-region)

;; better search
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; windmove
(global-set-key (kbd "C-x <left>")  'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <up>")    'windmove-up)
(global-set-key (kbd "C-x <down>")  'windmove-down)

(global-set-key (kbd "RET") 'newline-and-indent)

(setq frame-title-format "%b")
(set-default 'truncate-lines t)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq scroll-step 1)
(setq scroll-error-top-bottom t)
(blink-cursor-mode -1)
(setq ring-bell-function 'ignore)

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
(setq js-indent-level 2)

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

(defun tsdate (ts)
  (interactive (list (read-from-minibuffer "Timestamp: ")))
  (message (format-time-string "%Y-%m-%d %H:%M:%S" (seconds-to-time (string-to-number ts))))
  )

(defun show-file-name ()
  (interactive)
  (kill-new (buffer-file-name))
  (message (buffer-file-name)))

(defun clear-shell ()
   (interactive)
   (let ((comint-buffer-maximum-size 0))
     (comint-truncate-buffer)))

(defun sort-lines-nocase ()
  (interactive)
  (let ((sort-fold-case t))
    (call-interactively 'sort-lines)))

;; =============================================================
;; OSX

;; Allow hash to be entered
(when (eq 'darwin system-type)

  (global-set-key (kbd "M-3") '(lambda () (interactive) (insert "#")))

  (use-package exec-path-from-shell
    :ensure t
    :pin melpa-stable
    :config
    (exec-path-from-shell-initialize))

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

(let ((local-path (expand-file-name "~/.local/bin")))
  (setenv "PATH" (concat local-path ":" (getenv "PATH")))
  (add-to-list 'exec-path local-path))

;;; init.el ends here
