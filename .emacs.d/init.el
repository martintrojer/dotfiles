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
  :defer nil)

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

(use-package org-drill
  :ensure t)

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

;; Rust
(use-package rust-mode
  :ensure t
  :defer t
  :config
  (setq rust-format-on-save t)
  (define-key rust-mode-map (kbd "C-c C-c") 'rust-compile))

(use-package flycheck-rust
  :ensure t
  :defer t
  :config
  (with-eval-after-load 'rust-mode
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)))

(use-package lsp-mode
  :ensure t
  :defer t
  :init
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-lens-enable nil)
  :hook ((rust-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration)))

(use-package which-key
  :ensure t
  :defer t
  :hook ((rust-mode . which-key-mode)))

;; Ocaml
;; opam install tuareg merlin ocp-indent utop ocamlformat
(use-package caml
  :ensure t
  :defer t)
(use-package tuareg
  :ensure t
  :defer t
  :config
  (load "~/.opam/4.14.0+flambda/share/emacs/site-lisp/tuareg-site-file.elc")
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
  :bind (("C-c C-\\" . merlin-pop-stack)
         ("C-c C-c"  . infe-mk)
         ("C-c C-o"  . merlin-document))
  :config
  (add-hook 'tuareg-mode-hook 'merlin-mode)
  ;;  (add-hook 'reason-mode-hook 'merlin-mode)
  (add-hook 'merlin-mode-hook #'company-mode)
  (setq merlin-error-after-save nil)
  ;;(require 'merlin-comany)
  )
(add-hook 'tuareg-mode-hook 'merlin-mode)


(use-package ocp-indent
  :ensure t
  :defer t)

;; utop configuration
(use-package utop
  :ensure t
  :defer t
  :config
  (autoload 'utop-minor-mode "utop" "Minor mode for utop" t)
  (add-hook 'tuareg-mode-hook 'utop-minor-mode)
  )

;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
;; (require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line


;; Haskell
;; make sure there is no local ghc in the path!
;; stack setup
;;  stack install hlint hindent hasktags stylish-haskell ghcid

(setq haskell-stylish-on-save t)

(use-package haskell-mode
  :ensure t
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
               ;; (intero-mode)
               (hindent-mode)
               (haskell-indentation-mode t)
;;               (flycheck-add-next-checker 'intero '(warning . haskell-hlint))
               ;; (interactive-haskell-mode t)
               ;; (structured-haskell-mode t)
               ))
  (define-key haskell-mode-map "\C-c\C-h" 'hoogle))

(use-package hindent
  :ensure t
  :defer t)

;; Markdown
(use-package markdown-mode
  :ensure t
  :mode "\\.md\\'")

;; Yaml
(use-package yaml-mode
  :ensure t
  :defer t)

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
  :bind (("C-c C-g" . magit-status)
         ("C-c C-b" . magit-blame-mode))
  :config
  (setq magit-last-seen-setup-instructions "1.4.0")
  (setq magit-revert-buffers 'silent)
  (setq magit-diff-refine-hunk t))

;; Monky (hg)
(use-package monky
  :ensure t
  :pin melpa)

;; Company
(use-package company
  :ensure t
  :diminish company-mode
  :config
  (global-company-mode))

;; IDO
(use-package ido-completing-read+
  :ensure t
  :config
  (ido-mode t)
  (setq ido-enable-flex-matching t)
  (global-set-key "\M-x"
                (lambda ()
                  (interactive)
                  (call-interactively
                   (intern (ido-completing-read "M-x " (all-completions "" obarray 'commandp)))))))

;; Yasnippet
(use-package yasnippet
  :ensure t
  :diminish (yas-minor-mode . " Y")
  :config
  (yas-global-mode 1)
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets")
  (yas-load-directory "~/.emacs.d/snippets"))

;; Ripgrep
(use-package deadgrep
  :ensure t
  :bind ("C-x M-f" . deadgrep))

;; vdiff
(use-package vdiff
  :ensure t)

(require 'dash)

;; Linum
(use-package linum
  :ensure t
  :config
  (if window-system
      (setq linum-format "%d")
    (setq linum-format "%d "))
  (setq linum-modes '(c-mode-common
                      clojure-mode
                      css-mode
                      elm-mode
                      emacs-lisp-mode
                      go-mode
                      haskell-mode
                      html-mode
                      js-mode
                      makefile-gmake-mode
                      makefile-mode
                      markdown-mode
                      puppet-mode
                      purescripe-mode
                      purescript-mode
                      python-mode
                      ruby-mode
                      rust-mode
                      shell-script-mode
                      thrift-mode
                      tuareg-mode
                      ))
  (require 's)
  (--each linum-modes (add-hook (intern (s-concat (symbol-name it) "-hook")) 'linum-mode)))

;; Flycheck
(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

;; Flyspell
(use-package flyspell
  :ensure t
  :bind (("C-c £" . flyspell-buffer))
  :config
  (add-hook 'org-mode-hook 'flyspell-buffer)
  (add-hook 'org-mode-hook 'flyspell-mode)
  (add-hook 'markdown-mode-hook 'flyspell-buffer)
  (add-hook 'markdown-mode-hook 'flyspell-mode))

;; Golden ratio
(use-package golden-ratio
  :ensure t
  :diminish (golden-ratio-mode . "AU")
  :config
  (golden-ratio-mode 1)
  ;; (setq golden-ratio-auto-scale t)
  (setq split-width-threshold 200)
  (setq split-height-threshold 120)
  (add-to-list 'golden-ratio-exclude-modes "ediff-mode")
  (add-to-list 'golden-ratio-exclude-modes "vdiff-mode"))

;; browse-kill-ring
(use-package browse-kill-ring
  :ensure t
  :config
  (browse-kill-ring-default-keybindings))

;; Multiple cursors
(use-package multiple-cursors
  :ensure t
  :bind (("C-c ." . mc/mark-next-like-this)
         ("C-c ," . mc/mark-previous-like-this)
         ("C-c M-." . mc/mark-all-like-this)))

;; Expand region
(use-package expand-region
  :ensure t
  :bind ("C-\\" . er/expand-region))

;; Git gutter
(use-package git-gutter
  :ensure t
  :diminish (git-gutter-mode . "GG")
  :bind ("C-x C-g" . git-gutter:toggle))

;; inf-ruby
(use-package inf-ruby
  :ensure t
  :config
  (add-hook 'ruby-mode-hook 'inf-ruby-minor-mode))

;; Avy
(use-package avy
  :ensure t
  :bind (("M-g f" . avy-goto-line)
         ("M-g w" . any-goto-word-1)))

;; buffer-move
(use-package buffer-move
  :ensure t
  :bind (("C-c <C-right>" . mt-move-right)
         ("C-c <C-left>" . mt-move-left))
  :config
  (defun mt-move-right ()
    (interactive)
    (buf-move-right) (golden-ratio))
  (defun mt-move-left ()
    (interactive)
    (buf-move-left) (golden-ratio)))

;; scratch buffer
(use-package persistent-scratch
  :ensure t
  :config
  (persistent-scratch-setup-default))

;; =============================================================
;; Colors

(use-package zenburn-theme
  :ensure t
  :config
  (enable-theme 'zenburn))

;; =============================================================
;; Misc config

;; save the session state
(desktop-save-mode 1)

(let ((local-path (expand-file-name "~/.local/bin")))
  (setenv "PATH" (concat local-path ":" (getenv "PATH")))
  (add-to-list 'exec-path local-path))

(setq default-buffer-file-coding-system 'utf-8-unix)

(recentf-mode)
(setq recentf-max-menu-items 25)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

(show-paren-mode)
(global-auto-revert-mode :global)
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

;; =============================================================
;; FB

(add-to-list 'auto-mode-alist '("\\.bzl\\'" . python-mode))
(add-to-list 'auto-mode-alist '("/BUCK\\'" . python-mode))
(add-to-list 'auto-mode-alist '("/BUCK\\.v2\\'" . python-mode))
(add-to-list 'auto-mode-alist '("/TARGETS\\'" . python-mode))
(add-to-list 'auto-mode-alist '("/TARGETS\\.v2\\'" . python-mode))

(require 'magit)
(add-to-list 'magit-process-password-prompt-regexps "^Passcode or option .*: ?$")

(setq org-file-apps
      '((auto-mode . emacs)
        ("\\.x?html?\\'" . default)
        ("\\.pdf\\'" . default)))

(defun infer-mk ()
  (interactive)
  (compile "cd ~/infer && make -C infer/src -k byte"))

(defun infer-analyze ()
  (interactive)
  (shell-command "cd ~/fbsource/fbobjc && infer analyze --no-progress-bar -g -a infer &"))

(defvar infer-capture-history nil "History for infer-capture")
(defun infer-capture (task)
  (interactive (list (read-from-minibuffer "Task: " (car infer-capture-history) nil nil 'infer-capture-history)))
  (shell-command (concat "cd ~/fbsource/fbobjc && ./" task ".sh &")))

(defun fbobjc (fnl)
  (interactive (list (read-from-minibuffer "Filename (and line): ")))
  (let* ((parts (split-string fnl ":"))
         (filename (car parts))
         (no-fbobjc (string-trim (car (last (split-string filename "fbobjc/")))))
         (line (car (cdr parts))))
    (find-file (concat "~/fbsource/fbobjc/" no-fbobjc))
    (if (not (string= line ""))
        (goto-line (string-to-number line)))))

(delete 'Hg vc-handled-backends )

(setq org-agenda-files (list
                        "~/Dropbox (Meta)/docs/ClangPlugin.org"
                        "~/Dropbox (Meta)/docs/FBSource.org"
                        "~/Dropbox (Meta)/docs/Feedback.org"
                        "~/Dropbox (Meta)/docs/Impact.org"
                        "~/Dropbox (Meta)/docs/Infer.org"
                        "~/Dropbox (Meta)/docs/Infra.org"
                        "~/Dropbox (Meta)/docs/Interview.org"
                        "~/Dropbox (Meta)/docs/JackInfer.org"
                        "~/Dropbox (Meta)/docs/Meetings.org"
                        "~/Dropbox (Meta)/docs/Oncall.org"
                        "~/Dropbox (Meta)/docs/Stuff.org"
                        "~/Dropbox (Meta)/docs/Work.org"
                        "~/Dropbox (Meta)/docs/WWW.org"
                        ))

(setq org-default-notes-file "~/Dropbox (Meta)/docs/Work.org")

;;; init.el ends here
