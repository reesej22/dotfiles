;;; package --- summary -*- lexical-binding: t -*-
;;; Commentary:
;; init.el --- Emacs configuration file
;; Author: Joshua Reese
;; Version: 0.2
;; Keywords: convenience
;;; Code:
(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/")
        ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
;; Install use-package for managing packages
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)
;; Enable relative line numbers only in prog-mode
(add-hook 'prog-mode-hook (lambda () (display-line-numbers-mode t)
                                      (setq display-line-numbers 'relative)))
;; General settings
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(menu-bar-mode t)
 '(package-selected-packages
   '(company-quickhelp counsel flycheck ivy-hydra lsp-ivy lsp-mode
		       lsp-treemacs lsp-ui nov org-bullets paredit
		       pdf-tools projectile pyvenv-auto
		       rainbow-delimiters slime-company smex
		       treemacs-all-the-icons vertico-posframe))
 '(scroll-bar-mode -1)
 '(setq make-backup-files)
 '(tool-bar-mode nil)
 '(xterm-mouse-mode t))
;; Custom faces
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; Custom Packages and Package Configurations
; Better emacs menu search
(use-package vertico
  :hook (after-init . vertico-multiform-mode)
  :init (vertico-mode)
  (setq vertico-multiform-commands
        '((consult-line (:not posframe))
          (gopar/consult-line (:not posframe))
          (consult-ag (:not posframe))
          (t posframe))))
; Frame for vertico
(use-package vertico-posframe
  :custom
  (vertico-posframe-parameters
   '((left-fringe . 8)
     (right-fringe . 8))))
; Cycle through vertico options
(setq vertico-cycle t)
; Add which-key for keybindings
(use-package which-key
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.5))
;; Flycheck for syntax checking
(use-package flycheck
  :init (global-flycheck-mode))
;; projectile
(use-package projectile
  :init (projectile-mode +1)
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))
;; Python environment
(use-package pyvenv-auto
  :hook ((python-mode . pyvenv-auto-run)))
;; lsp Mode
(use-package lsp-mode
  :commands lsp
  :hook ((python-mode . lsp)
	 (rust-mode . lsp)
	 (c-mode . lsp)
	 (c++-mode . lsp)
	 (html-mode . lsp)
	 (css-mode . lsp)
	 (json . lsp)
	 (lsp . lsp-enable-which-key-integration))
  :config
  (setq lsp-auto-configure t
	lsp-auto-guess-root t
	lsp-enable-dap-auto-configure t
	lsp-enable-symbol-highlighting t
	lsp-enable-on-type-formatting t
	lsp-inlay-hint-enable t
	lsp-idle-delay 0.5))
(use-package lsp-treemacs
  :config
  (lsp-treemacs-sync-mode 1))
(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-use-childframe t
        lsp-ui-doc-position 'at-point
        lsp-ui-doc-include-signature t
        lsp-ui-sideline-enable nil
        lsp-ui-flycheck-enable t
        lsp-ui-flycheck-list-position 'right
        lsp-ui-flycheck-live-reporting t
        lsp-ui-peek-enable t
        lsp-ui-peek-peek-height 20
        lsp-ui-peek-list-width 50
        lsp-ui-sideline-ignore-duplicate t))
;; better bullets for org-mode
(use-package org-bullets
  :ensure t
  :hook (org-mode . org-bullets-mode))
;; pfd tools for orgmode
(use-package pdf-tools
  :config (pdf-tools-install))
;; epub reader
(use-package nov
  :mode ("\\.epub\\'" . nov-mode))
;; smex just because
(use-package smex)
;; better searching
(use-package ivy-hydra)
(use-package counsel)
;; Custom function for cousel
(defun gopar/consult-line ()
  "Improved `consult-line` with regex support."
  (interactive)
  (consult-line))
;; Treemacs
(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (setq treemacs-is-never-other-window t
        treemacs-collapse-dirs (if treemacs-python-executable 3 0)
        treemacs-sorting 'alphabetic-case-insensitive-asc
        treemacs-follow-after-init t
        treemacs-width 30
        treemacs-indentation 2
        treemacs-indentation-string " "
        treemacs-no-png-images nil
        treemacs-recenter-after-file-follow 'always
        treemacs-recenter-after-tag-follow 'always
        treemacs-file-event-delay 5000
        treemacs-file-follow-delay 0.2
        treemacs-follow-recenter-distance 0.1)
  (treemacs-filewatch-mode t)
  (treemacs-follow-mode t)
  (treemacs-fringe-indicator-mode t)
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))
;; icon support for treemacs
(use-package treemacs-all-the-icons
  :ensure t
  :after treemacs
  :config (treemacs-load-theme "all-the-icons"))
;; Load Common Lisp environment
(use-package slime
  :init
  (setq inferior-lisp-program "/usr/bin/sbcl")
  :config
  (slime-setup '(slime-fancy slime-company))
  (add-hook 'slime-mode-hook 'company-mode)
  (add-hook 'slime-repl-mode-hook 'company-mode)
  (setq slime-company-completion 'fuzzy
        slime-company-after-completion 'slime-company-just-one-space))
;; Load slime-company for better completion
(use-package slime-company
  :after (slime company)
  :config
  (setq slime-company-completion 'fuzzy
        slime-company-after-completion 'slime-company-just-one-space))
;; Enable company-mode for auto-completion
(use-package company
  :config
  (setq company-idle-delay 0.2)
  (setq company-minimum-prefix-length 1)
  (global-company-mode t))
;; Enable company-quickhelp for inline documentation
(use-package company-quickhelp
  :after company
  :config
  (company-quickhelp-mode))
;; Enable rainbow delimiters for better readability
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))
;; Paredit for structured editing of Lisp code
(use-package paredit
  :hook ((emacs-lisp-mode lisp-mode) . paredit-mode))
;; Enable electric-pair-mode for auto-closing brackets and quotes
(electric-pair-mode 1)
;; Add key bindings for SLIME
(global-set-key (kbd "C-c s") 'slime)
(global-set-key (kbd "C-c C-s") 'slime-selector)
;; Show matching parentheses
(show-paren-mode 1)
(provide 'init)
;;; init.el ends here
