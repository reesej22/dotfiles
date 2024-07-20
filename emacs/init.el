;;; package --- summary -*- lexical-binding: t -*-
;;; Commentary:
;; init.el --- Emacs configuration file
;; Author: Joshua Reese
;; Version: 0.2
;; Keywords: convenience
;;; Code:(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
	("org" . "https://orgmode.org/elpa/")
	("gnu" . "https://elpa.gnu.org/packages/")))

(setq gc-cons-threshold most-positive-fixnum)

(add-hook 'emacs-startup-hook
	  (lambda ()
	    (setq gc-cons-threshold (expt 2 23))))

(add-hook 'prog-mode-hook (lambda () (display-line-numbers-mode t)
			    (electric-pair-mode t)
			    (setq display-line-numbers 'relative)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-enabled-themes '(gruvbox-dark-soft))
 '(custom-safe-themes
	'("ba323a013c25b355eb9a0550541573d535831c557674c8d59b9ac6aa720c21d3" 
		 "871b064b53235facde040f6bdfa28d03d9f4b966d8ce28fb1725313731a2bcc8" 
		 default))
 '(evil-want-keybinding nil)
 '(initial-scratch-message "\12")
 '(make-backup-files nil)
 '(package-selected-packages
  '(treemacs-projectile treemacs-evil treemacs doom-modeline gruvbox-theme jsonrpc 
	   editorconfig rainbow-delimiters pdf-tools nov which-key projectile 
	   auto-virtualenvwrapper auto-virtualenv evil-collection company-quickhelp 
	   company dashboard org-superstar org evil))
 '(tool-bar-mode nil)
 '(visible-bell t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Hack Nerd Font Mono" :foundry "SRC" :slant normal 
		       :weight regular :height 98 :width normal)))))
;; Vim Keybinding
(use-package evil
  :init
  (evil-mode 1)
  (use-package evil-collection
    :after evil
    :config
    (evil-collection-init)
	  (evil-set-leader 'normal (kbd "SPC"))
	  (evil-define-key 'normal 'global (kbd "<leader>t") 'treemacs)
	  (evil-define-key 'normal 'global (kbd "<leader>f") 'find-file)
	  (evil-define-key 'normal 'global (kbd "<leader>q") 'kill-buffer)
	  (evil-define-key 'normal 'global (kbd "<leader>w") 'save-buffer)
    )
)
;; Dashboard Config
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-center-content t)
)
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
)
;; Treemacs
(with-eval-after-load 'treemacs
  (define-key treemacs-mode-map [mouse-1] #'treemacs-single-click-expand-action))
(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)
(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)
;; Auto-Completion
(use-package company
  :ensure t
  :init
  (use-package company-quickhelp
    :ensure t
    :config (company-quickhelp-mode 1))
  :config
  (add-hook 'after-init-hook 'global-company-mode)
)
;; Language Server Config
(use-package eglot
  :ensure t
  :hook
  (python-mode . eglot-ensure)
  (sh-mode . eglot-ensure)
)
;; Project Management
(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)
  (setq projectile-project-search-path
	'("~/Developer/" "~/Developer/BashScripts" ("~/Developer/PyProjects" . 1)))
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
)
;; PythonVirtual Environment 
(use-package auto-virtualenv
  :ensure t
  :init
  (use-package pyvenv
    :ensure t)
    :config
    (add-hook 'python-mode-hook 'auto-virualenv-set-virualenv)
    (add-hook 'projectile-after-swith-project-hook
	      'auto-virtualenv-set-virtualenv)
)
;; Easy Key Navigation
(use-package which-key
  :ensure t
  :init (which-key-mode t)
  :config
  (setq which-key-sort-order 'which-key-key-order)
  (setq which-key-show-early-on-C-h t)
)
;; EPUB Reader
(use-package nov
  :ensure t
  :mode ("\\.epub\\'" . nov-mode)
)
;; PDF Tools
(use-package pdf-tools
  :ensure t)
;; Org-Mode Config
(use-package org
  :ensure t
  :config
  (setq org-agenda-files '("~/Developer/Org/")
	org-todo-keywords
	'((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE"))
	org-todo-keyword-faces
	'(("TODO" . "red")
	  ("IN-PROGRESS" . "orange")
	  ("WAITING" . "yellow")
	  ("DONE" . "green")))
)
(use-package org-superstar
  :ensure t
  :config
  (add-hook 'org-mode-hook
	    (lambda () (org-superstar-mode 1)
		       (ruler-mode t)))
)
;; Rainbow Brackets
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode)
)
;; Github Copilot Configuration
(add-to-list 'load-path "~/.config/emacs/elpa/copilot.el")
(require 'copilot)
(add-hook 'prog-mode-hook 'copilot-mode)
(define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)
; copilot dependencies
(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1)
)
(use-package jsonrpc
  :ensure t)
(use-package dash
  :ensure t)
(use-package s
  :ensure t)
(use-package f
  :ensure t)
;; Theme
(use-package gruvbox-theme
  :ensure t)
(provide 'init)
;;; init.el ends here
