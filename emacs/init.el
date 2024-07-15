;;; package --- summary -*- lexical-binding: t -*-
;;; Commentary:
;; init.el --- Emacs configuration file
;; Author: Joshua Reese
;; Version: 0.2
;; Keywords: convenience
;;; Code:
;; Increase garbage collection threshold during startup
(setq gc-cons-threshold most-positive-fixnum)
(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
	("org" . "https://orgmode.org/elpa/")
	("gnu" . "https://elpa.gnu.org/packages/")))
(require 'use-package)

;; Minimize garbage collection during startup
(setq gc-cons-threshold most-positive-fixnum)

;; Lower threshold back to 8 MiB (default is 800kB)
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (setq gc-cons-threshold (expt 2 23))))

(defun loadup-gen ()
  "Generate the lines to include in the lisp/loadup.el file
to place all of the libraries that are loaded by your InitFile
into the main dumped emacs"
  (interactive)
  (defun get-loads-from-*Messages* ()
    (save-excursion
      (let ((retval ()))
	(set-buffer "*Messages*")
	(beginning-of-buffer)
	(while (search-forward-regexp "^Loading " nil t)
	  (let ((start (point)))
	    (search-forward "...")
	    (backward-char 3)
	    (setq retval (cons (buffer-substring-no-properties start (point)) retval))))
	retval)))
  (map 'list
       (lambda (file) (princ (format "(load \"%s\")\n" file)))
       (get-loads-from-*Messages*)))

;; Enable relative line numbers only in prog-mode
(add-hook 'prog-mode-hook (lambda () (display-line-numbers-mode t)
			    (setq display-line-numbers 'relative)))
;; Suppress all warning popups
(setq warning-minimum-level :emergency)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(tsdh-light))
 '(evil-want-keybinding nil)
 '(global-dash-fortify-mode t)
 '(initial-scratch-message nil)
 '(make-backup-files nil)
 '(package-selected-packages
   '(pdf-tools neotree doom-modeline undo-fu evil-collection evil org dashboard dash-fortify
     paredit rainbow-delimiters slime-company slime org-plus-contrib vertico-posframe vertico
     nov org-bullets company-quickhelp which-key projectile pyvenv-auto auto-virtualenv company
     f s dash jsonrpc editorconfig))
 '(tool-bar-mode nil)
 '(which-key-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Hack Nerd Font" :foundry "nil" :slant normal :weight regular :height 110 :width normal)))))

;; UI Configuration
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-items '((recents . 5)
			  (projects . 5)
			  (agenda . 5)))
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-center-content t)
  (setq dashboard-set-navigator t)
  (setq dashboard-navigator-buttons
	`(((,(all-the-icons-octicon "mark-github"
	       :height 1.1 :v-adjust 0.0)
	    "Github"
	    "Browse Github"
	    (lambda (&rest _) (browse-url )))))))
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))
(use-package neotree
  :ensure t
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))
(use-package which-key
  :ensure t)
(use-package vertico
  :hook (after-init . vertico-multiform-mode)
  :init (vertico-mode)
  (setq vertico-multiform-commands
	'((consult-line (:not posframe))
	  (gopar/consult-line (:not posframe))
	  (consult-ag (:not posframe))
	  (t posframe))))
(use-package vertico-posframe
  :custom
  (vertico-posframe-parameters
   '((left-fringe . 8)
     (right-fringe . 8))))
(use-package all-the-icons
  :ensure t)

;; Evil Mode Configuration
(require 'evil)
(evil-set-leader 'normal (kbd "SPC"))
(evil-define-key 'normal 'global (kbd "<leader>t") 'toggle-theme)
(evil-define-key 'normal 'global (kbd "<leader>f") 'find-file)
(evil-define-key 'normal 'global (kbd "<leader>q") 'kill-buffer)
(evil-define-key 'normal 'global (kbd "<leader>w") 'save-buffer)
(evil-define-key 'normal 'global (kbd "<leader>o") 'neotree-toggle)
(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))
(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

;; Auto-Completion Configuration
(use-package company
  :ensure t
  :init
  (use-package company-quickhelp
    :ensure t
    :config (company-quickhelp-mode 1))
  :config
  (add-hook 'after-init-hook 'global-company-mode))

;; Language Server Protocol Configuration
(use-package eglot
  :ensure t
  :config
  (add-hook 'python-mode-hook 'eglot-ensure)
  (add-hook 'sh-mode-hook 'eglot-ensure)
  (add-hook 'java-mode-hook 'eglot-ensure))

;; Project Management Configuration
(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

;; Python Packages
(use-package auto-virtualenv
  :ensure t
  :init
  (use-package pyvenv
    :ensure t)
    :config
    (add-hook 'python-mode-hook 'auto-virtualenv-set-virtualenv)
    (add-hook 'projectile-after-switch-project-hook 'auto-virtualenv-set-virtualenv))

;; Github Copilot Configuration
;; https://github.com/rksm/copilot-emacsd/blob/master/init.el
(add-to-list 'load-path "~/.config/emacs/elpa/copilot.el")
(require 'copilot)
(add-hook 'prog-mode-hook 'copilot-mode)
(define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)
; copilot dependencies
(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))
(use-package jsonrpc
  :ensure t)
(use-package dash
  :ensure t)
(use-package s
  :ensure t)
(use-package f
  :ensure t)
;; End of Github Copilot Configuration

;; Org Mode Configuration
(use-package org
  :ensure org-plus-contrib
  :config
  (setq org-agenda-files '("~/Developer/Org/"))
  (setq org-log-done 'time)
  (setq org-todo-keywords
	'((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))
  (setq org-todo-keyword-faces
	'(("TODO" . "red")
	  ("IN-PROGRESS" . "orange")
	  ("WAITING" . "yellow")
	  ("DONE" . "green")))
  (setq org-agenda-custom-commands
	'(("d" "Daily Agenda"
	   ((agenda "" ((org-agenda-span 1)))
	    (todo "TODO")
	    (todo "IN-PROGRESS")
	    (todo "WAITING")))))
  (setq org-capture-templates
	'(("t" "Todo" entry (file+headline "~/Developer/Org/todo.org" "Tasks")
	   "* TODO %?\n  %i\n  %a")
	  ("j" "Journal" entry (file+datetree "~/Developer/Org/journal.org")
	   "* %?\nEntered on %U\n  %i\n  %a")))
  (setq org-refile-targets '(("~/org/todo.org" :maxlevel . 3)
			     ("~/org/journal.org" :maxlevel . 3)))
  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-agenda-start-on-weekday 0)
  (setq org-agenda-start-day "-1d")
  (setq org-agenda-span 7)
  (setq org-agenda-start-with-log-mode t)
  (setq org-agenda-log-mode-items '(closed clock state))
  (setq org-agenda-time-grid
	'((daily today require-timed)
	  "----------------"
	  (800 1000 1200 1400 1600 1800 2000)))
  (setq org-agenda-time-grid '((daily today require-timed)
			       "----------------"
			       (800 1000 1200 1400 1600 1800 2000)))
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-agenda-skip-deadline-if-done t))

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package nov
  :ensure t
  :mode ("\\.epub\\'" . nov-mode))
(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install))
;; End of Org Mode Configuration

;; Common Lisp Configuration
(use-package slime
  :ensure t
  :init
  (load (expand-file-name "~/.quicklisp/slime-helper.el"))
  (setq inferior-lisp-program "sbcl")
  :config
  (slime-setup '(slime-fancy slime-company))
  (add-hook 'lisp-mode-hook 'slime-mode)
  (add-hook 'slime-mode-hook 'company-mode)
  (add-hook 'slime-repl-mode-hook 'company-mode))
;; Load slime-company for better completion
(use-package slime-company
  :after (slime company)
  :config
  (setq slime-company-completion 'fuzzy
	slime-company-after-completion 'slime-company-just-one-space))
;; Enable rainbow delimiters for better readability
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))
;; Paredit for structured editing of Lisp code
(use-package paredit
  :hook ((emacs-lisp-mode lisp-mode) . paredit-mode))
;; End of Common Lisp Configuration

(provide 'init)
;;; init.el ends here
