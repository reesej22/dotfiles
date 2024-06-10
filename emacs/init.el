;; -*- lexical-binding: t -*-
;; init.el --- Emacs configuration file

;; Author: Joshua Reese
;; Version: 0.2
;; Keywords: convenience
;; URL: https://github.com/reesej22/dotfiles/edit/Research/emacs/init.el

;; Commentary:
;; This is the main configuration file for Emacs.

;; Code:

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

;; General settings
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(xterm-mouse-mode t)
(global-display-line-numbers-mode 1)
(setq make-backup-files nil) ; stop creating ~ files

;; Custom-set variables and faces
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(doom-acario-dark))
 '(custom-safe-themes
   '("dccf4a8f1aaf5f24d2ab63af1aa75fd9d535c83377f8e26380162e888be0c6a9" default))
 '(package-selected-packages
   '(treemacs-all-the-icons treemacs vertico-multiform dashboard vertico-posframe doom-themes doom-modeline platformio-mode arduino-mode arduino-cli-mode airline-themes powerline bash-completion smex nov nose ivy-hydra counsel yasnippet toml-mode rustic rust-playground pdf-tools lsp-ui flycheck exec-path-from-shell company)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

 ; Packages

;; Dashboard
(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-center-content t)
  (setq dashboard-vertically-center-content t)
  (setq dashboard-show-shortcuts nil))

;; Doom modeline and themes
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 30)
  (doom-modeline-window-width-limit nil)
  (doom-modeline-buffer-file-name-style 'truncate-with-project)
  (doom-modeline-minor-modes nil)
  (doom-modeline-enable-word-count t)
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-buffer-modification-icon t)
  (doom-modeline-env-python-executable "python")
  (display-time-mode t)
  (doom-modeline-time t)
  (doom-modeline-vcs-max-length 50))

(use-package doom-themes
  :config
  (doom-themes-org-config)
  (doom-themes-visual-bell-config))
  
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

(setq vertico-cycle t)

;; Flycheck for syntax checking
(use-package flycheck
  :init (global-flycheck-mode))

;; Yasnippet for code snippets
(use-package yasnippet
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode))

;; Company for auto-completion
(use-package company
  :bind (:map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)
              ("M-<" . company-select-first)
              ("M->" . company-select-last))
  (:map company-mode-map
        ("<tab>" . tab-indent-or-complete)
        ("TAB" . tab-indent-or-complete))
  :hook (prog-mode . company-mode)
  :custom
  (company-idle-delay 0.2)
  (company-minimum-prefix-length 2))

(defun company-yasnippet-or-completion ()
  (interactive)
  (or (do-yas-expand)
      (company-complete-common)))

(defun check-expansion ()
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
        (backward-char 1)
        (if (looking-at "::") t nil)))))

(defun do-yas-expand ()
  (let ((yas/fallback-behavior 'return-nil))
    (yas/expand)))

(defun tab-indent-or-complete ()
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (if (or (not yas/minor-mode)
            (null (do-yas-expand)))
        (if (check-expansion)
            (company-complete-common)
          (indent-for-tab-command)))))

;; Rust development setup
(use-package rustic
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status)
              ("C-c C-c e" . lsp-rust-analyzer-expand-macro)
              ("C-c C-c d" . dap-hydra)
              ("C-c C-c h" . lsp-ui-doc-glance))
  :config
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

(defun rk/rustic-mode-hook ()
  (when buffer-file-name
    (setq-local buffer-save-without-query t))
  (add-hook 'before-save-hook 'lsp-format-buffer nil t))

(use-package lsp-mode
  :commands lsp
  :custom
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  (lsp-rust-analyzer-server-display-inlay-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-rust-analyzer-display-parameter-hints nil)
  (lsp-rust-analyzer-display-reborrow-hints nil)
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(use-package lsp-ui
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil))

(use-package rust-playground)
(use-package toml-mode)

;; Debugging support with dap-mode
(use-package exec-path-from-shell
  :init (exec-path-from-shell-initialize))

(when (executable-find "lldb-mi")
  (use-package dap-mode
    :config
    (dap-ui-mode)
    (dap-ui-controls-mode 1)
    (require 'dap-lldb)
    (require 'dap-gdb-lldb)
    (dap-gdb-lldb-setup)
    (dap-register-debug-template
     "Rust::LLDB Run Configuration"
     (list :type "lldb"
           :request "launch"
           :name "LLDB::Run"
           :gdbpath "rust-lldb"))))

;; Python development setup
(use-package lsp-mode
  :hook (python-mode . lsp-deferred)
  :commands (lsp lsp-deferred)
  :custom
  (lsp-pylsp-server-command "pylsp"))

(use-package lsp-ui
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable t)
  (lsp-ui-imenu-enable t))

(use-package pyvenv
  :config
  (setenv "WORKON_HOME" "~/.virtualenvs")
  (pyvenv-tracking-mode 1))

(use-package company
  :hook (python-mode . company-mode)
  :custom
  (company-idle-delay 0.0)
  (company-minimum-prefix-length 1)
  :bind (:map company-active-map
              ("M-n" . nil)
              ("M-p" . nil)
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)
              :map company-search-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)))

(use-package flycheck
  :hook (python-mode . flycheck-mode))

(use-package yasnippet
  :hook (python-mode . yas-minor-mode))

(use-package blacken
  :hook (python-mode . blacken-mode))

(use-package ein)

;; Bash development setup
(use-package sh-script
  :ensure nil
  :hook (sh-mode . lsp-deferred))

(use-package lsp-mode
  :hook (sh-mode . lsp-deferred)
  :custom
  (lsp-bash-highlight-parsing-errors t))

(use-package company
  :hook (sh-mode . company-mode)
  :custom
  (company-idle-delay 0.2)
  (company-minimum-prefix-length 2))

(use-package bash-completion)

;; Arduino development setup
(use-package arduino-mode)

(use-package arduino-cli-mode
  :after arduino-mode
  :config
  (setq arduino-cli-mode-arduino-executable "arduino-cli")
  (setq arduino-cli-mode-sketch-directory "/path/to/sketches"))

(defun my-compile-arduino-sketch ()
  "Compile the current Arduino sketch."
  (interactive)
  (let ((command (format "arduino-cli compile --fqbn <board_fqbn> %s"
                         (file-name-directory (buffer-file-name)))))
    (compile command)))

(defun my-upload-arduino-sketch ()
  "Upload the current Arduino sketch."
  (interactive)
  (let ((command (format "arduino-cli upload -p <port> --fqbn <board_fqbn> %s"
                         (file-name-directory (buffer-file-name)))))
    (compile command)))

;; Miscellaneous packages
(use-package pdf-tools
  :config (pdf-tools-install))

(use-package nov
  :mode ("\\.epub\\'" . nov-mode))

(use-package smex)

(use-package ivy-hydra)

(use-package counsel)

;; Custom functions
(defun gopar/consult-line ()
  "Improved `consult-line` with regex support."
  (interactive)
  (consult-line))

;; Treemacs and Treemacs All-The-Icons
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

(use-package treemacs-all-the-icons
  :ensure t
  :after treemacs
  :config (treemacs-load-theme "all-the-icons"))


(provide 'init)
;;; init.el ends here
