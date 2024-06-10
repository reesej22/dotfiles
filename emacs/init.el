;; -*- lexical-binding: t -*-
;; init.el --- Emacs configuration file

;; Author: Your Name
;; Maintainer: Your Name
;; Version: 0.1
;; Keywords: convenience
;; URL: http://example.com/your-emacs-config

;; Commentary:
;; This is the main configuration file for Emacs.

;; Code:
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
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

;; ========== Begin Package Configurations ==========

;; rustic = basic rust-mode + additions
(use-package rustic
  :ensure t
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

;; for rust-analyzer integration
(use-package lsp-mode
  :ensure t
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
  :ensure t
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil))

;; inline errors
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;; auto-completion and code snippets
(use-package yasnippet
  :ensure t
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode))

(use-package company
  :ensure t
  :bind (:map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)
              ("M-<" . company-select-first)
              ("M->" . company-select-last))
  (:map company-mode-map
        ("<tab>" . tab-indent-or-complete)
        ("TAB" . tab-indent-or-complete)))

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

;; Create / cleanup rust scratch projects quickly
(use-package rust-playground
  :ensure t)

;; for Cargo.toml and other config files
(use-package toml-mode
  :ensure t)

;; setting up debugging support with dap-mode
(use-package exec-path-from-shell
  :ensure t
  :init (exec-path-from-shell-initialize))

(when (executable-find "lldb-mi")
  (use-package dap-mode
    :ensure t
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

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; Python Development Setup

;; Install and configure lsp-mode for Python
(use-package lsp-mode
  :ensure t
  :hook (python-mode . lsp-deferred)
  :commands (lsp lsp-deferred)
  :custom
  (lsp-pylsp-server-command "pylsp"))

;; Optional: Configure lsp-ui for better UI
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable t)
  (lsp-ui-imenu-enable t))

;; Use pyvenv to manage Python virtual environments
(use-package pyvenv
  :ensure t
  :config
  (setenv "WORKON_HOME" "~/.virtualenvs")
  (pyvenv-tracking-mode 1))

;; Install and configure flycheck for Python
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :hook (python-mode . flycheck-mode))

;; Install and configure company for Python
(use-package company
  :ensure t
  :hook (python-mode . company-mode)
  :custom
  (company-idle-delay 0.0)
  (company-minimum-prefix-length 1)
  :bind (:map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)
              ("M-<" . company-select-first)
              ("M->" . company-select-last)))

;; Install and configure yasnippet for Python
(use-package yasnippet
  :ensure t
  :config
  (yas-reload-all)
  (add-hook 'python-mode-hook #'yas-minor-mode))

;; Optional: Use black for automatic code formatting
(use-package blacken
  :ensure t
  :hook (python-mode . blacken-mode)
  :custom
  (blacken-line-length 88))

;; Optional: Use isort for import sorting
(use-package py-isort
  :ensure t
  :hook (before-save . py-isort-before-save))

;; Keybindings specific to python-mode
(use-package python
  :ensure nil
  :bind (:map python-mode-map
              ("C-c C-f" . lsp-format-buffer)
              ("C-c C-r" . lsp-rename)))

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; Python-specific flycheck configuration
(use-package flycheck-pycheckers
  :ensure t
  :after flycheck
  :init (setq flycheck-pycheckers-checkers '(mypy3 pyflakes))
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-pycheckers-setup))

;; Optional: Integration with Jupyter notebooks via ein
(use-package ein
  :ensure t
  :config
  (setq ein:use-auto-complete-superpack t))

;;; End of Python Development Setup

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; Bash support
(use-package sh-script
  :mode ("\\.sh\\'" . sh-mode)
  :config
  (add-hook 'sh-mode-hook 'lsp-deferred))

(use-package bash-completion
  :ensure t
  :config
  (bash-completion-setup))

(use-package lsp-mode
  :commands lsp
  :hook (sh-mode . lsp-deferred)
  :custom
  (lsp-clients-bash-language-server-command '("bash-language-server" "start")))

;; Hook `company-mode` and `bash-completion` with `sh-mode`
(defun setup-sh-mode ()
  (company-mode)
  (bash-completion-setup)
  (lsp-deferred))

(add-hook 'sh-mode-hook 'setup-sh-mode)

;;; End of Bash support

;; Custom-set variables and faces
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes nil)
 '(package-selected-packages
   '(airline-themes powerline bash-completion smex nov nose ivy-hydra counsel yasnippet toml-mode rustic rust-playground pdf-tools lsp-ui flycheck exec-path-from-shell company)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Additional general configuration
(require 'powerline)
(powerline-default-theme)
(require 'airline-themes)
(load-theme 'airline-jellybeans t)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(xterm-mouse-mode t)
(global-display-line-numbers-mode 1)
(setq make-backup-files nil) ; stop creating ~ files

(provide 'init)
;;; init.el ends here

