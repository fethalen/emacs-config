;;; init.el -*- lexical-binding: t; -*-

;;; Packages

(package-initialize)

(setq package-selected-packages
      '(affe
	all-the-icons
	auctex
	blacken
	conda
	dap-mode
	eat
	eglot
	org
	python-ts-mode
	pyvenv
	slime
	treemacs
	treemacs-all-the-icons
	treemacs-magit
	treesit-auto
        avy
        cape
        cider
        consult
        corfu
        doom-themes
        embark
        embark-consult
        exec-path-from-shell
        expand-region
        geiser
        julia-snail
        julia-ts-mode
        magit
        marginalia
        markdown-mode
        orderless
        paredit
        poly-org
        polymode
        rainbow-delimiters
        savehist
        vertico
        which-key))

(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

(unless package-archive-contents
  (package-refresh-contents))

;;; Encoding

(set-default-coding-systems 'utf-8)     ; Default to UTF-8 encoding
(prefer-coding-system       'utf-8)     ; Add UTF-8 at the front for automatic detection
(set-terminal-coding-system 'utf-8)     ; Set coding system of terminal output
(set-keyboard-coding-system 'utf-8)     ; Set coding system for keyboard input on TERMINAL
(set-language-environment "English")    ; Set up multilingual environment

;;; General

;; Core Emacs settings
(use-package emacs
  :init
  (show-paren-mode 1)                   ; Highlight matching parens
  (electric-pair-mode 1)                ; Auto-close pairs
  (defalias 'yes-or-no-p 'y-or-n-p)
  :hook
  ((text-mode . visual-line-mode)
   (before-save . delete-trailing-whitespace))
  :config
  (setq
   ;; Cursor & feedback
   cursor-type 'box
   visible-bell t
   column-number-mode t
   echo-keystrokes 0

   ;; Startup
   inhibit-startup-screen t
   initial-scratch-message ""
   initial-buffer-choice t

   ;; Increase process output buffer
   read-process-output-max (* 1024 1024)

   ;; Indentation
   indent-tabs-mode nil
   fill-column 80)

  ;; Ensure backup and auto-save directories exist
  (dolist (dir '("backups" "auto-save"))
    (make-directory (expand-file-name dir user-emacs-directory) t))

  ;; Redirect backups and auto-saves
  (setq backup-directory-alist
        `((".*" . ,(expand-file-name "backups/" user-emacs-directory)))
        auto-save-file-name-transforms
        `((".*" ,(expand-file-name "auto-save/" user-emacs-directory) t)))

  ;; Enable smooth per-pixel scrolling (introduced in Emacs v29.1).
  ;; Makes trackpad/mouse-wheel scrolling fluid and precise.
  (when (fboundp 'pixel-scroll-precision-mode)
    (pixel-scroll-precision-mode 1))

  ;; macOS-specific runtime settings
  (when (eq system-type 'darwin)
    ;; Modifier keys
    (setq mac-control-modifier 'ctrl
          mac-command-modifier 'super
          mac-option-modifier  'meta
          frame-title-format nil)

    ;; Switch theme with system appearance
    (add-hook 'ns-system-appearance-change-functions #'my/apply-theme)

    ;; Load PATH from shell
    (use-package exec-path-from-shell
      :config
      (exec-path-from-shell-initialize))))

;; Enable mouse usage when running in a terminal.
(unless window-system
  (xterm-mouse-mode))

;;; Appearance

(add-to-list 'custom-theme-load-path
             (expand-file-name "themes/rose-pine-doom-emacs" user-emacs-directory))

(use-package doom-themes
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  :config
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (nerd-icons must be installed!)
  ;; (doom-themes-neotree-config)
  ;; or for treemacs users
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(defadvice load-theme (before clear-previous-themes activate)
  "Clear existing theme instead of layering them."
  (mapc #'disable-theme custom-enabled-themes))

(defun my/apply-theme (appearance)
  "Load theme, taking current system appearance into consideration."
  (mapc #'disable-theme custom-enabled-themes)
  (pcase appearance
    ('light (load-theme 'doom-rose-pine-dawn t))
    ('dark  (load-theme 'doom-rose-pine t))))

(add-hook 'ns-system-appearance-change-functions #'my/apply-theme)

;;; Editing

;; Quickly grow/shrink selection
(use-package expand-region
  :bind (("C-=" . er/expand-region)
         ("C--" . er/contract-region)))

;; Plain-text system for notes, tasks, and literate programming
(use-package org
  :ensure t
  :config
  (setq org-directory "~/org"
        org-agenda-files '("~/org/tasks.org" "~/org/projects.org")
        org-default-notes-file "~/org/inbox.org"
        org-log-done 'time
        org-startup-indented t
        org-hide-leading-stars t
        org-ellipsis " ▾"))

(defun my/prog-mode-hooks ()
  "Settings applied to programming modes."
  ;; Highlight current line
  ;; (hl-line-mode)
  ;; Show, in the echo area, the argument list of the function call you are
  ;; currently writing.
  (eldoc-mode 1)
  ;; Show line numbers
  ;; (display-line-numbers-mode)
  ;; Display relative line numbers
  ;; (setq display-line-numbers 'relative)
  ;; Display an indiciation of the ‘fill-column’ position
  ;; (display-fill-column-indicator-mode)
  ;; Show indicator for empty lines at the end of the buffer
  ;; (setq indicate-empty-lines t)
  )

(add-hook 'prog-mode-hook 'my/prog-mode-hooks)

(defun my/text-mode-hooks ()
  "Settings applied to text-centric modes."
  ;; Highlight current line
  ;; (hl-line-mode)
  ;; Show indicator for empty lines at the end of the buffer
  ;; (setq indicate-empty-lines t)
  )

(add-hook 'text-mode-hook 'my/text-mode-hooks)

(defun my/minibuffer-setup-hooks ()
  "Settings applied whenever a minibuffer is entered."
  ;; Disable paredit in all minibuffers to avoid RET issues
  (when (bound-and-true-p paredit-mode)
    (paredit-mode -1)))

(add-hook 'minibuffer-setup-hook #'my/minibuffer-setup-hooks)

;; Display available keybindings in a popup.
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;; Fast, on-screen navigation by character, word, or line
(use-package avy
  :ensure t
  :bind
  (("C-:"   . avy-goto-char)              ;; jump to a single char
   ("C-'"   . avy-goto-char-2)            ;; jump to a 2-char sequence
   ("M-g j" . avy-goto-char-timer)        ;; jump to typed string (timed)
   ("M-g w" . avy-goto-word-1)            ;; jump to word start
   ("M-g s" . avy-goto-subword-1)         ;; jump inside camelCase/snake_case
   ("M-g l" . avy-goto-line)              ;; jump to beginning of line
   ("M-g e" . avy-goto-end-of-line)       ;; jump to end of line
   ("M-g SPC" . avy-goto-whitespace-end)  ;; jump to end of whitespace
   ("M-g r" . avy-resume))                ;; resume last avy command
  :config
  (setq avy-background t           ;; dim background during selection
        avy-style 'at-full         ;; show all keys before target
        avy-all-windows t          ;; search across all visible windows
        avy-timeout-seconds 0.5    ;; idle delay before hints
        avy-case-fold-search nil)) ;; case-sensitive search

;; Vertical minibuffer completions
(use-package vertico
  :ensure t
  :init (vertico-mode))

;; Orderless completions
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides
   '((file (styles orderless partial-completion)))))

;; Annotations in minibuffer
(use-package marginalia
  :ensure t
  :init (marginalia-mode))

;; Commands & enhanced navigation
(use-package consult
  :ensure t
  :after orderless
  :init
  :bind (("M-x"         . execute-extended-command) ;; old M-x feel
         ("M-X"         . consult-mode-command)     ;; major-mode commands
         ("C-c C-c M-x" . execute-extended-command) ;; smex fallback
         ("C-x b"       . consult-buffer)
         ("C-s"         . consult-line)
         ("C-x p b"     . consult-project-buffer)
         ("C-x p f"     . project-find-file)))

;; Context actions
(use-package embark
  :ensure t
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim))
  :init (setq prefix-help-command #'embark-prefix-help-command))

;; Embark + Consult integration
(use-package embark-consult
  :after (embark consult)
  :ensure t
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;; Persist minibuffer history across sessions
(use-package savehist
  :init
  (setq history-length 1000
        history-delete-duplicates t
        savehist-additional-variables '(extended-command-history))
  (savehist-mode 1))

;; Async fuzzy finder
(use-package affe
  :ensure t
  :after consult
  :bind (("C-x p g" . affe-grep)
         ("C-x p G" . affe-grep-no-ignore)
         ("C-x p r" . affe-find))
  :config
  ;; Integrate with orderless
  (setq affe-regexp-function #'orderless-regexp
        affe-highlight-function #'orderless-highlight-matches))

;; In-buffer completion
(use-package corfu
  :ensure t
  :init
  (global-corfu-mode) ;; enable Corfu globally
  :bind (:map corfu-map
              ("M-p" . corfu-popupinfo-scroll-down)
              ("M-n" . corfu-popupinfo-scroll-up)
              ("M-d" . corfu-popupinfo-toggle))
  :config
  ;; (setq corfu-auto t)         ;; auto-complete without M-TAB
  ;; (setq corfu-auto-delay 0.2) ;; 0.2 seconds delay
  ;; (setq corfu-auto-prefix 1)  ;; start completion after 2 characters
  ;; (setq corfu-quit-no-match 'separator)
  ;; (setq corfu-preview-current nil) ;; don't preselect
  ;; (setq corfu-cycle t)             ;; cycle through candidates

  ;; Popup info (like docs/signatures) inline with Corfu
  (corfu-popupinfo-mode 1)
  (setq corfu-popupinfo-delay 0.1
        corfu-popupinfo-max-width 80
        corfu-popupinfo-max-height 20))

;; Extra completion sources
(use-package cape
  :ensure t
  :init
  ;; Add useful default completion sources
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-keyword))

;; Markdown & GitHub Flavored Markdown
(use-package markdown-mode
  :ensure t
  :mode (("\\.md\\'"       . gfm-mode)
         ("\\.markdown\\'" . markdown-mode)
         ("\\.text\\'"     . gfm-mode))
  :init
  (autoload 'gfm-mode "markdown-mode" "Major mode for editing GitHub Flavored Markdown" t))

;; Enable Polymode.
(use-package polymode
  :ensure t
  :mode (("\\.Rmd\\'"   . poly-markdown+r-mode)
         ("\\.Rnw\\'"   . poly-noweb+r-mode)
         ("\\.Snw\\'"   . poly-noweb+r-mode))
  :config
  ;; Enable smart indentation and highlighting
  (setq poly-noweb-auto-indent t
        poly-markdown-auto-indent t))

;; Terminal emulation
(use-package eat
  :ensure t
  :defer t
  :hook (eat-mode . (lambda ()
                      (setq-local scroll-conservatively 1000
                                  scroll-step 1
                                  scroll-margin 0
                                  auto-window-vscroll nil)))
  :custom
  (eat-term-name "xterm-256color")
  (eat-enable-mouse t)
  (eat-scroll-to-bottom-on-output 'this))

;; Treemacs core
(use-package treemacs
  :ensure t
  :defer t
  :init
  ;; Toggle with C-x t t
  (with-eval-after-load 'project
    (define-key project-prefix-map (kbd "t") #'treemacs))
  :config
  (setq treemacs-width 50
        treemacs-follow-after-init t
        treemacs-collapse-dirs 3
        treemacs-silent-refresh t
        treemacs-is-never-other-window t
        treemacs-sorting 'alphabetic-asc
        treemacs-no-png-images t) ;; for terminal support
  ;; Expand automatically
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t))

;; Magit integration
(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package all-the-icons
  :if (display-graphic-p)
  :ensure t)

;; Treemacs icons
(use-package treemacs-all-the-icons
  :after treemacs
  :if (display-graphic-p)
  :ensure t
  :config
  (treemacs-load-theme "all-the-icons"))

;; Tree-sitter auto management
(use-package treesit-auto
  :ensure t
  :config
  (setq treesit-auto-install 'prompt)
  (treesit-auto-add-to-auto-mode-alist 'python)
  (global-treesit-auto-mode))

;; LSP client
(use-package eglot
  :ensure t
  :config)

;; Conda integration
(use-package conda
  :ensure t
  :config
  (setq conda-anaconda-home (expand-file-name "~/opt/homebrew/Caskroom/miniconda/"))
  (setq conda-env-home-directory conda-anaconda-home)
  (setq conda-env-subdirectory "envs")
  (conda-env-autoactivate-mode t))

;;; Python

(use-package python-ts-mode
  :ensure nil
  :mode "\\.py\\'"
  :hook (python-ts-mode . eglot-ensure)
  :config
  (setq eglot-workspace-configuration
      '(:pyright (:python.analysis.typeCheckingMode "off")))

  ;; Tell Eglot to use pyright as the LSP server
  (add-to-list 'eglot-server-programs
               '(python-ts-mode . ("pyright-langserver" "--stdio"))))

;; Version management
(use-package pyvenv
  :ensure t
  :config
  ;; Automatically activate a virtualenv if .venv exists in project root
  (add-hook 'python-ts-mode-hook
            (lambda ()
              (let ((venv-path (locate-dominating-file default-directory ".venv")))
                (when venv-path
                  (pyvenv-activate (expand-file-name ".venv" venv-path))
                  (message "Activated venv: %s" (expand-file-name ".venv"
                                                                  venv-path)))))))

;; Black auto-format on save
(use-package blacken
  :ensure t
  :hook (python-ts-mode . blacken-mode)
  :config
  (setq blacken-line-length 88))

;; Debugging
(use-package dap-mode
  :ensure t
  :after python-ts-mode
  :config
  (require 'dap-python)
  (setq dap-python-debugger 'debugpy)
  (setq dap-python-executable "python"))

;;; Julia

(use-package julia-ts-mode
  :ensure t
  :mode "\\.jl$"
  :hook (julia-ts-mode . eglot-ensure)
  :config
  ;; Tell eglot how to start Julia LSP
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '(julia-ts-mode . ("julia"
                                    "--startup-file=no"
                                    "--history-file=no"
                                    "-e" "using LanguageServer, SymbolServer; runserver()")))))

(use-package julia-snail
  :after julia-snail-mode eat
  :hook (julia-ts-mode . julia-snail-mode)
  :config
  (setq julia-snail-terminal-type 'eat))

;;; Shell

(defun my/shell-hook ()
  "Settings applied to Shell mode"
  (setq tab-width 2)
  (setq indent-tabs-mode nil))

(use-package shell
  :hook (shell-mode . my/shell-hook))

;;; Lisp

(defun my/lisp-hook ()
  "Minor modes for various Lisp modes."
  (paredit-mode 1)
  (rainbow-delimiters-mode ))

(dolist (hook '(emacs-lisp-mode-hook
                eval-expression-minibuffer-setup-hook
                ielm-mode-hook
                lisp-mode-hook
                lisp-interaction-mode-hook
                scheme-mode-hook))
  (add-hook hook #'my/lisp-hook))

;; Geiser (Scheme)
(use-package geiser
  :ensure t
  :config
  (setq geiser-active-implementations '(mit)
        geiser-mit-binary "/usr/local/bin/mit-scheme")
  (add-hook 'geiser-repl-mode-hook #'rainbow-delimiters-mode))

;; Slime
(use-package slime
  :ensure t
  :init
  (setq inferior-lisp-program "/usr/local/bin/sbcl")
  :config
  (slime-setup '(slime-fancy slime-repl)))

;; Clojure
(use-package cider
  :ensure t
  :defer t
  :init (add-hook 'cider-mode-hook #'clj-refactor-mode)
  :diminish subword-mode
  :config
  (setq nrepl-log-messages t
        cider-repl-display-in-current-window t
        cider-repl-use-clojure-font-lock t
        cider-prompt-save-file-on-load 'always-save
        cider-font-lock-dynamically '(macro core function var)
        nrepl-hide-special-buffers t
        cider-overlays-use-font-lock t)
  (cider-repl-toggle-pretty-printing))

;;; init.el ends here
