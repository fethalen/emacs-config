;;; init.el -*- lexical-binding: t; -*-

;;; Packages

(package-initialize)

(setq package-selected-packages
      '(affe
        auctex
        avy
        bash-ts-mode
        blacken
        cape
        cider
        conda
        consult
        corfu
        dap-mode
        doom-themes
        eat
        eglot
        embark
        embark-consult
        exec-path-from-shell
        expand-region
        flycheck
        geiser
        julia-snail
        julia-ts-mode
        magit
        marginalia
        markdown-mode
        orderless
        org
        paredit
        python-ts-mode
        pyvenv
        rainbow-delimiters
        savehist
        shfmt
        slime
        tempel
        tempel-collection
        vertico
        vterm
        which-key
        yasnippet
        yasnippet-snippets))

(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

(unless package-archive-contents
  (package-refresh-contents))

(setq use-package-always-ensure t       ; Install packages if missing
      use-package-always-defer t)       ; Load lazily unless explicitly demanded

;;; Encoding

(set-default-coding-systems 'utf-8)     ; Default to UTF-8 encoding
(prefer-coding-system       'utf-8)     ; Add UTF-8 at the front for automatic detection
(set-terminal-coding-system 'utf-8)     ; Set coding system of terminal output
(set-keyboard-coding-system 'utf-8)     ; Set coding system for keyboard input on TERMINAL
(set-language-environment "English")    ; Set up multilingual environment

;;; General

;; On macOS, GUI Emacs does not inherit the shell environment. This
;; ensures that GUI frames on macOS/X11 gets the same PATH as your
;; terminal.
(use-package exec-path-from-shell
  :demand t
  :if (memq window-system '(mac ns x))
  :config
  ;; Use a non-interactive shell (faster)
  (setq exec-path-from-shell-arguments '("-l"))
  ;; Initialize environment from the user's shell
  (exec-path-from-shell-initialize))

;; Core Emacs settings
(use-package emacs
  :ensure nil
  :init
  (show-paren-mode 1)                   ; Highlight matching parens
  (electric-pair-mode 1)                ; Auto-close pairs
  (defalias 'yes-or-no-p 'y-or-n-p)
  :hook
  (
   ;; Soft-wrap long lines at word boundaries and move by visual lines
   (text-mode . visual-line-mode)
   ;; Remove trailing whitespace in buffer upon save
   (before-save . delete-trailing-whitespace)
   ;; Make script executable if hash-bang is found
   (after-save . executable-make-buffer-file-executable-if-script-p))
   :config
  (setq
   ;; Cursor & feedback
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

    ;; Enable mouse usage when running in a terminal.
  (unless window-system
    (xterm-mouse-mode))

  ;; Enable smooth per-pixel scrolling (introduced in Emacs v29.1)
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
    (add-hook 'ns-system-appearance-change-functions #'my/apply-theme)))

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

;;; Editing

(use-package prog-mode
  :ensure nil
  :hook (prog-mode . (lambda ()
                       (eldoc-mode 1))))

(use-package text-mode
  :ensure nil)

(use-package minibuffer
  :ensure nil
  :hook (minibuffer-setup . (lambda ()
                              (when (bound-and-true-p paredit-mode)
                                (paredit-mode -1)))))

;; Quickly grow/shrink selection
(use-package expand-region
  :bind (("C-=" . er/expand-region)
         ("C--" . er/contract-region)))

;; Plain-text system for notes, tasks, and literate programming
(use-package org
  :ensure nil
  :hook (org-babel-after-execute . my/org-babel-ansi-color)
  :bind (:map org-mode-map
              ("C-c C-r" . my/org-babel-restart-session))
  :config
  (defun my/org-babel-ansi-color ()
    "Apply ANSI color codes in the current Org babel result."
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (point-min) (point-max))))

  ;; Enable execution of shell scripts in Org Babel code blocks
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t)))

  ;; Use the appropriate Tree-sitter mode for shell code blocks
  (dolist (lang '("sh" "bash" "shell" "ash" "jsh" "bash2" "dash" "dtksh"
                  "ksh" "es" "rc" "itcsh" "tcsh" "jcsh" "csh" "ksh88"
                  "oash" "pdksh" "mksh" "posix" "wksh" "wsh" "zsh" "rpm"))
    (setf (alist-get lang org-src-lang-modes nil nil #'equal) 'bash-ts)
    (setf (alist-get "zsh"  org-src-lang-modes nil nil #'equal) 'zsh-ts)
    (setf (alist-get "sh"   org-src-lang-modes nil nil #'equal) 'bash-ts))

  ;; Don’t let sh-script second-guess the shell
  (setq sh-shell-file "bash")

  (setq ;; org-src-fontify-natively t
   ;; org-src-tab-acts-natively t
   ;; org-edit-src-content-indentation 0
   org-confirm-babel-evaluate nil)

  (setq org-directory "~/org"
        org-agenda-files '("~/org/tasks.org" "~/org/projects.org")
        org-default-notes-file "~/org/inbox.org"
        org-startup-indented t
        org-ellipsis " ▾"))

;; Display the keybindings following an incomplete command in a pop up
(use-package which-key
  :ensure nil
  :demand t
  :config
  (which-key-mode))

;; Fast, on-screen navigation by character, word, or line
(use-package avy
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
  :init (vertico-mode))

;; Orderless completions
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides
   '((file (styles orderless partial-completion)))))

;; Annotations in minibuffer
(use-package marginalia
  :init (marginalia-mode))

;; Commands & enhanced navigation
(use-package consult
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
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim))
  :init (setq prefix-help-command #'embark-prefix-help-command))

;; Embark + Consult integration
(use-package embark-consult
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;; Persist minibuffer history across sessions
(use-package savehist
  :ensure nil
  :init
  (setq history-length 1000
        history-delete-duplicates t
        savehist-additional-variables '(extended-command-history))
  (savehist-mode 1))

;; Async fuzzy finder
(use-package affe
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
  :init
  ;; Add useful default completion sources
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-keyword))

;; Markdown & GitHub Flavored Markdown
(use-package markdown-mode
  :mode (("\\.md\\'"       . gfm-mode)
         ("\\.markdown\\'" . markdown-mode)
         ("\\.text\\'"     . gfm-mode))
  :init
  (autoload 'gfm-mode "markdown-mode" "Major mode for editing GitHub Flavored Markdown" t))

(use-package tramp
  :ensure nil
  :config
  (setq
   ;; Don't create lock files on remote systems
   remote-file-name-inhibit-locks t
   ;; Disable auto-saving of remote buffers
   remote-file-name-inhibit-auto-save-visited t
   ;; Use an external tool to move files exceeding the file size limit
   tramp-copy-size-limit (* 1024 1024) ;; 1MB
   ;; Verbosity level for TRAMP messages:
   ;; 0 = silent, 1 = errors only, 2 = warnings,
   ;; 3 = info (default), 6 = debug (very noisy).
   tramp-verbose 1
   ;;Default to SSH
   tramp-default-method "ssh"
   ;; Tell Magit to use a pseudo-terminal (pty) for TRAMP pipes
   magit-tramp-pipe-stty-settings 'pty)

  ;; Enable TRAMP direct async process mode for faster remote command execution
  (connection-local-set-profile-variables
   'remote-direct-async-process
   '((tramp-direct-async-process . t)))

  ;; Apply the above profile to all TRAMP connections using the "scp" protocol
  (connection-local-set-profiles
   '(:application tramp :protocol "scp")
   'remote-direct-async-process)

  ;; Tell Magit to use a pseudo-terminal (pty) for TRAMP pipes
  (setq magit-tramp-pipe-stty-settings 'pty)

  ;; Use remote PATH over local one
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

;; Terminal emulation
(defun my/terminal-hook ()
  "Configuration for terminal buffers."
  (setq-local
   line-number-mode nil
   column-number-mode nil
   scroll-conservatively 1000
   scroll-step 1
   scroll-margin 0
   auto-window-vscroll nil
   truncate-lines t))

(use-package eat
  :hook (eat-mode . my/terminal-hook)
  :custom
  (eat-term-name "xterm-256color")
  (eat-enable-mouse t)
  (eat-scroll-to-bottom-on-output 'this)
  (eat-term-scrollback 10000))

(use-package vterm
  :hook (vterm-mode . my/terminal-hook))

(use-package treesit
  :ensure nil
  :preface
  (dolist (mapping
	   '((sh-mode . bash-ts-mode)
	     (sh-base-mode . bash-ts-mode)
	     (bash-mode . bash-ts-mode)
	     (python-mode . python-ts-mode))))
  :config
  (setq treesit-language-source-alist
        '((bash "https://github.com/tree-sitter/tree-sitter-bash")
          (zsh  "https://github.com/nvim-treesitter/tree-sitter-zsh")
          (fish "https://github.com/ram02z/tree-sitter-fish"))))

;; LSP client
(use-package eglot
  :ensure nil
  :config)

;; Conda integration
(use-package conda
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
  :hook (python-ts-mode . blacken-mode)
  :config
  (setq blacken-line-length 88))

;; Debugging
(use-package dap-mode
  :after python-ts-mode
  :config
  (require 'dap-python)
  (setq dap-python-debugger 'debugpy)
  (setq dap-python-executable "python"))

;;; Julia

(use-package julia-ts-mode
  :mode "\\.jl\\'"
  :hook (julia-ts-mode . eglot-ensure)
  :config
  ;; Tell Eglot how to start Julia LSP
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '(julia-ts-mode . ("julia"
                                    "--startup-file=no"
                                    "--history-file=no"
                                    "-e" "using LanguageServer, SymbolServer; runserver()")))))

(use-package julia-snail
  :after eat
  :hook (julia-ts-mode . julia-snail-mode)
  :config
  (setq julia-snail-terminal-type 'eat))

;;; Shell

;; Use bash-ts-mode over sh-mode for any files associated with that
;; mode
(dolist (entry auto-mode-alist)
  (when (eq (cdr entry) 'sh-mode)
    (setcdr entry 'bash-ts-mode)))

(defun my/shell-hook ()
  "Settings applied when editing shell scripts."
  (setq-local indent-tabs-mode nil
	      sh-basic-offset 2))

;; Tree-sitter based Bash mode
(use-package bash-ts-mode
  :ensure nil
  :mode "\\.\\(?:sh\\|bash\\)$"
  :interpreter (("bash" . bash-ts-mode)
                ("sh"   . bash-ts-mode))
  :hook ((bash-ts-mode . my/shell-hook)
	 (bash-ts-mode . eglot-ensure))
  :init
  ;; Let bash-ts-mode derive features from sh-mode
  (put 'bash-ts-mode 'derived-mode-parent 'sh-mode)
  :config
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '(bash-ts-mode . ("bash-language-server" "start")))))

;; Syntax checking with Flycheck (needs shellcheck)
(use-package flycheck
  :hook (bash-ts-mode . flycheck-mode)
  :config
  (setq flycheck-sh-shellcheck-executable "shellcheck"))

;; Snippets
(use-package yasnippet
  :hook (bash-ts-mode . yas-minor-mode)
  :config
  (yas-reload-all))

(use-package yasnippet-snippets
  :after yasnippet)

(use-package tempel
  :bind (("M-+" . tempel-complete) ;; alternative to Yasnippet expansion
         ("M-*" . tempel-insert))  ;; prompt for template and insert
  :custom
  ;; Default templates
  (tempel-path (expand-file-name "templates.eld" user-emacs-directory)))

(use-package tempel-collection
  :after tempel) ;; optional pre-built collection

;; Formatting (needs shfmt installed)
(use-package shfmt
  :hook (bash-ts-mode . shfmt-on-save-mode)
  :config
  (setq shfmt-arguments '("-i" "2" "-ci")))

;; Consult integration
(use-package consult
  :bind (("C-c f" . consult-flycheck)))

;;; Lisp

(use-package paredit)

(use-package rainbow-delimiters)

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
  :config
  (setq geiser-active-implementations '(mit)
        geiser-mit-binary "/usr/local/bin/mit-scheme")
  (add-hook 'geiser-repl-mode-hook #'rainbow-delimiters-mode))

;; Slime
(use-package slime
  :init
  (setq inferior-lisp-program "/usr/local/bin/sbcl")
  :config
  (slime-setup '(slime-fancy slime-repl)))

;; Clojure
(use-package cider
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
