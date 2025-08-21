;;; init.el -*- lexical-binding: t; -*-

;;; Packages

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(require 'package)

(package-initialize)

(setq package-selected-packages
      '(auctex
        avy
        cape
        cider
        consult
        corfu
        doom-themes
        elpy
        embark
        embark-consult
        exec-path-from-shell
        expand-region
        flycheck
        geiser
        guide-key
        ivy
        julia-ts-mode
        julia-snail
        magit
        marginalia
        markdown-mode
        orderless
        paredit
        poly-org
        polymode
        savehist
        smex
        swiper
        vertico
        which-key
        rainbow-delimiters))

(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")
        ("org" . "http://orgmode.org/elpa/")))

(unless package-archive-contents
  (package-refresh-contents))

;;; Encoding

(set-default-coding-systems 'utf-8)     ; Default to UTF-8 encoding
(prefer-coding-system       'utf-8)     ; Add UTF-8 at the front for automatic detection
(set-terminal-coding-system 'utf-8)     ; Set coding system of terminal output
(set-keyboard-coding-system 'utf-8)     ; Set coding system for keyboard input on TERMINAL
(set-language-environment "English")    ; Set up multilingual environment

;;; General

(tool-bar-mode -1)                      ; Disable tool bar
(menu-bar-mode t)                       ; Display a drawer-style menu bar
(show-paren-mode 1)                     ; Highlight matching delimiters
(electric-pair-mode t)                  ; Automatically close delimiters
(defalias 'yes-or-no-p 'y-or-n-p)       ; 'y' and 'n' instead of 'yes' and 'no'
(setq-default
 inhibit-startup-screen t               ; Inhibit startup screen
 inhibit-startup-message t              ; Inhibit startup message
 inhibit-startup-echo-area-message t    ; Disable initial echo message
 initial-scratch-message ""             ; Empty the initial *scratch* buffer
 initial-buffer-choice t                ; Open *scratch* buffer at init
 visible-bell t                         ; Visual bell instead of beep
 column-number-mode t                   ; Show current column in the mode line
 backup-inhibited t                     ; Disable auto-backup
 indent-tabs-mode nil                   ; Spaces instead of tabs when indenting
 fill-column 80                         ; Wrap lines at 79 characters
 auto-fill-function 'do-auto-fill       ; Automatically break long lines
 echo-keystrokes -1                     ; Echo keystrokes instantaneously
 cursor-type 'box                       ; Set the cursor style to a box
 blink-cursor-mode nil                  ; Disable cursor blinking
 auto-save-default nil)                 ; Disable auto-save

;; Delete trailing whitespace upon save.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Enable mouse usage when running in a terminal.
(unless window-system
  (xterm-mouse-mode))

;;; GUI

;; Only run at startup
(when (and window-system
           (not after-init-time))
  (set-frame-size
   (selected-frame) 120 60)             ; Frame size
  (set-frame-font "Lilex Nerd Font-12") ; Font and font size
  (scroll-bar-mode -1))                 ; Disable scroll bar

;; Enable smooth per-pixel scrolling (Emacs 29+).
;; Makes trackpad/mouse-wheel scrolling fluid and precise.
(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode 1))

;;; macOS

;; Make Emacs frames full-size, not compact utility windows
(setq default-frame-alist
      '((ns-appearance . light) ;; or 'light
        (ns-transparent-titlebar . nil)))

(when (eq system-type 'darwin)
  ;; Modifier keys
  (setq mac-control-modifier 'super
        mac-command-modifier 'ctrl
        mac-option-modifier  'meta)

  ;; Load the actual path environment
  (exec-path-from-shell-initialize)

  ;; Frame appearance
  (setq frame-title-format nil)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))

  ;; Apply theme based on system appearance
  (add-hook 'ns-system-appearance-change-functions #'my/apply-theme))

;;; Appearance

(straight-use-package
 '(rose-pine-doom-emacs
   :host github :repo
   "donniebreve/rose-pine-doom-emacs"
   :branch "main"))

(add-to-list 'custom-theme-load-path
             (straight--build-dir "rose-pine-doom-emacs"))

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

;; Auto complete commands
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; Old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; Quickly grow/shrink selection
(use-package expand-region
  :bind (("C-=" . er/expand-region)
         ("C--" . er/contract-region)))

;; Org-mode configuration
(with-eval-after-load 'org
  (define-key global-map "\C-cl" 'org-store-link)
  (define-key global-map "\C-ca" 'org-agenda)
  (defvar org-log-done t)
  ;; Define my agenda files
  (defvar org-agenda-files (list "~/org/work.org"
                                 "~/org/studies.org"
                                 "~/org/personal.org"))
  ;; Use actual circular bullets in lists.
  (font-lock-add-keywords 'org-mode
                          '(("^ +\\([-*]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
  ;; Hide leading stars.
  (defvar org-hide-leading-stars t)
  ;; Don't fold headlines when opening an Org file.
  (defvar org-inhibit-startup-visibility-stuff t)
  ;; Display entities as symbols.
  (defvar org-pretty-entities t)
  ;; Hide emphasis markers.
  (defvar org-hide-emphasis-markers t))

(defun my/org-hook ()
  "Execute these settings when going into org-mode."
  (poly-org-mode 1))

(add-hook 'org-mode-hook 'my/org-hook)

(defun my/prog-mode-hooks ()
  "Settings applied to programming modes."
  ;; Highlight current line
  (hl-line-mode)
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
  (hl-line-mode)
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
  :config
  ;; Optional settings
  ;; (setq corfu-auto t)         ;; auto-complete without M-TAB
  ;; (setq corfu-auto-delay 0.0) ;; no delay
  ;; (setq corfu-auto-prefix 1)  ;; start after 1 char
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

;; ;; Disable Checkdoc warnings when editing Emacs Lisp files
;; (with-eval-after-load 'flycheck
;;   (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

;; ;; Enable a spell checker.
;; (global-flycheck-mode)
;; (setq-default ispell-dictionary "american"
;;               ispell-local-dictionary "american")

;; When non-nil, display available key-bindings on combined keypresses.
(guide-key-mode 1)

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
(straight-use-package
 '(eat :type git
       :host codeberg
       :repo "akib/emacs-eat"
       :files ("*.el"
               ("term" "term/*.el")
               "*.texi"
               "*.ti"
               ("terminfo/e" "terminfo/e/*")
               ("terminfo/65" "terminfo/65/*")
               ("integration" "integration/*")
               (:exclude ".dir-locals.el" "*-tests.el"))))

(with-eval-after-load 'eat
  (setq eat-term-name "xterm-256color")
  (setq eat-enable-mouse t)
  (setq eat-scroll-to-bottom-on-output 'this)
  ;; Keep prompt at bottom like a normal terminal
  (add-hook 'eat-mode-hook
            (lambda ()
              (setq-local scroll-conservatively 1000)
              (setq-local scroll-step 1)
              (setq-local scroll-margin 0)
              (setq-local auto-window-vscroll nil))))

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

;; Ruff linting via Flymake
(use-package flymake-ruff
  :straight (flymake-ruff :type git :host github :repo "erickgnavar/flymake-ruff")
  :hook (python-ts-mode . flymake-ruff-load))

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
