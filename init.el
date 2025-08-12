;;; init.el -*- lexical-binding: t; -*-

;;; Packages

(require 'package)

(package-initialize)

(setq package-selected-packages
  '(auctex
    avy
    cider
    counsel
    doom-themes
    eat
    elpy
    exec-path-from-shell
    expand-region
    flycheck
    geiser
    guide-key
    ivy
    julia-ts-mode
    julia-snail
    magit
    markdown-mode
    paredit
    poly-org
    polymode
    smex
    swiper
    which-key
    rainbow-delimiters))

(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")
        ("org" . "http://orgmode.org/elpa/")))

(unless package-archive-contents
  (package-refresh-contents))

(unless (cl-every 'package-installed-p package-selected-packages)
  (package-install-selected-packages))

;;; Encoding

(set-default-coding-systems 'utf-8)     ; Default to UTF-8 encoding
(prefer-coding-system       'utf-8)     ; Add UTF-8 at the front for automatic detection
(set-terminal-coding-system 'utf-8)     ; Set coding system of terminal output
(set-keyboard-coding-system 'utf-8)     ; Set coding system for keyboard input on TERMINAL
(set-language-environment "English")    ; Set up multilingual environment

;;; General

(tool-bar-mode -1)                      ; Disable tool bar
(menu-bar-mode t)                       ; Display a drawer-style menu bar
(global-hl-line-mode t)                 ; Highlight current line
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
 auto-save-default nil)                 ; Disable auto-save

;; Delete trailing whitespace upon save.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Enable mouse usage when running in a terminal.
(unless window-system
  (xterm-mouse-mode))

;;; GUI

(when window-system
  (set-frame-size
   (selected-frame) 110 43)             ; Frame size
  (set-frame-font "Lilex Nerd Font-12") ; Font and font size
  (scroll-bar-mode -1))                 ; Disable scroll bar

;;; macOS

(when (eq system-type 'darwin)
  (setq mac-control-modifier 'ctrl)
  (setq mac-command-modifier 'super)
  (setq mac-option-modifier 'meta)
  ;; Load the actual path environment.
  (exec-path-from-shell-initialize)
  ;; Do not display a text or an icon within the titlebar.
  (setq frame-title-format nil)
  ;; This will cause the titlebar text to be visible on a light background.
  (add-to-list 'default-frame-alist '(ns-appearance . light))
  ;; Use a transparent titlebar.
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t)))

;;; Appearance

(defadvice load-theme (before clear-previous-themes activate)
  "Clear existing theme instead of layering them."
  (mapc #'disable-theme custom-enabled-themes))

;; Source additional themes.
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

;; Load custom theme.
(load-theme 'doom-rose-pine-dawn t)

;; Auto complete commands.
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; Old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(defun my/show-line-numbers-while-goto-line ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
        (display-line-numbers-mode 1)
        (call-interactively #'goto-line))
    (display-line-numbers-mode -1)))

(global-set-key (kbd "M-g g") #'my/show-line-numbers-while-goto-line)

(use-package expand-region
  :bind ("C-=" . er/expand-region))

;; Org-mode configuration.
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

(defun my-org-hook ()
  "Execute these settings when going into org-mode."
  (poly-org-mode 1))

(add-hook 'org-mode-hook 'my-org-hook)

(defun my-prog-mode-hooks ()
  "Minor modes used for all programming modes."
  ;; Show line numbers.
  ;; (display-line-numbers-mode)
  ;; Display relative line numbers.
  ;; (setq display-line-numbers 'relative)
  ;; Display an indiciation of the ‘fill-column’ position
  ;; (display-fill-column-indicator-mode)
  ;; Show, in the echo area, the argument list of the function call you are
  ;; currently writing.
  (eldoc-mode 1))

;;; Programming modes
(add-hook 'prog-mode-hook 'my-prog-mode-hooks)

;; Provide suggestions when opening files or switching buffers.
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; Display available keybindings in a popup.
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;; Avy configuration.
(global-set-key (kbd "C-:") 'avy-goto-char)
(global-set-key (kbd "C-'") 'avy-goto-char-2)
(global-set-key (kbd "M-g f") 'avy-goto-line)
(global-set-key (kbd "M-g w") 'avy-goto-word-1)
(global-set-key (kbd "M-g e") 'avy-goto-word-0)

;; Ivy configuration.
(ivy-mode 1)
(setq-default ivy-use-virtual-buffers t)
(setq-default enable-recursive-minibuffers t)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(define-key read-expression-map (kbd "C-r") 'counsel-expression-history)

;; Load markdown-mode.
(autoload 'gfm-mode "markdown-mode.el" "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(setq auto-mode-alist (cons '("\\.text$" . gfm-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.md$" . markdown-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.markdown$" . markdown-mode) auto-mode-alist))

;; ;; Disable Checkdoc warnings when editing Emacs Lisp files
;; (with-eval-after-load 'flycheck
;;   (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

;; ;; Enable a spell checker.
;; (global-flycheck-mode)
;; (setq-default ispell-dictionary "american"
;;               ispell-local-dictionary "american")

;; Show line numbers in AUXTeX.
(add-hook 'tex-mode-hook 'display-line-numbers-mode)
(add-hook 'LaTeX-mode-hook 'display-line-numbers-mode)
(add-hook 'bibtex-mode-hook 'display-line-numbers-mode)

;; Show line numbers in shell-script-mode.
(add-hook 'sh-mode-hook 'display-line-numbers-mode)

;; When non-nil, display available key-bindings on combined keypresses.
(guide-key-mode 1)

;; Enable Polymode.
(with-eval-after-load 'poly-R)
(with-eval-after-load 'poly-markdown)
(with-eval-after-load 'poly-org)

;; Terminal emulation
(use-package eat
  :ensure t
  :config
  (setq eat-term-name "xterm-256color"))

;; R modes
(add-to-list 'auto-mode-alist '("\\.Snw" . poly-noweb+r-mode))
(add-to-list 'auto-mode-alist '("\\.Rnw" . poly-noweb+r-mode))
(add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode))

;; Julia setup
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

;; Lisp modes.
(defun my-lisp-hook ()
  "Minor modes that should be loaded in various Lisp modes."
  ;; Highlight parentheses, brackets or braces according to their depth.
  (rainbow-delimiters-mode)
  (paredit-mode 1)
  (eldoc-mode 1))

;; Geiser-specifics
(defvar geiser-mit-binary "/usr/local/bin/mit-scheme")
(defvar geiser-active-implementations '(mit))
(add-hook 'geiser-repl-mode-hook 'rainbow-delimiters-mode)

;; Slime-specifics
(setq-default inferior-lisp-program "/usr/local/bin/sbcl")

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

;; Add hooks to each Lisp mode
(add-hook 'emacs-lisp-mode-hook #'my-lisp-hook)
(add-hook 'eval-expression-minibuffer-setup-hook #'my-lisp-hook)
(add-hook 'ielm-mode-hook #'my-lisp-hook)
(add-hook 'lisp-mode-hook #'my-lisp-hook)
(add-hook 'lisp-interaction-mode-hook #'my-lisp-hook)
(add-hook 'scheme-mode-hook #'my-lisp-hook)

;;; init.el ends here
