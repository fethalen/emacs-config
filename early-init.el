;;; early-init.el -*- lexical-binding: t; no-byte-compile:

;;; Startup performance

;; Reduce file-handling overhead during startup
(defvar my/file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Increase garbage collection threshold during init
(setq gc-cons-threshold most-positive-fixnum)

;; Restore settings after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist my/file-name-handler-alist
                  gc-cons-threshold 8000000)))

;; Prefer loading newer compiled files
(setq load-prefer-newer t)

;; Font compacting can be very resource-intensive, especially when rendering
;; icon fonts on Windows. This will increase memory usage.
(setq inhibit-compacting-font-caches t)

;;; Package management

(setq package-quickstart nil)           ; Disable package.el’s quickstart
                                        ; feature

;;; UI

;; Disable unneeded UI elements
(tool-bar-mode -1)                      ; No toolbar
(menu-bar-mode -1)                      ; No menubar
(scroll-bar-mode -1)                    ; No scrollbar
(blink-cursor-mode -1)                  ; Solid cursor

;; Frame defaults
(add-to-list 'default-frame-alist '(width . 120))
(add-to-list 'default-frame-alist '(height . 60))
(add-to-list 'default-frame-alist '(font . "Lilex Nerd Font-12"))
(add-to-list 'default-frame-alist '(line-spacing . 0.15))

;; macOS-specific frame settings
(when (eq system-type 'darwin)
  ;; Light appearance
  (add-to-list 'default-frame-alist '(ns-appearance . light))
  ;; Transparent titlebar
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  ;; Remove proxy icon in titlebar
  (add-to-list 'default-frame-alist '(ns-use-proxy-icon . nil))
  ;; Remove shadow around titlebar
  (add-to-list 'default-frame-alist '(ns-titlebar-transparent . t)))

;;; Miscellaneous

(setq default-input-method nil)         ; Don't assume any input method as the
                                        ; default

;; Suppress native compilation warnings
(setq native-comp-async-report-warnings-errors nil)

(provide 'early-init)

;;; early-init.el ends here
