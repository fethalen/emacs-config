;;; early-init.el -*- lexical-binding: t; no-byte-compile:

;;; Startup performance

;; Prefer loading newer compiled files
(setq load-prefer-newer t)

;; Font compacting can be very resource-intensive, especially when rendering
;; icon fonts on Windows. This will increase memory usage.
(setq inhibit-compacting-font-caches t)

;; ;;; Package management

(setq package-quickstart nil)           ; Disable package.el’s quickstart
                                        ; feature

;;; Miscellaneous

(setq default-input-method nil)         ; Don't assume any input method as the
                                        ; default
(provide 'early-init)

;;; early-init.el ends here
