;;; early-init.el -*- lexical-binding: t; no-byte-compile:

;;; Startup performance

;; Prefer loading newer compiled files
(setq load-prefer-newer t)

;; Font compacting can be very resource-intensive, especially when rendering
;; icon fonts on Windows. This will increase memory usage.
(setq inhibit-compacting-font-caches t)

(defvar doom--file-name-handler-alist file-name-handler-alist)

(setq gc-cons-threshold most-positive-fixnum
      ;; package-quickstart t
      file-name-handler-alist nil
      frame-inhibit-implied-resize t
      native-comp-async-report-warnings-errors nil)

(add-hook
 'emacs-startup-hook
 (lambda ()
   (setq gc-cons-threshold (* 100 1024 1024) ; 100mb
         file-name-handler-alist doom--file-name-handler-alist)
   (message "*** Emacs loaded in %.2f seconds with %d garbage collections."
            (float-time (time-subtract after-init-time before-init-time))
            gcs-done)
   (let ((private-file (concat user-emacs-directory "private.el")))
     (when (file-exists-p private-file)
       (load-file private-file)))))

;; Supress warnings
(setq byte-compile-warnings '(not obsolete)
      warning-suppress-log-types '((comp) (bytecomp))
      native-comp-async-report-warnings-errors 'silent)

;;; Package management

(setq straight-disable-mixed-use-warnings t)
(setq package-enable-at-startup )       ; Prevent Emacs from initializing
                                        ; packages before init.el runs
(setq package-quickstart nil)           ; Disable package.el’s quickstart
                                        ; feature

;;; Miscellaneous

(setq default-input-method nil)         ; Don't assume any input method as the
                                        ; default
(provide 'early-init)

;;; early-init.el ends here
