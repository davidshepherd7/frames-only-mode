;;; frames-only-mode.el --- Use frames instead of emacs windows -*- lexical-binding: t; -*-

;; Copyright (C) 2015 Free Software Foundation, Inc.

;; Author: David Shepherd <davidshepherd7@gmail.com>
;; Version: 0.0.0
;; Package-Requires: ((emacs "24") (let-mode))
;; Keywords: frames windows
;; URL: https://github.com/davidshepherd7/frames-only-mode

;;; Commentary:

;; Collection of settings and code to use frames instead of emacs
;; "windows".

;; To automatically open a "useful" buffer in new frames xmonads binding
;; for a new frame is set to "emacsclient -c -n -e '(switch-to-buffer
;; nil)'". For other window managers something similar should work...


;;; Code:

(require 'let-mode)

;; Ensure that some variables we will be setting are loaded
(require 'org-agenda)
(require 'ediff-wind)



;;; Options:

(defgroup frames-only-mode '()
  "Use frames instead of emacs windows."
  :group 'frames)

(defcustom frames-only-mode-kill-frame-when-buffer-killed-buffer-list
  '("*RefTeX Select*" "*Help*" "*Popup Help*" "*Completions*")
  "Buffer names for which the containing frame should be
 killed when the buffer is killed."
  :group 'frames-only-mode)


(defcustom frames-only-mode-use-windows-for-completion t
  "Use emacs windows for display of completions.

This is useful because a new completion frame would steal
window manager focus.

Completion windows are always split horizontally (helm style).

To disable completion popups entirely use the variable
`completion-auto-help' for default emacs completion or
`ido-completion-buffer' for ido-based completion. "
  :group 'frames-only-mode)

(defcustom frames-only-mode-use-window-functions
  (list #'calendar)
  "A list of functions inside which new emacs windows should be created instead of frames.

(i.e. pop-up-frames is let bound to nil, the default value)."
  :group 'frames-only-mode)




(defvar frames-only-mode--revert-fn #'ignore
  "Storage for function to revert changes to variables made by frames-only-mode")

(defun frames-only-mode-advice-use-windows (fun &rest args)
  "Create new emacs windows instead of frames within this function"
  (let ((pop-up-frames nil))
    (apply fun args)))


(defun super-abort-recursive-edit ()
  "kill any sub-windows and abort recursive edit."
  (interactive)

  ;; Biggest window is probably the "main" one, select it and delete the
  ;; rest.
  (select-window (get-largest-window 't 'nil 'nil))
  (delete-other-windows)

  (abort-recursive-edit))


(defun frames-only-mode-kill-frame-if-current-buffer-matches ()
  "Kill frames as well when certain buffers are closed (but only
  if there is only a single window in the frame), helps stop some
  packages spamming frames."
  (when (and (one-window-p)
             (member (buffer-name) frames-only-mode-kill-frame-when-buffer-killed-buffer-list))
    (delete-frame)))


(defun frames-only-mode-advice-use-windows-for-completion (orig-fun &rest args)
  (let ((pop-up-frames (not frames-only-mode-use-windows-for-completion))
        (split-width-threshold 9999))
    (apply orig-fun args)))


(defadvice bury-buffer (around frames-only-mode-kill-frame-if-current-buffer-matches activate)
  "Kill the frame when burying certain buffers (but only if there
  is only a single window in the frame)."
  (let ((buffer-to-bury (buffer-name)))
    ad-do-it
    (when (and (one-window-p)
               (member buffer-to-bury frames-only-mode-kill-frame-when-buffer-killed-buffer-list))
      (delete-frame))))

(defun frames-only-mode-bury-completions ()
  (when (get-buffer "*Completions*")
    (bury-buffer "*Completions*")))




(define-minor-mode frames-only-mode
  "Use frames instead of emacs windows."
  :global t

  (if frames-only-mode
      (setq frames-only-mode--revert-fn
            (let-mode-revertable-set
             ;; Make new frames instead of new windows, the main setting
             'pop-up-frames 'graphic-only


             ;; Focus follows mouse (for emacs windows) off to prevent crazy things
             ;; happening when I click on e.g. compilation error links. Would do nothing
             ;; interesting anyway if everything is working because there are no windows
             ;; within frames.
             'mouse-autoselect-window nil
             'focus-follows-mouse nil

             ;; kill frames when a buffer is buried, makes most things play nice with
             ;; frames
             'frame-auto-hide-function 'delete-frame

             ;; gdb (gud) does things with windows by default, this stops some of it:
             ;; 'gdb-use-separate-io-buffer nil
             ;; 'gdb-many-windows nil


             ;; org windows: Use frames not emacs windows
             'org-agenda-window-setup 'other-frame
             'org-src-window-setup 'other-frame

             ;; Use a single frame for ediff (without this you end up with an entire
             ;; frame for the control buffer, this doesn't work well at all with tiling
             ;; window managers).
             'ediff-window-setup-function 'ediff-setup-windows-plain))
    ;; else
    (funcall frames-only-mode--revert-fn))


  ;; Disable in some functions as specified by customisation
  (if frames-only-mode
      (mapc (lambda (fun) (advice-add fun :around #'frames-only-mode-advice-use-windows)) frames-only-mode-use-window-functions)
    (mapc (lambda (fun) (advice-remove fun #'frames-only-mode-advice-use-windows)) frames-only-mode-use-window-functions))


  ;; Key bind to close sub-windows (e.g. as created by re-builder or
  ;; calendar) with the same key as abort-recursive-edit.
  (global-set-key [remap abort-recursive-edit] #'super-abort-recursive-edit)


  ;; Hacks to make other things play nice by killing the frame when certain
  ;; buffers are closed.
  (if frames-only-mode
      (add-hook 'kill-buffer-hook #'frames-only-mode-kill-frame-if-current-buffer-matches)
    (remove-hook 'kill-buffer-hook #'frames-only-mode-kill-frame-if-current-buffer-matches))


  (when (require 'magit nil 'noerror)

    ;; Use the current frame/window to enter the magit commit message
    (set 'magit-server-window-for-commit nil)

    ;; Don't auto popup a magit diff buffer when commiting, can still get it
    ;; if needed with C-c C-d. The variable name for this changed in magit
    ;; version 2.30, so for now we will set both variables (28/11/2015).
    ;; TODO: remove in a year or two.
    (if (boundp 'magit-commit-show-diff)
        (set 'magit-commit-show-diff nil)
      (set 'magit-diff-auto-show nil))

    )


  ;; Advise completion popup functions to use windows instead of frames if
  ;; the custom setting is true.
  (if frames-only-mode
      (advice-add #'minibuffer-completion-help :around #'frames-only-mode-advice-use-windows-for-completion)
    (advice-remove #'minibuffer-completion-help #'frames-only-mode-advice-use-windows-for-completion))

  (if frames-only-mode
      (advice-add #'ido-completion-help :around #'frames-only-mode-advice-use-windows-for-completion)
    (advice-remove #'ido-completion-help #'frames-only-mode-advice-use-windows-for-completion))

  ;; Make sure completions buffer is buried after we are done with the minibuffer
  (if frames-only-mode
      (add-hook 'minibuffer-exit-hook #'frames-only-mode-bury-completions)
    (remove-hook 'minibuffer-exit-hook #'frames-only-mode-bury-completions)))



(provide 'frames-only-mode)

;;; frames-only-mode.el ends here
