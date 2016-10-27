;;; frames-only-mode.el --- Use frames instead of Emacs windows -*- lexical-binding: t; -*-

;; Copyright (C) 2015 Free Software Foundation, Inc.

;; Author: David Shepherd <davidshepherd7@gmail.com>
;; Version: 0.0.0
;; Package-Requires: ((emacs "24.4") (seq "2.3"))
;; Keywords: frames, windows
;; URL: https://github.com/davidshepherd7/frames-only-mode


;;; Commentary:

;;; Code:

(require 'seq)

;; Ensure that some variables we will be setting are loaded. Strictly speaking
;; we should use eval-after-load but for now I don't want to implement
;; revertable-set (see below) combined with eval-after-load because it sounds
;; like a nightmare.
(require 'org-agenda)
(require 'ediff-wind)



;;; Options:

(defgroup frames-only-mode '()
  "Use frames instead of emacs windows."
  :group 'frames)

(defcustom frames-only-mode-kill-frame-when-buffer-killed-buffer-list
  '("*RefTeX Select*" "*Help*" "*Popup Help*" "*Completions*")
  "Buffer names for which the containing frame should be killed when the buffer is killed."
  :group 'frames-only-mode)


(defcustom frames-only-mode-use-windows-for-completion t
  "Use Emacs windows for display of completions.

This is useful because a new completion frame would steal
window manager focus.

Completion windows are always split horizontally (helm style).

To disable completion popups entirely use the variable
`completion-auto-help' for default Emacs completion or
`ido-completion-buffer' for ido-based completion."
  :group 'frames-only-mode)

(defcustom frames-only-mode-use-window-functions
  (list #'calendar #'report-emacs-bug #'checkdoc)
  "A list of functions inside which new emacs windows should be created instead of frames.

\(i.e. pop-up-frames is let bound to nil, the default value)."
  :group 'frames-only-mode)



;;; Revertable-set helpers (from https://github.com/davidshepherd7/let-mode),
;;; possible TODO: publish that code to melpa and use it here.

(defun frames-only-mode-revertable-set (&rest args)
  "As set but return a closure to revert the change."
  (let* ((pairs (seq-partition args 2))
         (vars (seq-map #'car pairs))
         (values (seq-map #'cadr pairs)))
    (frames-only-mode--revertable-set-helper vars values)))

(defun frames-only-mode--revertable-set-helper (vars values)
  (let ((initial-values (seq-mapn #'symbol-value vars))
        (revert-done nil))
    (seq-mapn #'set vars values)
    (lambda ()
      (when (not revert-done)
        "Revert the variable values set by revertable-set"
        (seq-mapn
         (lambda (var value initial)
           (when (equal (symbol-value var) value)
             (set var initial)))
         vars values initial-values))
      (setq revert-done t))))

(defvar frames-only-mode--revert-fn #'ignore
  "Storage for function to revert changes to variables made by ‘frames-only-mode’.")

(defvar frames-only-mode--revert-magit-fn #'ignore
  "Storage for function to revert changes to magit configuration made by ‘frames-only-mode’.")



;;; Other helpers

(defun frames-only-mode-advice-use-windows (fun &rest args)
  "Create new emacs windows instead of frames within this function."
  (let ((pop-up-frames nil))
    (apply fun args)))


(defun frames-only-mode-abort-recursive-edit ()
  "Close any sub-windows and abort recursive edit.

This is useful for closing temporary windows created by some commands."
  (interactive)

  ;; Biggest window is probably the "main" one, select it and delete the rest.
  (select-window (get-largest-window 't 'nil 'nil))
  (delete-other-windows)

  (abort-recursive-edit))


(defun frames-only-mode-kill-frame-if-current-buffer-matches ()
  "Kill frames as well when certain buffers are closed.

Only if there is only a single window in the frame, helps stop some
packages spamming frames."
  (when (and (one-window-p)
             (member (buffer-name) frames-only-mode-kill-frame-when-buffer-killed-buffer-list))
    (delete-frame)))


(defun frames-only-mode-advice-use-windows-for-completion (orig-fun &rest args)
  "Advise a completion function to not use frames."
  (let ((pop-up-frames (not frames-only-mode-use-windows-for-completion))
        (split-width-threshold 9999))
    (apply orig-fun args)))


(defun frames-only-mode-advice-delete-frame-on-bury (orig-fun &rest args)
  "Delete the frame when burying certain buffers.

Only if there are no other windows in the frame, and if the buffer is in frames-only-mode-kill-frame-when-buffer-killed-buffer-list."
  ;; Store the buffer name now because we can't get it after burying the buffer
  (let ((buffer-to-bury (buffer-name)))
    (apply orig-fun args)
    (when (and (one-window-p)
               (member buffer-to-bury
                       frames-only-mode-kill-frame-when-buffer-killed-buffer-list))
      (delete-frame))))

(defun frames-only-mode-bury-completions ()
  (when (get-buffer "*Completions*")
    (bury-buffer "*Completions*")))




(defvar frames-only-mode-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-]") #'frames-only-mode-abort-recursive-edit)
    map)
  "Keymap for ‘frames-only-mode’.")

(define-minor-mode frames-only-mode
  "Use frames instead of emacs windows."
  :global t
  :keymap frames-only-mode-mode-map

  (if frames-only-mode
      (setq frames-only-mode--revert-fn
            (frames-only-mode-revertable-set
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

             ;; TODO: figure out why this didn't seem to work
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


  ;; Hacks to make other things play nice by killing the frame when certain
  ;; buffers are closed.
  (if frames-only-mode
      (progn
        (add-hook 'kill-buffer-hook #'frames-only-mode-kill-frame-if-current-buffer-matches)
        (advice-add #'bury-buffer :around #'frames-only-mode-advice-delete-frame-on-bury))
    (remove-hook 'kill-buffer-hook #'frames-only-mode-kill-frame-if-current-buffer-matches)
    (advice-remove #'bury-buffer #'frames-only-mode-advice-delete-frame-on-bury))


  ;; TODO: figure out how to revert these changes when the mode is disabled.
  ;; What if the use loads magit after enabling this mode? :(
  (if frames-only-mode
      (when (require 'magit-commit nil 'noerror)
        (setq frames-only-mode--revert-magit-fn
              (frames-only-mode-revertable-set
               ;; Don't auto popup a magit diff buffer when commiting, can still
               ;; get it if needed with C-c C-d. The variable name for this
               ;; changed in magit version 2.30 (~November 2015) so we are not
               ;; compatible with older versions.
               'magit-commit-show-diff nil)))
    (funcall frames-only-mode--revert-magit-fn))


  ;; Advise completion popup functions to use windows instead of frames if
  ;; the custom setting is true.
  (if frames-only-mode
      (progn
        (advice-add #'minibuffer-completion-help :around #'frames-only-mode-advice-use-windows-for-completion)
        (advice-add #'ido-completion-help :around #'frames-only-mode-advice-use-windows-for-completion))
    (advice-remove #'minibuffer-completion-help #'frames-only-mode-advice-use-windows-for-completion)
    (advice-remove #'ido-completion-help #'frames-only-mode-advice-use-windows-for-completion))

  ;; Make sure completions buffer is buried after we are done with the minibuffer
  (if frames-only-mode
      (add-hook 'minibuffer-exit-hook #'frames-only-mode-bury-completions)
    (remove-hook 'minibuffer-exit-hook #'frames-only-mode-bury-completions)))



(provide 'frames-only-mode)

;;; frames-only-mode.el ends here
