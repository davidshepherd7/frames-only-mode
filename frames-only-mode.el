;;; frames-only-mode.el --- Use frames instead of Emacs windows -*- lexical-binding: t; -*-

;; Copyright (C) 2015 Free Software Foundation, Inc.

;; Author: David Shepherd <davidshepherd7@gmail.com>
;; Version: 1.0.0
;; Package-Requires: ((emacs "26.3") (dash "2.13.0") (s "1.11.0"))
;; Keywords: frames, windows
;; URL: https://github.com/davidshepherd7/frames-only-mode


;;; Commentary:

;;; Code:

(require 'dash)
(require 's)



;;; Options:

(defgroup frames-only '()
  "Use frames instead of emacs windows."
  :group 'frames)

(defcustom frames-only-mode-kill-frame-when-buffer-killed-buffer-list
  '("*RefTeX Select*" "*Help*" "*Popup Help*" "*Completions*")
  "Buffers for which the containing frame should be killed when the
buffer is killed.

Each entry in the list is either a string representing a buffer
name or a pair like `(regexp . \"\\\\*foo.*\\\\.bz\\\\*\")' representing a regexp
which matches buffer names.
"
  :type '(repeat (choice
                  string
                  (cons (const regexp) string)))
  :group 'frames-only)


(defcustom frames-only-mode-use-windows-for-completion t
  "Use Emacs windows for display of completions.

This is useful because a new completion frame would steal
window manager focus.

Completion windows are always split horizontally (helm style).

To disable completion popups entirely use the variable
`completion-auto-help' for default Emacs completion or
`ido-completion-buffer' for ido-based completion."
  :type 'boolean
  :group 'frames-only)

(defcustom frames-only-mode-use-window-functions
  (list #'calendar #'report-emacs-bug 'checkdoc-show-diagnostics 'checkdoc 'org-compile-file)
  "List of functions inside which new emacs windows should be
created instead of frames.

\(i.e. pop-up-frames is let bound to nil, the default value)."
  ;; Type is symbol, not function, because we sometimes want to add functions to
  ;; this list without requiring the library.
  :type '(repeat symbol)

  :group 'frames-only)


(defcustom frames-only-mode-configuration-variables
  (list
   ;; Make new frames instead of new windows, the main setting
   (list 'pop-up-frames 'graphic-only)

   ;; Focus follows mouse (for emacs windows) off to prevent crazy things
   ;; happening when I click on e.g. compilation error links. Would do nothing
   ;; interesting anyway if everything is working because there are no windows
   ;; within frames.
   (list 'mouse-autoselect-window nil)
   (list 'focus-follows-mouse nil)

   ;; kill frames when a buffer is buried, makes most things play nice with
   ;; frames
   (list 'frame-auto-hide-function 'delete-frame)

   ;; org windows: Use frames not emacs windows
   (list 'org-agenda-window-setup 'other-frame)
   (list 'org-src-window-setup 'other-frame)

   ;; Use a single frame for ediff (without this you end up with an entire
   ;; frame for the control buffer, this doesn't work well at all with tiling
   ;; window managers).
   (list 'ediff-window-setup-function 'ediff-setup-windows-plain)

   ;; When using ido-switch-buffer: open buffers in the current frame
   ;; rather than raising any existing frame (the default behaviour is
   ;; to only do this with emacs windows but switch to existing
   ;; frames).
   (list 'ido-default-buffer-method 'selected-window)

   ;; Don't auto popup a magit diff buffer when commiting, can still
   ;; get it if needed with C-c C-d. The variable name for this
   ;; changed in magit version 2.30 (~November 2015) so we are not
   ;; compatible with older versions.
   (list 'magit-commit-show-diff nil)

   ;; We've configured magit to always open a new frame, so we need to delete
   ;; the frame on quitting otherwise each use of magit-status leaves an
   ;; additional frame lying around.
   (list 'magit-bury-buffer-function #'frames-only-mode-magit-bury-buffer-function)

   ;; Don't pop an errors buffer, it's really annoying, instead
   ;; format a message in the minibuffer
   (list 'flycheck-display-errors-function #'frames-only-mode-flycheck-display-errors))
  "List of configuration variables set by frames-only-mode.

Each entry should be of the form `(list variable-symbol value)'.

If you find any settings that you think will be useful to others using this
mode please open an issue at https://github.com/davidshepherd7/frames-only-mode/issues
to let me know."
  :type '(repeat (list symbol sexp))
  :group 'frames-only)



(defun frames-only-mode-revertable-set (var-vals)
  "Like set but return a closure to revert the change.

In accordance with
https://www.gnu.org/software/emacs/manual/html_node/elisp/Hooks-for-Loading.html
we even set variables that are not currently bound, but we unbind
them again on revert."
  ;; Transform to list of (var value initial-value) and call helper function. I
  ;; wish we had a back-portable thread-last macro...
  (let ((var-val-initials (-map (lambda (s) (append s
                                               (list (when (boundp (car s))
                                                       (symbol-value (car s)))
                                                     (boundp (car s)))))
                                var-vals)))
    (frames-only-mode--revertable-set-helper var-val-initials)))

(defun frames-only-mode--revertable-set-helper (var-value-initials)
  "Internal function."
  (let ((revert-done nil)
        (revert-var-fn
         (lambda (s)
           (when (equal (symbol-value (car s)) (cadr s))
             ;; core emacs doesn't have caddr, why? :(
             (if (cadr (cdr (cdr s)))
                 (set (car s) (cadr (cdr s)))
               (makunbound (car s)))))))

    ;; Set each var
    (-map (lambda (s) (set (car s) (cadr s))) var-value-initials)

    ;; Return a function to revert the changes
    (lambda ()
      "Revert the variable values set by revertable-set"
      (when (not revert-done)
        (-map revert-var-fn var-value-initials)
        (setq revert-done t)))))

(defvar frames-only-mode--revert-fn #'ignore
  "Storage for function to revert changes to variables made by ‘frames-only-mode’.")



;;; Other helpers

(defun frames-only-mode-advice-use-windows (fun &rest args)
  "Create new emacs windows instead of frames within this FUN."
  (let ((pop-up-frames nil))
    (apply fun args)))


(defun frames-only-mode-abort-recursive-edit ()
  "Close any sub-windows and abort recursive edit.

This is useful for closing temporary windows created by some commands."
  (interactive)

  ;; Biggest window is probably the "main" one, select it and delete the rest.
  (select-window (get-largest-window))
  (delete-other-windows)

  (abort-recursive-edit))


(defun frames-only-should-kill-frame-for-buffer (b-name)
  (--any?
   (cond
    ((stringp it) (equal b-name it))
    ((and (consp it) (eq (car it) 'regexp)) (s-matches-p (cdr it) b-name))
    (t nil))
   frames-only-mode-kill-frame-when-buffer-killed-buffer-list))


(defun frames-only-mode-kill-frame-if-current-buffer-matches ()
  "Kill frames as well when certain buffers are closed.

Only if there is only a single window in the frame, helps stop some
packages spamming frames."
  (when (and (one-window-p)
             (frames-only-should-kill-frame-for-buffer (buffer-name)))
    (delete-frame)))


(defun frames-only-mode-advice-use-windows-for-completion (orig-fun &rest args)
  "Advise a completion function to not use frames."
  (let ((pop-up-frames (not frames-only-mode-use-windows-for-completion))
        (split-width-threshold 9999))
    (apply orig-fun args)))


(defun frames-only-mode-advice-delete-frame-on-bury (orig-fun &rest args)
  "Delete the frame when burying certain buffers.

Only if there are no other windows in the frame, and if the
buffer is in frames-only-mode-kill-frame-when-buffer-killed-buffer-list."
  ;; Store the buffer name now because we can't get it after burying the buffer
  (let ((buffer-to-bury (buffer-name)))
    (apply orig-fun args)
    (when (and (one-window-p)
               (frames-only-should-kill-frame-for-buffer buffer-to-bury))
      (delete-frame))))

(defun frames-only-mode-bury-completions ()
  (when (get-buffer "*Completions*")
    (bury-buffer "*Completions*")))

(defun frames-only-mode-flycheck-display-errors (errors)
  (message "%s" (mapcar 'flycheck-error-format-message-and-id errors)))

;; Quiet the compiler: will always be loaded by the time this function is used.
(declare-function magit-restore-window-configuration "magit-mode")

(defun frames-only-mode-magit-bury-buffer-function (kill-buffer)
  "By default magit leaves open frames after quitting magit-status
buffers in some cases.

This modification makes it always kill the frame after quitting a
magit status buffer."
  (let ((current-buffer-is-magit-status (derived-mode-p 'magit-status-mode))
        (current-frame (selected-frame)))
    (magit-restore-window-configuration kill-buffer)
    (when (and current-buffer-is-magit-status
               (equal (selected-frame) current-frame))
      (delete-frame))))



(defvar frames-only-mode-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-]") #'frames-only-mode-abort-recursive-edit)
    map)
  "Keymap for ‘frames-only-mode’.")

;;;###autoload
(define-minor-mode frames-only-mode
  "Use frames instead of emacs windows."
  :global t
  :group 'frames-only
  :keymap frames-only-mode-mode-map

  ;; Apply most of the configuration changes
  (if frames-only-mode
      (setq frames-only-mode--revert-fn (frames-only-mode-revertable-set frames-only-mode-configuration-variables))
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

  ;; Advise completion popup functions to use windows instead of frames if
  ;; the custom setting is true.
  (if frames-only-mode
      (progn
        (advice-add #'minibuffer-completion-help :around #'frames-only-mode-advice-use-windows-for-completion)
        (advice-add 'ido-completion-help :around #'frames-only-mode-advice-use-windows-for-completion)
        (advice-add 'pcomplete :around #'frames-only-mode-advice-use-windows-for-completion))
    (advice-remove #'minibuffer-completion-help #'frames-only-mode-advice-use-windows-for-completion)
    (advice-remove 'ido-completion-help #'frames-only-mode-advice-use-windows-for-completion)
    (advice-remove 'pcomplete #'frames-only-mode-advice-use-windows-for-completion))

  ;; Make sure completions buffer is buried after we are done with the minibuffer
  (if frames-only-mode
      (add-hook 'minibuffer-exit-hook #'frames-only-mode-bury-completions)
    (remove-hook 'minibuffer-exit-hook #'frames-only-mode-bury-completions)))



(provide 'frames-only-mode)

;;; frames-only-mode.el ends here
