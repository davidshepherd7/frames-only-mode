
;; Collection of settings and code to use frames instead of emacs
;; "windows".



;; To automatically open a "useful" buffer in new frames xmonads binding
;; for a new frame is set to "emacsclient -c -n -e '(switch-to-buffer
;; nil)'". For other window managers something similar should work...




;; Options:

(defvar kill-frame-when-buffer-killed-buffer-list
  '("*RefTeX Select*" "*Help*" "*Popup Help*" "*Completions*")
  "Buffer names for which the containing frame should be
 killed when the buffer is killed.")


(defcustom frames-only-mode-use-windows-for-completion t
  "Use emacs windows for display of completions.

This is useful because a new completion frame would steal
window manager focus.

Completion windows are always split horizontally (helm style).

To disable completion popups entirely use the variable
`completion-auto-help' for default emacs completion or
`ido-completion-buffer' for ido-based completion. ")



;; Code

;; Make new frames instead of new windows
(set 'pop-up-frames 'graphic-only)

;; Focus follows mouse (for emacs windows) off to prevent crazy things
;; happening when I click on e.g. compilation error links. Would do nothing
;; interesting anyway if everything is working because there are no windows
;; within frames.
(set 'mouse-autoselect-window nil)
(set 'focus-follows-mouse nil)

;; gdb (gud) does some stupid things with windows, this stops some of it:
(set 'gdb-use-separate-io-buffer nil)
(set 'gdb-many-windows nil)

;; org windows: Use frames not emacs windows
(set 'org-agenda-window-setup 'other-frame)
(set 'org-src-window-setup 'other-frame)

(when (require 'magit nil 'noerror)
  ;; Use the current frame/window to enter the magit commit message
  (set 'magit-server-window-for-commit nil)

  ;; Don't auto popup a magit diff buffer when commiting, can still get it
  ;; if needed with C-c C-d.
  (set 'magit-commit-show-diff nil))



;; Make calendar do something more sensible with its window/frame layout.
(defadvice calendar (around disable-pop-up-frames activate)
  "Disable pop-up-frames while this is going on, otherwise we get
extra useless frames."
  (let ((pop-up-frames 'nil))
    ad-do-it))

;; Key bind to close sub-windows (e.g. as created by re-builder or
;; calendar) with the same key as abort-recursive-edit.
(defun super-abort-recursive-edit ()
  "kill any sub-windows and abort recursive edit."
  (interactive)

  ;; Biggest window is probably the "main" one, select it and delete the
  ;; rest.
  (select-window (get-largest-window 't 'nil 'nil))
  (delete-other-windows)

  (abort-recursive-edit))
(global-set-key [remap abort-recursive-edit] #'super-abort-recursive-edit)



;; kill frames when a buffer is buried, makes most things play nice with
;; frames
(set 'frame-auto-hide-function 'delete-frame)

;; Hacks to make other things play nice by killing the frame when certain
;; buffers are closed.
(defun kill-frame-if-current-buffer-matches ()
  "Kill frames as well when certain buffers are closed (but only
  if there is only a single window in the frame), helps stop some
  packages spamming frames."
  (when (and (one-window-p)
             (member (buffer-name) kill-frame-when-buffer-killed-buffer-list))
    (delete-frame)))

(add-hook 'kill-buffer-hook 'kill-frame-if-current-buffer-matches)

(defadvice bury-buffer (around kill-frame-if-current-buffer-matches activate)
  "Kill the frame when burying certain buffers (but only if there
  is only a single window in the frame)."
  (let ((buffer-to-bury (buffer-name)))
    ad-do-it
    (when (and (one-window-p)
               (member buffer-to-bury kill-frame-when-buffer-killed-buffer-list))
      (delete-frame))))

;; Advise completion popup functions to use windows instead of frames if
;; the custom setting is true.
(defun advice-use-windows-for-completion (orig-fun &rest args)
  (let ((pop-up-frames (not frames-only-mode-use-windows-for-completion))
        (split-width-threshold 9999))
    (apply orig-fun args)))
(advice-add 'minibuffer-completion-help :around #'advice-use-windows-for-completion)
(advice-add 'ido-completion-help :around #'advice-use-windows-for-completion)

;; Make sure completions buffer is buried after we are done with the minibuffer
(add-hook 'minibuffer-exit-hook (lambda () (when (get-buffer "*Completions*")
                                        (bury-buffer "*Completions*"))))


(provide 'frames-only-mode)
