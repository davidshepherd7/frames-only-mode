
;; Collection of settings and code to use frames instead of emacs
;; "windows".



;; To automatically open a "useful" buffer in new frames xmonads binding
;; for a new frame is set to "emacsclient -c -n -e '(switch-to-buffer
;; nil)'". For other window managers something similar should work...




;; Options:

(defvar kill-frame-when-buffer-killed-buffer-list
  '("*RefTeX Select*" "*Help*" "*Popup Help*")
  "Buffer names for which the containing frame should be
 killed when the buffer is killed.")



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
  (set 'magit-server-window-for-commit nil))

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

;; Hack to make other things play nice by killing the frame when certain
;; buffers are closed.
(defun kill-frame-if-current-buffer-matches ()
  "Kill frames as well when certain buffers are closed, helps stop some
  packages spamming frames."
  (interactive)
  (if (member (buffer-name) kill-frame-when-buffer-killed-buffer-list)
      (delete-frame)))
(add-hook 'kill-buffer-hook 'kill-frame-if-current-buffer-matches)


(provide 'frames-only-mode)
