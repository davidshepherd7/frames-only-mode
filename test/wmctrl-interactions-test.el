
(ert-deftest current-desktop ()
  (with-mock
    (mock (frames-only-mode--call-process "wmctrl" "-d") => "
0  - DG: N/A  VP: N/A  WA: N/A               1
1  * DG: N/A  VP: N/A  WA: 1920,0 1920x1200  2
2  - DG: N/A  VP: N/A  WA: N/A               3
3  - DG: N/A  VP: N/A  WA: N/A               dump
")
    (should (equal (frames-only-mode--x-current-desktop) "1"))))


(ert-deftest visible-windows ()
  (with-mock
    (mock (frames-only-mode--x-current-desktop) => "1")
    (mock (frames-only-mode--call-process "wmctrl" "-l") => "
0x0200013c  1 cantor Extended Window Manager Hints - Mozilla Firefox
0x01c08469  1 cantor frames-only-mode.el
0x01c0867d  1 cantor *compilation*
0x01c06151  1 cantor wmctrl-interactions-test.el
0x01c0548c  1 cantor *scratch*
0x04000009  1 cantor x-terminal-emulator
0x0200001e  3 cantor Nowena - Amenra - Google Play Music - Mozilla Firefox
0x01c05f98  3 cantor main.c
")
    (should (equal (frames-only-mode--x-visible-window-names)
                   '("Extended Window Manager Hints - Mozilla Firefox"
                     "frames-only-mode.el"
                     "*compilation*"
                     "wmctrl-interactions-test.el"
                     "*scratch*"
                     "x-terminal-emulator"
                     )))
    ))


(ert-deftest compilation-window-display ()
  (with-frames-only-mode

   ;; This is not running graphically, so we need to override the usual
   ;; frames-only-mode setting of 'graphic-only and set pop-up-frames to true.
   (let ((pop-up-frames t)
         (frames-only-mode-reopen-frames-from-hidden-x11-virtual-desktops t))

     (get-buffer-create "*compilation*")

     ;; When the compilation buffer is visible
     (with-mock
       (mock (frames-only-mode--x-buffer-window-visible "*compilation*") => t)
       (mock (make-frame *))
       (display-buffer "*compilation*"))

     ;; When the compilation buffer is not currently visible
     (with-mock
       (mock (frames-only-mode--x-buffer-window-visible "*compilation*") => nil)
       (mock (make-frame *))
       (display-buffer "*compilation*")))))


(defun fom/display-buffer-alist-contains-settings ()
  (--find (equal (car it) #'frames-only-mode--should-force-new-frame)
          display-buffer-alist))

(ert-deftest settings-reverted-for-compilation-window-display ()
  (should (not (fom/display-buffer-alist-contains-settings)))

  (with-frames-only-mode
   (should (fom/display-buffer-alist-contains-settings)))

  (should (not (fom/display-buffer-alist-contains-settings))))


(ert-deftest error-running-wmctrl ()
  (cl-letf ((frames-only-mode-reopen-frames-from-hidden-x11-virtual-desktops t)
            ((symbol-function 'frames-only-mode--x-buffer-window-visible)
             (lambda (_) (error "foo"))))

    (should-not (frames-only-mode--should-force-new-frame "buff" nil))))
