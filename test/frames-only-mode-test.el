;; These tests are a bit of a hack. Really each one should run in it's own
;; instance of emacs to prevent them affecting each other, but that seems
;; hard/impossible.

;; Don't run these tests from within your own emacs: they may not rollback all
;; changes correctly (since this is what they are testing!).


(ert-deftest variables-set-when-mode-runs ()

  (should-not (equal pop-up-frames 'graphic-only))

  (frames-only-mode)

  (should (equal pop-up-frames 'graphic-only))

  (frames-only-mode 0)

  (should-not (equal pop-up-frames 'graphic-only)))


(ert-deftest advise-functions ()
  (defun fom/foo () pop-up-frames)
  (add-to-list 'frames-only-mode-use-window-functions #'fom/foo)

  (should-not (equal pop-up-frames 'graphic-only))
  (should-not (equal (fom/foo) 'graphic-only))
  (should-not (advice-member-p #'frames-only-mode-advice-use-windows #'fom/foo))

  (frames-only-mode)
  (should (advice-member-p #'frames-only-mode-advice-use-windows #'fom/foo))
  (should-not (equal (fom/foo) 'graphic-only))

  (frames-only-mode 0)
  (should-not (advice-member-p #'frames-only-mode-advice-use-windows #'fom/foo))
  (should-not (equal (fom/foo) 'graphic-only))
  )

(ert-deftest kill-buffer-hook ()

  (should-not (seq-contains kill-buffer-hook #'kill-frame-if-current-buffer-matches))

  (frames-only-mode)
  (should (seq-contains kill-buffer-hook #'kill-frame-if-current-buffer-matches))

  (frames-only-mode 0)
  (should-not (seq-contains kill-buffer-hook #'kill-frame-if-current-buffer-matches))
  )
