(ert-deftest variables-set-when-mode-runs ()

  (should-not (equal pop-up-frames 'graphic-only))

  (frames-only-mode t)

  (should (equal pop-up-frames 'graphic-only)))
