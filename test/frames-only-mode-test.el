;; These tests are a bit of a hack. Really each one should run in it's own
;; instance of emacs to prevent them affecting each other, but that seems
;; hard/impossible.

;; Don't run these tests from within your own emacs: they may not rollback all
;; changes correctly (since this is what they are testing!).

(require 'frames-only-mode)


(defmacro fom-rollback-test (expr)
  "Check expr before, during and after toggling frames-only-mode.

Expr should be false, true, false respectively."
  `(progn (should-not ,expr)
          (unwind-protect
              (progn
                (frames-only-mode 1)
                (should ,expr))
            (frames-only-mode 0))
          (should-not ,expr)))


(ert-deftest variables-set-when-mode-runs ()
  (fom-rollback-test (equal pop-up-frames 'graphic-only)))


(ert-deftest advise-functions ()
  (defun fom/foo () pop-up-frames)
  (add-to-list 'frames-only-mode-use-window-functions #'fom/foo)
  (fom-rollback-test (advice-member-p #'frames-only-mode-advice-use-windows #'fom/foo)))


(ert-deftest kill-buffer-hook ()
  (fom-rollback-test
   (seq-contains kill-buffer-hook #'frames-only-mode-kill-frame-if-current-buffer-matches)))
