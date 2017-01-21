
(ert-deftest revertable-set-set-outside ()
  ""
  (let ((lm/foo 1)
        (lm/revert-fn nil))
    ;; Set
    (setq lm/revert-fn (frames-only-mode-revertable-set
                        '((lm/foo 2))))

    ;; Something else sets the variable
    (setq lm/foo 10)

    ;; Revert should do nothing
    (funcall lm/revert-fn)
    (should (equal lm/foo 10))

    ;; Ignore multiple calls to revert
    (setq lm/foo 2)
    (funcall lm/revert-fn)
    (should (equal lm/foo 2))))


(ert-deftest revertable-set-multiple ()
  ""
  (let ((lm/foo 1)
        (lm/bar "a")
        (lm/revert-fn nil))
    ;; Set
    (setq lm/revert-fn (frames-only-mode-revertable-set
                        '((lm/foo 2)
                          (lm/bar "b"))))
    (should (equal lm/foo 2))
    (should (equal lm/bar "b"))

    ;; Revert
    (funcall lm/revert-fn)
    (should (equal lm/foo 1))
    (should (equal lm/bar "a"))
    ))


(ert-deftest revertable-set-unbound-vars ()
  (let ((lm/foo 1)
        (lm/baz 1)
        (lm/revert-fn nil))

    (setq lm/revert-fn (frames-only-mode-revertable-set
                        '((lm/foo 2)
                          (lm/bar "b")
                          (lm/baz 7))))

    (should (equal lm/foo 2))
    (should-not (boundp 'lm/bar))
    (should (equal lm/baz 7))

    (funcall lm/revert-fn)
    (should (equal lm/foo 1))
    (should-not (boundp 'lm/bar))
    (should (equal lm/baz 1))))


(ert-deftest revertable-set-nil-vars ()
  (let ((lm/foo nil)
        (lm/revert-fn nil))

    (setq lm/revert-fn (frames-only-mode-revertable-set
                        '((lm/foo 2))))

    (should (equal lm/foo 2))
    (funcall lm/revert-fn)
    (should (equal lm/foo nil))))
