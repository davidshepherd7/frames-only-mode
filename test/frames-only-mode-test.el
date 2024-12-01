;; These tests are a bit of a hack. Really each one should run in it's own
;; instance of emacs to prevent them affecting each other, but that seems
;; hard/impossible.

;; Don't run these tests from within your own emacs: they may not rollback all
;; changes correctly (since this is what they are testing!).

(require 'frames-only-mode)
(require 'validate)

(defmacro with-frames-only-mode (&rest body)
  `(unwind-protect
       (progn
         (frames-only-mode 1)
         ,@body)
     (frames-only-mode 0)))


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


(ert-deftest delete-frame-on-kill-or-bury ()
  (fom-rollback-test
   (seq-contains kill-buffer-hook #'frames-only-mode-kill-frame-if-current-buffer-matches))
  (fom-rollback-test
   (advice-member-p #'frames-only-mode-advice-delete-frame-on-bury #'bury-buffer)))

(ert-deftest completion-popups ()
  (fom-rollback-test
   (advice-member-p #'frames-only-mode-advice-use-windows-for-completion #'minibuffer-completion-help))
  (fom-rollback-test
   (advice-member-p #'frames-only-mode-advice-use-windows-for-completion #'ido-completion-help)))

(ert-deftest bury-completions-hook ()
  (fom-rollback-test
   (seq-contains minibuffer-exit-hook #'frames-only-mode-bury-completions)))


(require 'magit-commit)
(ert-deftest magit-settings ()
  (fom-rollback-test
   (equal magit-commit-show-diff nil)))


(require 'flycheck)
(ert-deftest flycheck-settings ()
  (fom-rollback-test
   (equal flycheck-display-errors-function #'frames-only-mode-flycheck-display-errors)))

(ert-deftest flycheck-displays-errors-in-minibuffer ()
  (with-frames-only-mode
   (flycheck-display-errors (list (flycheck-error-new
                                   :buffer "buff"
                                   :checker "check"
                                   :filename "file"
                                   :line 1
                                   :message "Phasellus neque orci, porta a, aliquet quis, semper a, massa.  Cum sociis natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus.  Nullam rutrum.  Phasellus purus.  In id erat non orci commodo lobortis.  "
                                   :level 'error)
                                  (flycheck-error-new
                                   :buffer "buff"
                                   :checker "check"
                                   :filename "file"
                                   :line 1
                                   :message "Sed id ligula quis est convallis tempor."
                                   :level 'error)
                                  ))
   (should (equal (length (window-list)) 1))))



(ert-deftest custom-variable-types ()
  (validate-variable 'frames-only-mode-kill-frame-when-buffer-killed-buffer-list)
  (validate-variable 'frames-only-mode-use-windows-for-completion)
  (validate-variable 'frames-only-mode-use-window-functions)
  (validate-variable 'frames-only-mode-configuration-variables)
  )

(ert-deftest test-validate-setq-kill-frame-for-buffer ()
  (validate-setq frames-only-mode-kill-frame-when-buffer-killed-buffer-list
                 '("foo" (regexp . "foo.*"))))

(ert-deftest test-should-kill-frame-for-buffer-empty ()
  (let ((frames-only-mode-kill-frame-when-buffer-killed-buffer-list '()))
    (should-not (frames-only-should-kill-frame-for-buffer "foo"))))

(ert-deftest test-should-kill-frame-for-buffer-static-strings ()
  (let ((frames-only-mode-kill-frame-when-buffer-killed-buffer-list '("foo" "baz")))
    (should (frames-only-should-kill-frame-for-buffer "foo"))
    (should-not (frames-only-should-kill-frame-for-buffer "foolish"))
    (should (frames-only-should-kill-frame-for-buffer "baz"))
    (should-not (frames-only-should-kill-frame-for-buffer "bar"))))

(ert-deftest test-should-kill-frame-for-buffer-regexp ()
  (let ((frames-only-mode-kill-frame-when-buffer-killed-buffer-list '((regexp . "foo.*"))))
    (should (frames-only-should-kill-frame-for-buffer "foo"))
    (should (frames-only-should-kill-frame-for-buffer "foolish"))
    (should-not (frames-only-should-kill-frame-for-buffer "bar"))))


(ert-deftest test-window-keybinding-overrides ()
  ;; By default we don't do any rebinding
  (with-frames-only-mode
   (should (equal (key-binding (kbd "C-x 2")) #'split-window-below))
   (should (equal (key-binding (kbd "C-x 4 b")) #'switch-to-buffer-other-window))
   )

  (frames-only-mode-remap-common-window-split-keybindings)
  (with-frames-only-mode
   (should (equal (key-binding (kbd "C-x 2")) #'make-frame-command))
   (should (equal (key-binding (kbd "C-x 4 b")) #'switch-to-buffer-other-frame))
   )
  )
