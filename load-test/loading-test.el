
;; Test that frames-only-mode can be loaded and enabled without requiring any
;; other features.

(require 'ert)

;; Don't accidentally load old .elc files
(setq load-prefer-newer t)

;; This fixes an issue in emacs 25.1 where the debugger would be invoked
;; incorrectly, breaking ert.
(when (and (= emacs-major-version 25) (< emacs-minor-version 2))
  (require 'cl-preloaded)
  (setf (symbol-function 'cl--assertion-failed)
        (lambda (form &optional string sargs args)
          "This function has been modified by espuds to remove an incorrect manual call
to the debugger in emacs 25.1. The modified version should only be used for
running the espuds tests."
          (if string
              (apply #'error string (append sargs args))
            (signal 'cl-assertion-failed `(,form ,@sargs))))))


(defvar test-features
  (list 'org-agenda 'ido 'magit-commit 'flycheck 'ediff-wind)
  "Features used by frames-only-mode")

(ert-deftest toggle-mode-without-other-features ()

  (should-not (featurep 'frames-only-mode))

  ;; For these tests we need to make sure we don't have these things loaded initially
  (dolist (elt test-features)
    (should-not (featurep elt)))

  (require 'frames-only-mode (concat default-directory "frames-only-mode.el"))
  (frames-only-mode t)
  (should frames-only-mode)
  ;; Appropriate vars should have been set anyway
  (should (equal flycheck-display-errors-function 'frames-only-mode-flycheck-display-errors))
  (frames-only-mode 0)

  ;; And we still should not have loaded them
  (dolist (elt test-features)
    (should-not (featurep elt))))

(ert-run-tests-batch-and-exit)
