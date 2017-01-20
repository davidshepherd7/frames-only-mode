
;; Test that frames-only-mode can be loaded and enabled without requiring any
;; other features.

(require 'ert)

(require 'f)

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


(ert-deftest toggle-mode-without-other-features ()

  (cl-assert (not (featurep 'frames-only-mode)))

  ;; For these tests we need to make sure we don't have these things loaded initially
  (cl-assert (not (featurep 'ido)))
  (cl-assert (not (featurep 'magit)))
  (cl-assert (not (featurep 'flycheck)))
  (cl-assert (not (featurep 'gdb)))

  (require 'frames-only-mode (f-expand "frames-only-mode.el" (f-parent (f-dirname (f-this-file)))))
  (frames-only-mode t)
  (frames-only-mode 0)

  ;; And we still should not have loaded them
  (cl-assert (not (featurep 'ido)))
  (cl-assert (not (featurep 'magit)))
  (cl-assert (not (featurep 'flycheck)))
  (cl-assert (not (featurep 'gdb))))

(ert-run-tests-batch-and-exit)
