(require 'f)

(require 'frames-only-mode (f-expand "frames-only-mode.el" (f-parent (f-dirname (f-this-file)))))

;; For el-mock
(eval-when-compile
  (require 'cl))

(require 'el-mock)

(require 'validate)

(defmacro with-frames-only-mode (&rest body)
  `(unwind-protect
       (progn
         (frames-only-mode 1)
         ,@body)
     (frames-only-mode 0)))
