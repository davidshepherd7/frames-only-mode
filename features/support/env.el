(require 'f)

(defvar frames-only-mode-support-path
  (f-dirname load-file-name))

(defvar frames-only-mode-features-path
  (f-parent frames-only-mode-support-path))

(defvar frames-only-mode-root-path
  (f-parent frames-only-mode-features-path))

(add-to-list 'load-path frames-only-mode-root-path)

(require 'frames-only-mode)
(require 'espuds)
(require 'ert)

(Setup
 ;; Before anything has run
 )

(Before
 ;; Before each scenario is run
 )

(After
 ;; After each scenario is run
 )

(Teardown
 ;; After when everything has been run
 )
