(require 'fakespace)

;; Start be declaring a package. `defpackage' currently supports :use
;; and :export. Anything listed in :use will be `require'd. You can
;; make your own calls to require, but they should occur before
;; `defpackage'. Otherwise the functions and variables defined in the
;; require will become part of your package and won't get exported.

(defpackage example
  (:use cl ido)
  (:export example-main example-var))

;; Caveat: any functions or variables you declare *will* be defined in
;; the main namespace (we're faking namespaces here), but the
;; non-exported ones will be removed later. They can be redefined
;; elsewhere without interfering with the definitions here.

(defvar my-var 100
  "A hidden variable.")

(defvar example-var nil
  "A public variable.")

(defun my-func ()
  "A private function."
  my-var)

(defun example-main ()
  "An exported function. Notice we can access all the private
variables and functions from here."
  (interactive)
  (list (list (my-func) my-var) example-var
	(ido-completing-read "New value: " (list "foo" "bar"))))

;; Unlike Common Lisp, rather than declaring your namespace with
;; `in-package' you must end your package definition with
;; `end-package'. This will hide all of your internal functions away
;; from the main namespace.

(end-package)
