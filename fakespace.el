;;; fakespace.el --- fake Emacs lisp namespaces

;; This is free and unencumbered software released into the public domain.

;;; Commentary:

;; See example.el for an example of using this package.

;; Provides fake namespaces through a very rudimentary `defpackage',
;; familiar to Common Lisp users. The developer declares which symbols
;; are to be exported outside the package. All other symbols are
;; hidden away (by `end-package') where no other package can trample
;; on them.

;; It works by comparing the symbol table before any declarations to
;; the symbol table afterward, then uninterning any symbols that were
;; created and not exported. This will work even on byte-compiled
;; files. In fact, everything is determined at compile-time, so there
;; is practically no load-time penalty from using these fake
;; namespaces.

;; Generally you will not want to actively use these fake namespaces
;; during development because there is no `in-package' function. You
;; will always been in the main namespace unable to access your
;; private functions. Later, when you're finishing up and want to test
;; out your namespace, make sure your code is unloaded from Emacs
;; (i.e. none of your symbols is in the symbol table) before applying
;; your namespace. The same applies to compilation: the package must
;; not be loaded before compilation or the `defpackage' will not hide
;; any symbols.

;; I still think it is possible to support `in-package' here. When
;; symbols are uninterned, they could also be stored in a package
;; symbol table. When switching packages, the old symbol table's
;; symbols are uninterned and the new symbols are loaded in. I may add
;; this in the future.

;;; Code:

(require 'cl)

;; Dummy call to force autoload of cl-seq.
(remove-if-not 'identity ())

(defun atom-list (&optional ob)
  "Return given obarray OB as a list. Defaults to obarray."
  (let ((lst ()))
    (mapatoms (lambda (s) (push s lst)) ob)
    lst))

(defun atom-difference (a b)
  "Like set-difference, but, for performance reasons, requires
specially formed lists (i.e. from `atom-list'). Returns items
that are in B and not A."
  (let ((diff))
    (while (and (not (null a)) (not (null b)))
      (while (not (eq (car a) (car b)))
	(push (car b) diff)
	(setq b (cdr b)))
      (setq a (cdr a))
      (setq b (cdr b)))
    diff))

(lexical-let ((old-obarray ()))
  (defmacro defpackage (name &rest args)
    (dolist (arg args)
      (let ((type (car arg)))
	(cond ((eq type :exports) t) ; interning the symbols is enough
	      ((eq type :use) (mapcar (lambda (s) (require s)) (cdr arg))))))
    (setq old-obarray (atom-list))
    `(provide (quote ,name)))

  (defmacro end-package ()
    (cons 'progn
	  (mapcar (lambda (s) `(unintern (quote ,s) nil))
		  (atom-difference old-obarray (atom-list))))))

(provide 'fakespace)
