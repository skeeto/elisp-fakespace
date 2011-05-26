;;; fakespace.el --- fake Emacs lisp namespaces


;;; Code:

(require 'cl)

;; Dummy call to force autoload.
(remove-if-not 'identity ())

(defun atom-list (&optional ob)
  "Return given obarray OB as a list. Defaults to obarray."
  (let ((lst ()))
    (mapatoms (lambda (s) (push s lst)) ob)
    lst))

(defun atom-difference (a b)
  "Like set-difference, but, for performance reaons, requires
specially formed lists. Returns items that are in B and not A."
  (let ((diff))
    (while (and (not (null a)) (not (null b)))
      (while (not (eq (car a) (car b)))
	(push (car b) diff)
	(setq b (cdr b)))
      (setq a (cdr a))
      (setq b (cdr b)))
    diff))

(defvar old-obarray ()
  "List of all the items from obarray at some previous time.")

(defmacro defpackage (name &rest args)
  (dolist (arg args)
    (let ((type (car arg)))
      (cond ((eq type :exports) t)   ; interning the symbols is enough
	    ((eq type :use) (mapcar (lambda (s) (require s)) (cdr arg))))))
  (setq old-obarray (atom-list))
  `(provide (quote ,name)))

(defmacro end-package ()
  (cons 'progn
	(mapcar (lambda (s) `(unintern (quote ,s)))
		(atom-difference old-obarray (atom-list)))))

(provide 'fakespace)
