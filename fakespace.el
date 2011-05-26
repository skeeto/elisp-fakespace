;;; fakespace.el --- fake Emacs lisp namespaces


;;; Code:

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

(defun package-hide-p (s)
  "Return t if this symbol should be uninterned."
  (or (functionp s)
      (documentation-property s 'variable-documentation)))

(defvar old-obarray ()
  "List of all the items from obarray at some previous time.")

(defmacro defpackage (name &rest args)
  (let ((code (list 'progn)))
    (dolist (arg args)
      (let ((type (car arg)))
	(cond ((eq type :exports) t) ; interning the symbols is enough
	      ((eq type :use)
	       (setq code (append code (mapcar
					(lambda (s)
					  `(require (quote ,s)))
					(cdr arg))))))))
    (setq old-obarray (atom-list))
    (append code (list `(provide (quote ,name))))))

(defmacro end-package ()
  (cons 'progn
	(mapcar (lambda (s) `(unintern (quote ,s)))
		(remove-if-not 'package-hide-p
			       (atom-difference old-obarray (atom-list))))))

(provide 'fakespace)
