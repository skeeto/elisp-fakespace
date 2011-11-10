Elisp Namespaces
================

This package provides fake namespaces for Emacs Lisp through a
rudimentary version of `defpackage`.

```cl
(defpackage example
  (:use cl ido)
  (:export example-main example-var))
````

Symbols that are not exported are hidden from any access outside the
package. See the header of `fakespace.el` for more information, and
`example.el` for a full example of its use.

See also: http://nullprogram.com/blog/2011/08/18/
