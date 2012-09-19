;;; anaphora.el --- anaphoric macros providing implicit temp variables
;;
;; This code is in the public domain.
;;
;; Author: Roland Walker <walker@pobox.com>
;; Homepage: http://github.com/rolandwalker/anaphora
;; URL: http://raw.github.com/rolandwalker/anaphora/master/anaphora.el
;; Version: 0.0.4
;; Last-Updated: 14 Sep 2012
;; EmacsWiki: Anaphora
;; Keywords: extensions
;;
;;; Commentary:
;;
;; Quickstart
;;
;;     (require 'anaphora)
;;
;;     (awhen (big-long-calculation)
;;       (foo it)      ; `it' is provided as
;;       (bar it))     ; a temporary variable
;;
;;     ;; anonymous function to compute factorial using `self'
;;     (alambda (x) (if (= x 0) 1 (* x (self (1- x)))))
;;
;; Explanation
;;
;; Anaphoric expressions implicitly create one or more temporary
;; variables which can be referred to during the expression.  This
;; technique can improve clarity in certain cases.  It also enables
;; recursion for anonymous functions.
;;
;; To use anaphora, place the anaphora.el library somewhere
;; Emacs can find it, and add the following to your ~/.emacs file:
;;
;;     (require 'anaphora)
;;
;; The following macros are made available
;;
;;     `aand'
;;     `ablock'
;;     `acase'
;;     `acond'
;;     `aecase'
;;     `aetypecase'
;;     `aif'
;;     `alambda'
;;     `alet'
;;     `aprog1'
;;     `atypecase'
;;     `awhen'
;;     `awhile'
;;     `a+'
;;     `a-'
;;     `a*'
;;     `a/'
;;
;; See Also
;;
;;     M-x customize-group RET anaphora RET
;;     http://en.wikipedia.org/wiki/On_Lisp
;;     http://en.wikipedia.org/wiki/Anaphoric_macro
;;
;; Notes
;;
;; Principally based on examples from the book "On Lisp", by Paul
;; Graham.
;;
;; When this library is loaded, the provided anaphoric forms are
;; registered as keywords in font-lock.  This may be disabled via
;; customize.
;;
;; Compatibility and Requirements
;;
;;     Tested on GNU Emacs versions 23.3 and 24.1
;;
;; Bugs
;;
;; TODO
;;
;;; License
;;
;; This code is in the public domain.  It is provided without
;; any express or implied warranties.
;;
;;; Code:
;;

;;; requires

;; for declare, labels, do, block, case, ecase, typecase, etypecase
(require 'cl)

;;; customizable variables

;;;###autoload
(defgroup anaphora nil
  "Anaphoric macros providing implicit temp variables"
  :version "0.0.3"
  :link '(emacs-commentary-link "anaphora")
  :prefix "anaphora-"
  :group 'extensions)

(defcustom anaphora-add-font-lock-keywords t
  "Add anaphora macros to font-lock keywords when editing Emacs Lisp."
  :type 'boolean
  :group 'anaphora)

;;; font-lock

(when anaphora-add-font-lock-keywords
  (eval-after-load "lisp-mode"
    '(progn
       (let ((new-keywords '(
                             "aif"
                             "aprog1"
                             "awhen"
                             "awhile"
                             "aand"
                             "acond"
                             "alambda"
                             "ablock"
                             "acase"
                             "aecase"
                             "atypecase"
                             "aetypecase"
                             "alet"
                             )))
         (font-lock-add-keywords 'emacs-lisp-mode `((,(concat "(\\s-*" (regexp-opt new-keywords 'paren) "\\>")
                                                     1 font-lock-keyword-face)) 'append))
       (dolist (buf (buffer-list))
         (with-current-buffer buf
           (when (and (eq major-mode 'emacs-lisp-mode)
                      (boundp 'font-lock-mode)
                      font-lock-mode)
             (font-lock-refresh-defaults)))))))

;;; macros

;;;###autoload
(defmacro aif (cond then &rest else)
  "Like `if', except that the value of COND is bound to `it'.

The variable `it' is available within THEN and ELSE.

COND, THEN, and ELSE are otherwise as documented for `if'."
  (declare (debug (sexp form &rest form))
           (indent 2))
  `(let ((it ,cond))
     (if it ,then ,@else)))

;;;###autoload
(defmacro aprog1 (first &rest body)
  "Like `prog1', except that the value of FIRST is bound to `it'.

The variable `it' is available within BODY.

FIRST and BODY are otherwise as documented for `prog1'."
  (declare (debug (sexp &rest form))
           (indent 1))
  `(let ((it ,first))
     (progn ,@body)
     it))

;;;###autoload
(defmacro awhen (cond &rest body)
  "Like `when', except that the value of COND is bound to `it'.

The variable `it' is available within BODY.

COND and BODY are otherwise as documented for `when'."
  (declare (debug (sexp &rest form))
           (indent 1))
  `(aif ,cond
       (progn ,@body)))

;;;###autoload
(defmacro awhile (test &rest body)
  "Like `while', except that the value of TEST is bound to `it'.

The variable `it' is available within BODY.

TEST and BODY are otherwise as documented for `while'."
  (declare (debug (sexp &rest form)
                  (indent 1)))
  `(do ((it ,test ,test))
       ((not it))
     ,@body))

;;;###autoload
(defmacro aand (&rest conditions)
  "Like `and', except that the value of the previous condition is bound to `it'.

The variable `it' is available within all CONDITIONS after the
initial one.

CONDITIONS are otherwise as documented for `and'.

Note that some implementations of `aand' bind only the first
condition to `it', rather than each successive condition."
  (cond
    ((null conditions)
     t)
    ((null (cdr conditions))
     (car conditions))
    (t
     `(aif ,(car conditions) (aand ,@(cdr conditions))))))

;;;###autoload
(defmacro acond (&rest clauses)
  "Like `cond', except that the value of each condition is bound to `it'.

The variable `it' is available within the remainder of each of CLAUSES.

CLAUSES are otherwise as documented for `cond'."
  (declare (indent 0))
  (if (null clauses)
      nil
    (let ((cl1 (car clauses))
          (sym (gensym)))
      `(let ((,sym ,(car cl1)))
         (if ,sym
             (if (null ',(cdr cl1))
                 ,sym
               (let ((it ,sym)) ,@(cdr cl1)))
           (acond ,@(cdr clauses)))))))

;;;###autoload
(defmacro alambda (args &rest body)
  "Like `lambda', except that the function may refer to itself as `self'.

ARGS and BODY are otherwise as documented for `lambda'."
  (declare (debug (sexp &rest form))
           (indent defun))
  `(labels ((self ,args ,@body))
     #'self))

;;;###autoload
(defmacro ablock (name &rest body)
  "Like `block', except that the value of the previous expression is bound to `it'.

The variable `it' is available within all expressions of BODY
except the initial one.

NAME and BODY are otherwise as documented for `block'."
  (declare (debug (sexp &rest form))
           (indent 1))
  `(block ,name
     ,(funcall (alambda (body)
                        (case (length body)
                          (0 nil)
                          (1 (car body))
                          (t `(let ((it ,(car body)))
                                ,(self (cdr body))))))
               body)))

;;;###autoload
(defmacro acase (expr &rest clauses)
  "Like `case', except that the value of EXPR is bound to `it'.

The variable `it' is available within CLAUSES.

EXPR and CLAUSES are otherwise as documented for `case'."
  (declare (debug (sexp &rest form))
           (indent 1))
  `(let ((it ,expr))
     (case it ,@clauses)))

;;;###autoload
(defmacro aecase (expr &rest clauses)
  "Like `ecase', except that the value of EXPR is bound to `it'.

The variable `it' is available within CLAUSES.

EXPR and CLAUSES are otherwise as documented for `ecase'."
  (declare (indent 1))
  `(let ((it ,expr))
     (ecase it ,@clauses)))

;;;###autoload
(defmacro atypecase (expr &rest clauses)
  "Like `typecase', except that the value of EXPR is bound to `it'.

The variable `it' is available within CLAUSES.

EXPR and CLAUSES are otherwise as documented for `typecase'."
  (declare (indent 1))
  `(let ((it ,expr))
     (typecase it ,@clauses)))

;;;###autoload
(defmacro aetypecase (expr &rest clauses)
  "Like `etypecase', except that the value of EXPR is bound to `it'.

The variable `it' is available within CLAUSES.

EXPR and CLAUSES are otherwise as documented for `etypecase'."
  (declare (indent 1))
  `(let ((it ,expr))
     (etypecase it ,@clauses)))

;;;###autoload
(defmacro alet (varlist &rest body)
  "Like `let', except that the content of VARLIST is bound to `it'.

VARLIST as it appears in `it' is not evaluated.  The variable `it'
is available within BODY.

VARLIST and BODY are otherwise as documented for `let'."
  (declare (debug (sexp &rest form))
           (indent 1))
  `(let ((it ',varlist)
          ,@varlist)
     (progn ,@body)))

;;;###autoload
(defmacro a+ (&rest numbers-or-markers)
  "Like `+', except that the value of the previous expression is bound to `it'.

The variable `it' is available within all expressions after the
initial one.

NUMBERS-OR-MARKERS are otherwise as documented for `+'."
  (cond
    ((null numbers-or-markers)
     0)
    (t
     `(let ((it ,(car numbers-or-markers)))
        (+ it (a+ ,@(cdr numbers-or-markers)))))))

;;;###autoload
(defmacro a- (&optional number-or-marker &rest numbers-or-markers)
  "Like `-', except that the value of the previous expression is bound to `it'.

The variable `it' is available within all expressions after the
initial one.

NUMBER-OR-MARKER and NUMBERS-OR-MARKERS are otherwise as
documented for `-'."
  (cond
    ((null number-or-marker)
     0)
    ((null numbers-or-markers)
     `(- ,number-or-marker))
    (t
     `(let ((it ,(car numbers-or-markers)))
        (- ,number-or-marker (+ it (a+ ,@(cdr numbers-or-markers))))))))

;;;###autoload
(defmacro a* (&rest numbers-or-markers)
  "Like `*', except that the value of the previous expression is bound to `it'.

The variable `it' is available within all expressions after the
initial one.

NUMBERS-OR-MARKERS are otherwise as documented for `*'."
  (cond
    ((null numbers-or-markers)
     1)
    (t
     `(let ((it ,(car numbers-or-markers)))
        (* it (a* ,@(cdr numbers-or-markers)))))))

;;;###autoload
(defmacro a/ (dividend divisor &rest divisors)
  "Like `/', except that the value of the previous divisor is bound to `it'.

The variable `it' is available within all expressions after the
first divisor.

DIVIDEND, DIVISOR, and DIVISORS are otherwise as documented for `/'."
  (cond
    ((null divisors)
     `(/ ,dividend ,divisor))
    (t
     `(let ((it ,divisor))
        (/ ,dividend (* it (a* ,@divisors)))))))

(provide 'anaphora)

;;
;; Emacs
;;
;; Local Variables:
;; indent-tabs-mode: nil
;; mangle-whitespace: t
;; require-final-newline: t
;; coding: utf-8
;; byte-compile-warnings: (not cl-functions)
;; End:
;;
;; LocalWords: Anaphora EXPR awhen COND ARGS alambda ecase typecase
;; LocalWords: etypecase aprog aand acond ablock acase aecase alet
;; LocalWords: atypecase aetypecase VARLIST
;;

;;; anaphora.el ends here
