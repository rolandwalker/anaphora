
(require 'anaphora)

;; some tests (but no code) adapted from anaphora.lisp

;;; anaphora-aif

(ert-deftest anaphora-aif-01 nil
  (should (= 3
             (aif (1+ 1)
                 (1+ it)))))

(ert-deftest anaphora-aif-02 nil
  (should (= 3
             (aif (1+ 1)
                 (progn
                   (1+ it)
                   (1+ it))))))

(ert-deftest anaphora-aif-03 nil
  (should (= 4
             (aif (1+ 1)
                 (progn
                   (incf it)
                   (1+ it))))))

(ert-deftest anaphora-aif-04 nil
  (should
   (aif nil
       (+ 5 it)
     (null it))))

(ert-deftest anaphora-aif-05 nil
  (should (equal '(nil 1)
                 (let ((x 0))
                   (aif (eval `(and ,(incf x) nil))
                       :never
                     (list it x))))))


;;; anaphora-aprog1

(ert-deftest anaphora-aprog1-01 nil
  (should (= 5
             (aprog1 5
               (assert (eq it 5))
               10))))

(ert-deftest anaphora-aprog1-02 nil
  (should (= 6
             (aprog1 5
               (incf it)
               10))))

(ert-deftest anaphora-aprog1-03 nil
  (should-error
   (aprog1 (1+ it)
     (1+ it))))


;;; anaphora-awhen

(ert-deftest anaphora-awhen-01 nil
  (should (= 3
             (awhen (1+ 1)
               (1+ it)))))

(ert-deftest anaphora-awhen-02 nil
  (should (= 4
             (awhen (1+ 1)
               (incf it)
               (1+ it)))))

(ert-deftest anaphora-awhen-03 nil
  (should (= 2
             (let ((x 0))
               (awhen (incf x)
                 (+ 1 it))))))

(ert-deftest anaphora-awhen-04 nil
  (should (= 1
             (let ((x 0))
               (or (awhen (not (incf x))
                     t)
                   x)))))


;;; anaphora-awhile

(ert-deftest anaphora-awhile-01 nil
  (should (equal '((4) (3 4) (2 3 4) (1 2 3 4))
                 (let ((list '(1 2 3 4))
                       (out nil))
                   (awhile list
                     (push it out)
                     (pop list))
                   out))))

(ert-deftest anaphora-awhile-02 nil
  (should (equal '((5 4) (5 3 4) (5 2 3 4) (5 1 2 3 4))
                 (let ((list '(1 2 3 4))
                       (out nil))
                   (awhile list
                     (push 5 it)
                     (push it out)
                     (pop list))
                   out))))


;;; anaphora-aand

(ert-deftest anaphora-aand-01 nil
  (should (= 3
             (aand (1+ 1)
                   (1+ it)))))

(ert-deftest anaphora-aand-02 nil
  (should (= 5
             (aand (1+ 1)
                   (1+ it)
                   (1+ it)
                   (1+ it)))))

(ert-deftest anaphora-aand-03 nil
  (should (= 5
             (aand (1+ 1)
                   (1+ it)
                   (incf it)
                   (1+ it)))))

(ert-deftest anaphora-aand-04 nil
  (should (equal '(1 2 3)
                 (aand (1+ 1)
                       '(1 2 3)
                       it))))

(ert-deftest anaphora-aand-05 nil
  (should-error
   (aand (1+ it)
         (1+ it))))


;;; anaphora-acond

(ert-deftest anaphora-acond-01 nil
  (should (= 1
             (acond (1)))))

(ert-deftest anaphora-acond-02 nil
  (should-not
   (acond (1 nil))))

(ert-deftest anaphora-acond-03 nil
  (should
   (acond (1 t))))

(ert-deftest anaphora-acond-04 nil
  (should (eq :foo
              (acond (:foo) ("bar") (:baz)))))

(ert-deftest anaphora-acond-05 nil
  (should (= 1
             (acond (:foo 1) ("bar") (:baz)))))

(ert-deftest anaphora-acond-06 nil
  (should (= 1
             (acond (1 it)))))

(ert-deftest anaphora-acond-07 nil
  (should (= 2
             (acond (1 (1+ it))))))

(ert-deftest anaphora-acond-08 nil
  (should (= 3
             (acond
               (nil 4)
               (2 (1+ it))))))

(ert-deftest anaphora-acond-09 nil
  (should (equal '(:yes 3)
                 (acond
                   ((null 1)
                    (list :no it))
                   ((+ 1 2)
                    (list :yes it))
                   (t
                    :nono)))))

(ert-deftest anaphora-acond-10 nil
  (should (eq :yes
              (acond
                ((= 1 2)
                 :no)
                (nil
                 :nono)
                (t
                 :yes)))))

(ert-deftest anaphora-acond-11 nil
  (should (= 42
             (let ((foo))
               (acond
                 ((+ 2 2)
                  (setf foo 38)
                  (incf foo it)
                  foo)
                 (t
                  nil))))))


;;; anaphora-alambda

(ert-deftest anaphora-alambda-01 nil
  (should (= 120
             (funcall (alambda (x)
                        (if (= x 0) 1 (* x (self (1- x))))) 5))))

(ert-deftest anaphora-alambda-02 nil
  (should (equal '(1 2 1 2)
                 (let ((obj 'a))
                   (mapcar (alambda (list)
                             (if (consp list)
                                 (+ (if (eq (car list) obj) 1 0)
                                    (self (car list))
                                    (self (cdr list)))
                               0))
                           '((a b c) (d a r (p a)) (d a r) (a a)))))))


;;; anaphora-ablock

(ert-deftest anaphora-ablock-01 nil
  (should-not
   (ablock testing
     1
     (1+ it)
     (1+ it)
     (return-from testing))))

(ert-deftest anaphora-ablock-02 nil
  (should (= 4
             (ablock testing
               1
               (1+ it)
               (1+ it)
               (return-from testing (1+ it))))))

(ert-deftest anaphora-ablock-03 nil
  (should (= 3
             (ablock testing
               1
               (1+ it)
               (1+ it)))))

(ert-deftest anaphora-ablock-04 nil
  (should (= 0
             (ablock testing
               1
               (1+ it)
               (1+ it)
               0))))


;;; anaphora-acase

(ert-deftest anaphora-acase-01 nil
  (should (equal '(:yes 1)
                 (let ((x 0))
                   (acase (incf x)
                     (0 :no)
                     (1 (list :yes it))
                     (2 :nono))))))

(ert-deftest anaphora-acase-02 nil
  (should (equal '(:yes 1)
                 (let ((x 0))
                   (acase (incf x)
                     (0 :no)
                     ((incf it) (list :yes it))
                     (1 (list :yes it)))))))

(ert-deftest anaphora-acase-03 nil
  (should (equal "bb"
                 (acase ?b
                   (?a "a")
                   (?c "c")
                   (?d "d")
                   (otherwise (string ?b it))))))


;;; anaphora-aecase

(ert-deftest anaphora-aecase-01 nil
  (should (equal '(:yes 1)
                 (let ((x 0))
                   (aecase (incf x)
                     (0 :no)
                     (1 (list :yes it))
                     (2 :nono))))))

(ert-deftest anaphora-aecase-02 nil
  (should-error
   (aecase ?b
     (?a "a")
     (?c "c")
     (?d "d"))))


;;; anaphora-atypecase

(ert-deftest anaphora-atypecase-01 nil
  (should (= 0.0
             (atypecase 1.0
               (integer
                (+ 2 it))
               (float
                (1- it))))))

(ert-deftest anaphora-atypecase-02 nil
  (should-not
   (atypecase "Foo"
     (fixnum
      :no)
     (hash-table
      :nono))))

;;; anaphora-aetypecase

(ert-deftest anaphora-aetypecase-01 nil
  (should (= 0.0
             (aetypecase 1.0
               (integer
                (+ 2 it))
               (float
                (1- it))))))

(ert-deftest anaphora-aetypecase-02 nil
  (should-error
   (aetypecase "Foo"
     (fixnum
      :no)
     (hash-table
      :nono))))


;;; anaphora-alet

(ert-deftest anaphora-alet-01 nil
  (should (= 1
             (alet ((x 1)
                    (y 2)
                    (z 3))
               x))))

(ert-deftest anaphora-alet-02 nil
  (should (equal '(y 2)
                 (alet ((x 1)
                        (y 2)
                        (z 3))
                   (nth 1 it)))))

(ert-deftest anaphora-alet-03 nil
  (should (eq 'y
              (alet (x y z)
                (car (memq 'y it))))))

(ert-deftest anaphora-alet-04 nil
  (should (equal '(x y z)
                 (let ((vars '((x 1)
                               (y 2)
                               (z 3))))
                   (eval `(alet ,vars
                            (mapcar 'car it)))))))


;;; anaphora-a+

(ert-deftest anaphora-a+-01 nil
  (should (= 0
             (a+))))

(ert-deftest anaphora-a+-02 nil
  (should (= 2
             (a+ 2))))

(ert-deftest anaphora-a+-03 nil
  (should-error
   (progn
     (a+ it))))

(ert-deftest anaphora-a+-04 nil
  (should (= 9
             (a+ 2 3 4))))

(ert-deftest anaphora-a+-05 nil
  (should (= 13
             (a+ 2 3 4 it))))

(ert-deftest anaphora-a+-06 nil
  (should (= 15
             (a+ 2 3 4 it 2))))


;;; anaphora-a-

(ert-deftest anaphora-a--01 nil
  (should (= 0
             (a-))))

(ert-deftest anaphora-a--02 nil
  (should (= -2
             (a- 2))))

(ert-deftest anaphora-a--03 nil
  (should-error
   (progn
     (a- it))))

(ert-deftest anaphora-a--04 nil
  (should (= 13
             (a- 20 3 4))))

(ert-deftest anaphora-a--05 nil
  (should (= 9
             (a- 20 3 4 it))))

(ert-deftest anaphora-a--06 nil
  (should (= 7
             (a- 20 3 4 it 2))))


;;; anaphora-a*

(ert-deftest anaphora-a*-01 nil
  (should (= 1
             (a*))))

(ert-deftest anaphora-a*-02 nil
  (should (= 2
             (a* 2))))

(ert-deftest anaphora-a*-03 nil
  (should-error
   (progn
     (a* it))))

(ert-deftest anaphora-a*-04 nil
  (should (= 24
             (a* 2 3 4))))

(ert-deftest anaphora-a*-05 nil
  (should (= 96
             (a* 2 3 4 it))))

(ert-deftest anaphora-a*-06 nil
  (should (= 192
             (a* 2 3 4 it 2))))


;;; anaphora-a/

(ert-deftest anaphora-a/-01 nil
  (should-error
   (progn
     (a/))))

(ert-deftest anaphora-a/-02 nil
  (should-error
   (progn
     (a/ 200))))

(ert-deftest anaphora-a/-03 nil
  (should (= 40
             (a/ 200 5))))

(ert-deftest anaphora-a/-04 nil
  (should-error
   (progn
     (a/ 200 it))))

(ert-deftest anaphora-a/-05 nil
  (should (= 20
             (a/ 200 5 2))))

(ert-deftest anaphora-a/-06 nil
  (should (= 10
             (a/ 200 5 2 it))))

(ert-deftest anaphora-a/-07 nil
  (should (= 2
             (a/ 200 5 2 it 5))))


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

;;; anaphora-test.el ends here
