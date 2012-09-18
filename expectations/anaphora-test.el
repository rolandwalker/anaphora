
(require 'anaphora)

;; some tests (but no code) adapted from anaphora.lisp

(expectations

  (desc "anaphora-aif")

  (expect 3
    (aif (1+ 1)
        (1+ it)))

  (expect 3
    (aif (1+ 1)
        (progn
          (1+ it)
          (1+ it))))

  (expect 4
    (aif (1+ 1)
        (progn
          (incf it)
          (1+ it))))

  (expect t
    (aif nil
        (+ 5 it)
      (null it)))

  (expect '(nil 1)
    (let ((x 0))
      (aif (eval `(and ,(incf x) nil))
          :never
        (list it x)))))


(expectations

  (desc "anaphora-aprog1")

  (expect 5
    (aprog1 5
      (assert (eq it 5))
      10))

  (expect 6
    (aprog1 5
      (incf it)
      10))

  (expect (error)
    (aprog1 (1+ it)
      (1+ it))))


(expectations

  (desc "anaphora-awhen")

  (expect 3
    (awhen (1+ 1)
      (1+ it)))

  (expect 4
    (awhen (1+ 1)
      (incf it)
      (1+ it)))

  (expect 2
    (let ((x 0))
      (awhen (incf x)
        (+ 1 it))))

  (expect 1
    (let ((x 0))
      (or (awhen (not (incf x))
            t)
          x))))


(expectations

  (desc "anaphora-awhile")

  (expect '((4) (3 4) (2 3 4) (1 2 3 4))
    (let ((list '(1 2 3 4))
          (out nil))
      (awhile list
        (push it out)
        (pop list))
      out))

  (expect '((5 4) (5 3 4) (5 2 3 4) (5 1 2 3 4))
    (let ((list '(1 2 3 4))
          (out nil))
      (awhile list
        (push 5 it)
        (push it out)
        (pop list))
      out)))


(expectations

  (desc "anaphora-aand")

  (expect 3
    (aand (1+ 1)
          (1+ it)))

  (expect 5
    (aand (1+ 1)
          (1+ it)
          (1+ it)
          (1+ it)))

  (expect 5
    (aand (1+ 1)
          (1+ it)
          (incf it)
          (1+ it)))

  (expect '(1 2 3)
    (aand (1+ 1)
          '(1 2 3)
          it))

  (expect (error)
    (aand (1+ it)
          (1+ it))))


(expectations

  (desc "anaphora-acond")

  (expect 1
    (acond (1)))

  (expect nil
    (acond (1 nil)))

  (expect t
    (acond (1 t)))

  (expect :foo
    (acond (:foo) ("bar") (:baz)))

  (expect 1
    (acond (:foo 1) ("bar") (:baz)))

  (expect 1
    (acond (1 it)))

  (expect 2
    (acond (1 (1+ it))))

  (expect 3
    (acond
     (nil 4)
     (2 (1+ it))))

  (expect '(:yes 3)
    (acond
     ((null 1)
      (list :no it))
     ((+ 1 2)
      (list :yes it))
     (t
      :nono)))

  (expect :yes
    (acond
     ((= 1 2)
      :no)
     (nil
      :nono)
     (t
      :yes)))

  (expect 42
    (let ((foo))
      (acond
        ((+ 2 2)
         (setf foo 38)
         (incf foo it)
         foo)
        (t
         nil)))))


(expectations

  (desc "anaphora-alambda")

  (expect 120
    (funcall (alambda (x) (if (= x 0) 1 (* x (self (1- x))))) 5))

  (expect '(1 2 1 2)
    (let ((obj 'a))
      (mapcar (alambda (list)
                (if (consp list)
                    (+ (if (eq (car list) obj) 1 0)
                       (self (car list))
                       (self (cdr list)))
                  0))
              '((a b c) (d a r (p a)) (d a r) (a a))))))


(expectations

  (desc "anaphora-ablock")

  (expect nil
    (ablock testing
      1
      (1+ it)
      (1+ it)
      (return-from testing)))

  (expect 4
    (ablock testing
      1
      (1+ it)
      (1+ it)
      (return-from testing (1+ it))))

  (expect 3
    (ablock testing
      1
      (1+ it)
      (1+ it)))

  (expect 0
    (ablock testing
      1
      (1+ it)
      (1+ it)
      0)))


(expectations

  (desc "anaphora-acase")

  (expect '(:yes 1)
    (let ((x 0))
      (acase (incf x)
        (0 :no)
        (1 (list :yes it))
        (2 :nono))))

  (expect '(:yes 1)
    (let ((x 0))
      (acase (incf x)
        (0 :no)
        ((incf it) (list :yes it))
        (1 (list :yes it)))))

  (expect "bb"
    (acase ?b
      (?a "a")
      (?c "c")
      (?d "d")
      (otherwise (string ?b it)))))


(expectations

  (desc "anaphora-aecase")

  (expect '(:yes 1)
    (let ((x 0))
      (aecase (incf x)
        (0 :no)
        (1 (list :yes it))
        (2 :nono))))

  (expect (error)
    (aecase ?b
      (?a "a")
      (?c "c")
      (?d "d"))))


(expectations

  (desc "anaphora-atypecase")

  (expect 0.0
    (atypecase 1.0
      (integer
       (+ 2 it))
      (float
       (1- it))))

  (expect nil
    (atypecase "Foo"
      (fixnum
       :no)
      (hash-table
       :nono))))

(expectations

  (desc "anaphora-aetypecase")

  (expect 0.0
    (aetypecase 1.0
      (integer
       (+ 2 it))
      (float
       (1- it))))

  (expect (error)
    (aetypecase "Foo"
      (fixnum
       :no)
      (hash-table
       :nono))))


(expectations

  (desc "anaphora-alet")

  (expect 1
    (alet ((x 1)
           (y 2)
           (z 3))
      x))

  (expect '(y 2)
    (alet ((x 1)
           (y 2)
           (z 3))
      (nth 1 it)))

  (expect 'y
    (alet (x y z)
      (car (memq 'y it))))

  (expect '(x y z)
    (let ((vars '((x 1)
                  (y 2)
                  (z 3))))
      (eval `(alet ,vars
               (mapcar 'car it))))))


(expectations

  (desc "anaphora-a+")

  (expect 0
    (a+))

  (expect 2
    (a+ 2))

  (expect (error)
    (a+ it))

  (expect 9
    (a+ 2 3 4))

  (expect 13
    (a+ 2 3 4 it))

  (expect 15
    (a+ 2 3 4 it 2)))


(expectations

  (desc "anaphora-a-")

  (expect 0
    (a-))

  (expect -2
    (a- 2))

  (expect (error)
    (a- it))

  (expect 13
    (a- 20 3 4))

  (expect 9
    (a- 20 3 4 it))

  (expect 7
    (a- 20 3 4 it 2)))


(expectations

  (desc "anaphora-a*")

  (expect 1
    (a*))

  (expect 2
    (a* 2))

  (expect (error)
    (a* it))

  (expect 24
    (a* 2 3 4))

  (expect 96
    (a* 2 3 4 it))

  (expect 192
    (a* 2 3 4 it 2)))


(expectations

  (desc "anaphora-a/")

  (expect (error)
    (a/))

  (expect (error)
    (a/ 200))

  (expect 40
    (a/ 200 5))

  (expect (error)
    (a/ 200 it))

  (expect 20
    (a/ 200 5 2))

  (expect 10
    (a/ 200 5 2 it))

  (expect 2
    (a/ 200 5 2 it 5)))


;;
;; Emacs
;;
;; Local Variables:
;; indent-tabs-mode: nil
;; mangle-whitespace: t
;; require-final-newline: t
;; coding: utf-8
;; byte-compile-warnings: (not cl-functions redefine)
;; End:
;;

;;; anaphora-test.el ends here
