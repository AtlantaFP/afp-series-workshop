(defpackage afp-series-workshop
  (:use :cl))
(in-package :afp-series-workshop)

;; blah blah blah.
;; \sigma_{x=1}^{100}{x}

(defparameter *main-series* (make-array 10 :adjustable t))

;; Scanners
;; x - initial value
;; F - step function (essentially defining how the next element is computed)
;; P - predicate to determine where the series should end
;;
;; def scanning(x, F, P):
;;  if P(x) == true then return nil;
;;  else concat(x, scanning(F(x), F, P))

;; Collectors
;;
;; z - initial vlaue (usually left identity of F) i.e. F(x,z) = x.
;; F - binary function takes in the initial value as well as the current value of sequence we're
;;     reducing.
;; S - the series that we're applying the collection on.
;;
;; def collection(z, F, S):
;;   if empty(S) then return z;
;;   else return collection(F(z, S0), S');
;;
;; collection(0, \lambda(x,y).x+y, <1,2,3>) = 6

;; example of transducer
;; spreading(R, S, z)
;; spreading(<3,1>,<-7,-1>, 0) = <0,0,0,-7,0,-1>
;;
;; def spreading(R, S, z):
;;    if empty(R) or empty(S) then return <>;
;;    else if R0 = 0 then return <S0> || spreading(R', S', z);
;;    else return <z> || spreading(<R0-1> || R', S, z);

(defparameter *first5* ; Implementation of <1,2,3,4,5>
  (let ((x 0))
    (list #'(lambda (at-end)
              (if (< x 5)
                  (setf x (+ x 1))
                  (funcall at-end))))))

(defun generator (s)
  "returns a generator for the elements of a series"
  (let ((g (car s)))
    #'(lambda (at-end)
        (when (null (cdr s))
          (setf (cdr s)
                (block nil
                  (list (funcall g #'(lambda () (return T)))))))
        (if (not (eq (cdr s) T))
            (car (setf s (cdr s)))
            (funcall at-end)))))

(defun choose-if (p s)
  "Implementation of choosing(P,S)"
  (let ((gen (generator s)))
    (list #'(lambda (end-action)
              (loop (let ((x (funcall gen end-action)))
                      (if (funcall p x) (return x))))))))

(defun collect-sum (s)
  "Implementation of example collector using collection definition.
   computes collection(0, \lambda(x,y).x+y, S)"
  (let ((gen (generator s))
        (sum 0))
    (loop (let ((x (funcall gen #'(lambda () (return sum)))))
            (setf sum (+ sum x))))))

(disassemble '(lambda ()
               (collect-sum
                (choose-if #'oddp *first5*))))
