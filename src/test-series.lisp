#|
Series
- Library written by Dr. Richard C. Waters
- Motivation
  - Algorithms written in a functional style are easier to understand and modify
    than equivalent algorithms expressed as loops.
    - functional style
        - emphasis on function composition
        - operate mainly on data structures (i.e. series/vectors/streams)
- Goal
  - express computations with Series without any incurring any runtime overhead
- Library Features
  - efficient transformation of series expressions to iterative loops
  - built-in data type to represent series
  - collection of several dozen function split into three categories
    - Scanners : produce series without consuming any
    - Transducers : compute series from series
    - Collectors : consume series without producing any
- Benefits
  - retain most virtues of loop-free functional programming while eliminating
    most of the costs in about 80% of commonly programmed situations
  - Represents a major shift towards using expressions to represent
    computations across collections which forms a more declarative
    rather than imperative approach to programming.
  - Improved Conciseness, Readability 
- Disadvantages
  - series expressions may not be able to optimize efficiently in certain cases.
- BIG CAVEAT ON USING SERIES EXPRESSIONS (i.e. the following restrictions make up what is considered to be the class of optimizable sequence expressions)
  - Optimizable sequence expressions have to be statically analyzable
  - Optimizable expressions have to represent "straight-line computation"
  - Procedures called by optimizable sequence expressions must be preorder
  - Intermediate values in optimizable sequence expressions must be sequences
  - Every non-directed data flow cycle in an optimizable sequence expression online
|#
(defpackage #:com.rv.series.examples
  (:use :cl :series)
  (:export
   #:simple-collect-sum))

(in-package #:com.rv.series.examples)

;; first example: compute the sum of the positive numbers of a list

(with-open-file (strm #P"~/.roswell/local-projects/afp-series-workshop/src/test-data.txt" :if-does-not-exist :create)
  (loop :for line = (read-line strm nil)
        :until (null line)
        :when (oddp (parse-integer line))
        :summing (parse-integer line)))

(let ((sum 0))
  (dolist (x '(1 -2 3 -4))
    (when (plusp x)
      (setf sum (+ sum x))))
  (print sum))

(reduce #'+ (mapcar #'(lambda (x) (if (plusp x) x 0)) '(1 -2 3 -4)))

(print (collect-sum (choose-if #'plusp (scan '(1 -2 3 -4)))))

;; F, G, H s.t. F(G(H(x)))
;; 1. each element of the output of each procedure in the pipeline must be created and consumed one at a time.
;; 2. the elements are created IN THE SAME ORDER as they are created.
(series::let ((x (subseries (scan-range :from 0 :by 2) 0 5)))
  (values (collect x) (collect-sum x)))

(let ((res 0))
 (dotimes (x 11 res)
   (when (evenp x)
     (setf res (+ res x)))))

;; predefined scanners include:
;; series - create a unbounded series object repeating a given value
;; scan - enumerates true a SEQUENCE object (list or vector)
;; scan-range - enumerates through number (integer, float, single-float, etc.) in a range
;; scan-plist - creates a series of indicators in a property list along with a series of corresponding values
;; scan-file - create a series object out of a file (WARNING: reads entire file into memory)
(series 'a)
(scan '(a b c))
(scan 'vector #(a b c))
(scan-range)
(scan-plist '(a 1 b 2))
(scan-file #P"~/.roswell/local-projects/afp-series-workshop/src/test-series.lisp")


;; pre-defined Transducers include:
;;
;; positions : returns the non-nil positions inside a series
;; choose : creates a series by selecting the elements of its second argument that
;;          correspond to non-null elements of its first argument.
(positions (series:make-series 'a 'b nil 'c nil 'd nil))
(choose (series:make-series nil T T nil T) (series:make-series 1 2 3 4 5))

;;
;; pre-defined collectors
;;
(collect (series:make-series 'a 'b 'c))
(collect 'vector (series:make-series 'a 'b 'c))
(collect-sum (series:make-series 1 2 3 4 5))
(collect-sum (series:scan-range :from 0 :upto 10))
(collect-length (series:scan-range :from 0 :upto 10))
(collect-first (series:subseries (series:scan-range :from 0 :upto 100 :by 2) 1 10))
(collect 'vector (series:subseries (series:scan-range :from 0 :upto 100 :by 2) 4 10))


;;
;; simple example of creating user-defined functions
;;

(series::defun simple-collect-sum (numbers)
  (declare (optimizable-series-function))
  (collect-fn 'number #'(lambda () 0) #'+ numbers))

(print (simple-collect-sum (scan-range :from 0 :upto 10)))

(mapping ((x (scan-file #P"~/.roswell/local-projects/afp-series-workshop/src/test.data")))
         (expt (abs x) 3))

(iterate ((x (scan-file #P"~/.roswell/local-projects/afp-series-workshop/src/test.data")))
         (when (evenp x)
           (print x)))


;;
;; generating some test data
;;
;;(with-open-file (strm #P"~/lisp-projects/test.data" :direction :output :if-exists :supersede)
;;  (loop :for i
;;        :from 1
;;        :to 1000000
;;        :do
;;        (format strm "~A        ~A~%" i (* i i))))

(defparameter *test-file-path* #P"~/.roswell/local-projects/afp-series-workshop/src/test.data")
(defparameter *test-file* (scan-file *test-file-path*))

(time (collect-sum *test-file*))

(time (let ((lst (loop for i from 1 to 1000000 collecting i)))
        (loop for i in lst
              for j = (* i i)
              sum (+ i j))))

(with-open-file (strm *test-file-path* :direction :input)
  (time (loop :for i = (read strm nil nil)
              :until (null i)
              :sum i)))

(time (series::let ((fseries (scan-file *test-file-path*)))
        (collect-sum fseries)))

;;
;; example of why intermediate sequence value restriction is needed
;;
(series::defun normalized-max (fpath)
  (declare (optimizable-series-function))
  (let ((data (scan-file fpath)))
    (series:collect-max (series:map-fn 'vector #'(lambda (x) (/ x (series:collect-sum data))) data))))

(series::defun odd-sum (fpath)
  (declare (optimizable-series-function))
  (let ((data (scan-file fpath))
        (odd-data (choose-if #'oddp data)))
    (series:collect-sum (series:map-fn 'vector #'(lambda (x y) (* x y)) data odd-data))))
