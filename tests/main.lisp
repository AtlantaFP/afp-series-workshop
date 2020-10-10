(defpackage afp-series-workshop/tests/main
  (:use :cl
        :afp-series-workshop
        :rove))
(in-package :afp-series-workshop/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :afp-series-workshop)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
