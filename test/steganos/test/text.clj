(ns steganos.test.text
  (:use [clojure.test]
        [steganos.text]))

(deftest max-message-size-test
  (is (= (max-message-size 7 10 1)
         29))
  (is (= (max-message-size 10 10 1)
         41))
  (is (= (max-message-size 10 10 2)
         84)))

(deftest can-fit?-test
  (is (= (can-fit? "this string has 29 characters" 5 5 1)
         false))
  (is (= (can-fit? "this string has 29 characters" 7 10 1)
         true)))




