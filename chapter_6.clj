;; 1. Using the control flow constructs we’ve learned, write a schedule function which,
;; given an hour of the day, returns what you’ll be doing at that time. (schedule 18),
;; for me, returns :dinner.
(defn schedule [hour]
  (cond
    (< hour 6)   :sleep
    (< hour 8)   :getting-ready-and-breakfast
    (< hour 9)   :commute
    (< hour 18)  :office-work
    (< hour 19)  :commute
    (< hour 20)  :home-dinner
    (< hour 22)  :learn-new-stuff
    (<= hour 24) :sleep))

;; 2. Using the threading macros, find how many numbers from 0 to 9999 are palindromes:
;; identical when written forwards and backwards. 121 is a palindrome, as is 7447 and 5,
;; but not 12 or 953.
(defn palindrome? [n]
  (-> n str reverse clojure.string/join Integer. (= n)))

(->> (range 10000) (filter palindrome?) count)

;; 3. Write a macro id which takes a function and a list of args: (id f a b c), and
;; returns an expression which calls that function with the given args: (f a b c).
(defmacro id [f & args]
  `(~f ~@args))

;; 4. Write a macro log which uses a var, logging-enabled, to determine whether or not
;; to print an expression to the console at compile time. If logging-enabled is false,
;; (log :hi) should macroexpand to nil. If logging-enabled is true, (log :hi) should
;; macroexpand to (prn :hi). Why would you want to do this check during compilation,
;; instead of when running the program? What might you lose?
(def logging-enabled true)

(defmacro log [x]
  (if logging-enabled `(prn ~x) nil))

;; 5. (Advanced) Using the rationalize function, write a macro exact which rewrites any
;; use of +, -, *, or / to force the use of ratios instead of floating-point numbers.
;; (* 2452.45 100) returns 245244.99999999997, but (exact (* 2452.45 100)) should
;; return 245245N
(defmacro exact [[op & args]]
  `(~op ~@(map rationalize args)))
