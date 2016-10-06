;; Finding the sum of the first 10000000 numbers takes about 1 second on my machine:
;; user=> (defn sum [start end] (reduce + (range start end))) user=> (time (sum 0 1e7))
;; "Elapsed time: 1001.295323 msecs"
;; 49999995000000

;; 1. Use delay to compute this sum lazily; show that it takes no time to return
;; the delay, but roughly 1 second to deref.
(defn sum [start end]
  (delay (reduce + (range start end))))

(def result (sum 0 1e7))

(time (deref result))

;; 2. We can do the computation in a new thread directly, using
;; (.start (Thread. (fn [] (sum 0 1e7)))–but this simply runs the (sum) function
;; and discards the results. Use a promise to hand the result back out of
;; the thread. Use this technique to write your own version of the future macro.


;; 3. If your computer has two cores, you can do this expensive computation
;; twice as fast by splitting it into two parts: (sum 0 (/ 1e7 2)),
;; and (sum (/ 1e7 2) 1e7), then adding those parts together. Use future to do
;; both parts at once, and show that this strategy gets the same answer as
;; the single-threaded version, but takes roughly half the time.

;; 4. Instead of using reduce, store the sum in an atom and use two futures to
;; add each number from the lower and upper range to that atom. Wait for both
;; futures to complete using deref, then check that the atom contains the right
;; number. Is this technique faster or slower than reduce? Why do you think that
;; might be?

;;  Instead of using a lazy list, imagine two threads are removing tasks from
;; a pile of work. Our work pile will be the list of all integers
;; from 0 to 10000:
;; user=> (def work (ref (apply list (range 1e5))))
;; user=> (take 10 @work)
;; (0 1 2 3 4 5 6 7 8 9)
;; And the sum will be a ref as well:
;; user=> (def sum (ref 0))
;; Write a function which, in a dosync transaction, removes the first number in
;; work and adds it to sum. Then, in two futures, call that function over and
;; over again until there’s no work left. Verify that @sum is 4999950000.
;; Experiment with different combinations of alter and commute–if both are
;; correct, is one faster? Does using deref instead of ensure change the result?