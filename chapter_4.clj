;; 1. Write a function to find out if a string is a palindrome–that is, if it
;; looks the same forwards and backwards.
(defn palindrome? [input]
  (= (seq input) (reverse input)))

;; 2. Find the number of ‘c’s in “abracadabra”.
(reduce #(if (= \c %2) (inc %1) %1) 0 "abracadabra")

;; 3. Write your own version of filter.
(defn my-filter [filter-fn input-seq]
  (lazy-seq
   (when-let [s (seq input-seq)]
     (let [f (first s) r (rest s)]
       (if (filter-fn f)
         (cons f (my-filter filter-fn r))
         (my-filter filter-fn r))))))

(take 10 (my-filter odd? (range)))
(take 10 (filter odd? (range)))

;; 4. Find the first 100 prime numbers: 2, 3, 5, 7, 11, 13, 17, ....
(def prime-nums
  ((fn get-primes [primes sieve]
     (lazy-seq
      (let [d (first sieve)
            r (filter #(pos? (mod % d)) (rest sieve))]
        (cons d (get-primes primes r)))))
   [] (drop 2 (range))))

(take 10 prime-nums)
