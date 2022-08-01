(ns fractalis.fractal)

;; exp

;; parse string into equation and calcul


"z*z"

(rest "abc")
(first "abc")

;; input : string
;; output : next-complex-function, with complex input [1 2]

;; patterns : 
;; z*z : square
;; z+1 : real addition
;; z+i : imaginary addition
;; z   : identity
;; z/2 : real division
;; (z+1)/2 : composition


;; try
;; eval math string 
;; https://stackoverflow.com/questions/35675832/evaluate-math-string-in-clojure

(defn next-complex-function [string]
	(let [first (first string)]
		(if (= string "z*z")
			(fn [z]
				(multiply-complex z z))
			(if (= string "z+z")
				(fn [z]
					(add-complex z z)))
			(if (= string "z-z")
				(fn [z]
					(subtract-complex z z)))
			)))

([1 2] 0)

(defn z-map [z-vector]
	{:real (z-vector 0)
	 :imaginary (z-vector 1)})

((next-complex-function "z*z") (z-map [2 0]))
((next-complex-function "z+z") (z-map [1 1]))
((next-complex-function "z-z") (z-map [1 1]))
((next-complex-function "z^2") (z-map [1 1]))
((next-complex-function "z/z") (z-map [1 1])) ;; => error, can't divide 2 complexs
((next-complex-function "exp(z)") (z-map [1 1])) ;; => error, exp not allowed
((next-complex-function "cos(z)") (z-map [1 1])) ;; => error, cos not allowed
((next-complex-function "arccos(z)") (z-map [1 1])) ;; => error, arccos not allowed
((next-complex-function "sin(z)") (z-map [1 1])) ;; => error, sin not allowed
((next-complex-function "arcsin(z)") (z-map [1 1])) ;; => error, arcsin not allowed

;; exp


;; api 
;;mandelbrot-set [delta]
;;points-to-check [delta]

(defn subtract-complex [z1 z2]
  {:real (- (z1 :real) (z2 :real))
   :imaginary (- (z1 :imaginary) (z2 :imaginary))})

(defn add-complex [z1 z2]
  {:real (+ (z1 :real) (z2 :real))
   :imaginary (+ (z1 :imaginary) (z2 :imaginary))})


(defn multiply-complex [z1 z2]
  {:real (- (* (z1 :real) (z2 :real))
            (* (z1 :imaginary) (z2 :imaginary)))
   :imaginary (+ (* (z1 :real) (z2 :imaginary))
                 (* (z1 :imaginary) (z2 :real)))})

(defn next-complex-number
	([c]
		(next-complex-number {:real 0 :imaginary 0} c))
	([z c]
  	(add-complex (multiply-complex z z) c)))

(comment
	(def c {:real 1.1 :imaginary -1.4})
	(def z {:real 1.1 :imaginary -1.4})
	(add-complex (multiply-complex z z) c)
	(multiply-complex z z))

(defn read-complex [string])
(defn write-complex [z]
	(if (< (z :imaginary) 0)
		(str (z :real) " - " (- (z :imaginary)) "i")
		(str (z :real) " + " (z :imaginary) "i")))


(comment
	(def z1-test
	{:real 1
	 :imaginary 1})

	(def z2-test
		{:real 2
		 :imaginary 2})	
	(add-complex z1-test z2-test)
	(multiply-complex z1-test z2-test)

	(def z0 {:real 0 :imaginary 0})
	(def c {:real 1 :imaginary 1})
	(def z1 (next-complex-number z0 c))
	(def z2 (next-complex-number z1 c))
	z1
	z2

	(def z3-test
		{:real 1
		 :imaginary -97})

	(def z4-test
		{:real 1
		 :imaginary -107})

	(def multiplied (multiply-complex z0 z0))
	(add-complex multiplied c)

	(write-complex z0)
	(write-complex z1)
	(write-complex z2-test)
	(write-complex z3-test)
	(write-complex z4-test))


(defn number-of-points-on-row [delta]
  (int (Math/ceil 
          (+ 1 
            (/ 4 delta)))))

(defn number-of-points-on-col [delta]
  (number-of-points-on-row delta))


(defn number-of-points-to-check [delta]
  (* (number-of-points-on-row delta)
     (number-of-points-on-col delta)))


(comment
  (def delta 0.9)
  (number-of-points-on-row delta)
  (number-of-points-on-col delta)
  (int (Math/ceil delta))
  (number-of-points-to-check delta))


; x times from left to right
; y times from top to bottom
(defn point-to-check [x y delta]
	[(+ -2 (* x delta))  (+ 2 (- (* y delta)))])

(comment
	(+ -2 (* 1 1))
	(+ 2 (- (* 0 1)))
	(point-to-check (- (number-of-points-on-row 1) 1)
                  0
                 	1))


(defn not-before-last-point-on-row [point delta]
	(let [x (nth point 0)
				distance-with-end (- 2 x)]
		(if (> distance-with-end delta)
			true
			false)))

(defn not-before-last-point-on-col [point delta]
	(let [y (nth point 1)
				distance-with-end (Math/abs (- -2 y))]
		(if (> distance-with-end delta)
			true
			false)))


(defn next-point-to-check-on-row [point delta]
	(if (not-before-last-point-on-row point delta)
		(let [x (nth point 0)
			  	y (nth point 1)]
			[(+ x delta) y])
		[2 (nth point 1)]))

(defn next-point-to-check-on-col [point delta]
	(if (not-before-last-point-on-col point delta)
		(let [x (nth point 0)
			  	y (nth point 1)]
			[x (+ y (- delta))])
		[-2 (nth point 0)]))

(defn is-last-point-on-row [point]
	(if (= (nth point 0) 2)
		true
		false))

(comment
	(is-last-point-on-row [1.9 2])

	; delta = 1
	(next-point-to-check-on-row [-2 2] 1)
	(next-point-to-check-on-row [-1 2] 1)
	(next-point-to-check-on-row [0 2] 1)
	(next-point-to-check-on-row [1 2] 1)
	(next-point-to-check-on-row [2 2] 1)

	; delta = 0.9
	(next-point-to-check-on-row [-2 2] 0.9)
	(next-point-to-check-on-row [-1.1 2] 0.9)
	(next-point-to-check-on-row [-0.2 2] 0.9)
	(next-point-to-check-on-row [0.7 2] 0.9)
	(next-point-to-check-on-row [1.6 2] 0.9)
	(next-point-to-check-on-row [2 2] 0.9)

	(Math/abs (- -2 1.5)))

(defn first-point-on-next-row [last-point-on-row delta]
	(let [y (nth last-point-on-row 1)
				distance-with-end (Math/abs (- -2 y))]
		(if (< distance-with-end delta)
			[-2 -2]
			[-2 (+ y (- delta))])))

(comment
	(Math/abs (- -2 1))
	(> (Math/abs (- -2 1)) 0.5)
	(first-point-on-next-row [2 1] 0.5))


(defn next-point-to-check [point delta]
		(if (is-last-point-on-row point)
			(first-point-on-next-row point delta)
			(next-point-to-check-on-row point delta)))

(comment 
  (next-point-to-check [1 1] 0.5)
  (next-point-to-check [1.5 1] 0.5)
  (next-point-to-check [2 1] 0.5)
  (next-point-to-check [-2 0.5] 0.5))
  ; ...
  

(defn next-points [points delta]
  (let [last-point (last points)
        next-point (next-point-to-check last-point delta)]
    (conj points next-point)))

(comment
  (next-points [[1 1] [1.5 1] [2 1]] 0.5))


; given delta precision, generates the points to check later
;;api
(defn points-to-check
  ([delta]
   (points-to-check [[-2 2]] delta))
  ([points delta]
    (let [last-point (last points)
          last-point-x (nth last-point 0)
          last-point-y (nth last-point 1)
          last-point-float [(float last-point-x) (float last-point-y)]
          is-last-point-to-check (= [2.0 -2.0] last-point-float)]
      (if is-last-point-to-check
        points
        (recur (next-points points delta) delta)))))

(points-to-check 2)

;; upgrade : remove recursivity from points-to-check
;; ->

(defn x-value [delta index]
	(let [increment (/ 4 (dec (number-of-points-on-row delta)))]
		delta
		(+ -2 (* delta index))))

(defn y-value [delta index]
	(- 2 (* delta index)))

(x-value 2 0)
(x-value 2 1)
(x-value 2 2)

(x-value 1 0)

(y-value 2 0)
(y-value 2 1)
(y-value 2 2)

;; 0 -> -2
;; 1 -> 0
;; 2 -> 2

(defn rowify [row]
	(map (partial x-value 2) row))

(rowify [0 1 2])

(defn columnify [col]
	(map (partial y-value 2) col))

(defn point [delta x-index y-index]
	[(x-value delta x-index) (y-value delta y-index)])

(point 2 0 0)

(defn row-indexes [delta col-index]
	(let [nr (number-of-points-on-row delta)]
		(map vector (range nr) (repeat nr col-index))))

(defn indexes [delta]
	;; ([0 0] [1 0] [2 0]
	;;  [0 1] [1 1] [2 1]
	;;  [0 2] [1 2] [2 2])
	(let [nc (number-of-points-on-col delta)]
		(map (partial row-indexes delta) (range nc)))
	)

(indexes 2)

(defn row-to-points [delta row]
	(map (fn [index-vector] (point delta (index-vector 0) (index-vector 1)))
			 row))

(row-to-points 2 [[0 0] [1 0] [2 0]])


;; api : new implementation
(defn points-to-check-flat [delta]
	(into [] cat
		(map (partial row-to-points delta) (indexes delta))))

(points-to-check-flat 2)

(= (points-to-check 2) (points-to-check-flat 2))
(= (points-to-check 1) (points-to-check-flat 1))
(= (points-to-check 0.5) (points-to-check-flat 0.5))
(= (points-to-check 0.1) (points-to-check-flat 0.1))

(comment
	(time (points-to-check-flat 0.005))
	(time (points-to-check 0.05))

	(count (points-to-check-flat 0.005))
	(count (points-to-check-flat 0.05)))

;; capacity increase : one order of delta
;; number of points x100
;; + more linear compute time on very low deltas

(range 3)
(def delta 2)
(def nr (number-of-points-on-row delta))
(def nc (number-of-points-on-col delta))

(concat
	(map vector (range nr) (repeat nr 0))
	(map vector (range nr) (repeat nr 1))
	(map vector (range nr) (repeat nr 2))
	(map vector (range nr) (repeat nr 3))
	(map vector (range nr) (repeat nr 4))
	)



(row-indexes 1 0)



(defn points)





(comment
	(flatten [[1 2] [3 4]])
	(vec
		(map (fn [x] [(x-value x 2) x])
			(range 
				(number-of-points-to-check 2))))

	(->> (repeat (number-of-points-on-col 2)
				(range (number-of-points-on-row 2)))
			(map rowify))

	(->> (repeat (number-of-points-on-col 2)
				(range (number-of-points-on-row 2)))
			 (map columnify))

	(point ))


(comment
  (points-to-check 2)
  (points-to-check 1)
  (points-to-check 0.5)
  (count (points-to-check 0.5))
  (->
    (points-to-check 0.5)
    count)
  (= [2 -2.0] [2 -2]))


(comment
	(def delta-used 3)
	(println (points-to-check delta-used))
	(count (points-to-check delta-used))
	(number-of-points-to-check delta-used))


;; upgrade : performance tracking
;; plot points to check
;; point number-of-points-to-check = f(delta)
;; plot smoothly while it calculates (plot time-to-calc = f(delta) and f(number-of-points)

;; 2    9     <100ms
;; 1    25    <100ms
;; 0.5  81    <100ms
;; 0.1  1681  179ms
;; 0.05 6561  1.9s
;; 0.04 10201 4.4s
;; 0.03 18225 13s
;; 0.02 40401 76s


; turn [1 1] into [{:x 1 :y 1}]
;;upgrade : into function definition
(defn point-data [point]
	{:x (nth point 0)
	 :y (nth point 1)})

(comment 
	(point-data [1 1])
	(def points [[-2 2] [-1.5 -2] [-1.0 -2]])
	(map point-data points)
	(type (map point-data points))
	(vec (map point-data points)))


;; divergence
;; diverges if goes out of 2 by 2 square
;; try 100 times maximum, if no divergence, then assume convergence

(defn next-point 
	([initial-point]
		(next-point [0 0] initial-point))
	([point initial-point]
			(let [initial-complex {:real (initial-point 0) :imaginary (initial-point 1)}
						complex {:real (point 0) :imaginary (point 1)}
						next-complex (next-complex-number complex initial-complex)]
				[(next-complex :real) (next-complex :imaginary)])))


(comment
	(next-complex-number {:real 0 :imaginary 0} {:real 1 :imaginary 1})

	(def initial-point [1 1])
	(next-point initial-point)
	(next-point [0 0] initial-point)
	(next-point [1 1] initial-point)
	(next-point [1 2] initial-point)

	(next-point [1 1] initial-point)
	(next-point [1 1])
	(next-point [1 2])
	(next-point [1.1 -1.4] [1.1 -1.4])
	(next-complex-number {:real 1.1 :imaginary -1.4} {:real 1.1 :imaginary -1.4})


	)



(defn out-of-2-by-2-square [point]
	(if (or (> (point 0) 2) (> (point 1) 2)
					(< (point 0) -2) (< (point 1) -2))
		true
		false))

;; upgrade : out-of-2-by-2-circle

(comment
	(out-of-2-by-2-square [1 1])
	(out-of-2-by-2-square [2 2])
	(out-of-2-by-2-square [2.1 2.1])
	(out-of-2-by-2-square [2.1 1.9])
	(out-of-2-by-2-square [1.9 2.1])
	(out-of-2-by-2-square [0 0])
	(out-of-2-by-2-square [-2 -2])
	(out-of-2-by-2-square [-2.1 -0])
	(out-of-2-by-2-square [-2.1 -2.1]))


(defn next-sequence-point [sequence-point]
  (let [next-point (next-point (sequence-point :point) 
                               (sequence-point :initial-point))
        next-n (+ 1 (sequence-point :n))]
    (assoc sequence-point 
           :point next-point
           :n next-n)))

(comment
  (def point [1 1])
  (def sequence-point {:point [1 1] :initial-point [1 1] :n 1})
  (next-sequence-point sequence-point)
  (if (out-of-2-by-2-square point)
    false
    (next-point point)))

(defn diverges? [sequence-point]
	(if (out-of-2-by-2-square (sequence-point :point))
		true
		(if (> (sequence-point :n) 100)
			false
			(recur (next-sequence-point sequence-point)))))

(diverges? {:n 1 :initial-point [1 1] :point [1 1]})
(next-sequence-point {:n 1 :initial-point [1 1] :point [1 1]})
(diverges? {:n 1 :initial-point [0 0] :point [0 0]})
(diverges? {:n 1 :initial-point [-1 0] :point [-1 0]})


(defn belongs-to-mandelbrot-set [point]
	(let [x (point 0)
				first-sequence-point {:n 1 :initial-point point :point point}
				diverges? (diverges? first-sequence-point)]
		(not diverges?)))


(defn next-sequence-point-reducable [sequence-point string]
	(next-sequence-point sequence-point))


(comment

	(belongs-to-mandelbrot-set [1.1 -1.4]) ;;should return false
	(belongs-to-mandelbrot-set [1.1 1.4]) ;;should be symetric


	(def point [1.1 -1.4])
	(def first-sequence-point {:n 1 :initial-point point :point point})
	(next-sequence-point first-sequence-point)
	(next-sequence-point first-sequence-point)
	;; reduce here
	(reduce next-sequence-point [first-sequence-point first-sequence-point])

	(next-sequence-point-reducable first-sequence-point "")
	(reduce next-sequence-point-reducable [first-sequence-point 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0])
	
	)


(next-sequence-point {:n 1 :initial-point [0.8 0] :point [0.8 0]})
(next-sequence-point {:n 1 :initial-point [0.8 0] :point [1.44 0]})
;; should be 3.68, is 2.87


;;api
(defn mandelbrot-set [delta]
	(filter belongs-to-mandelbrot-set (points-to-check delta)))


(comment 
	(mandelbrot-set 2)
	(mandelbrot-set 1)
	(mandelbrot-set 0.5)

	([2 0] 0)
	(nth [2 0] 0))






;; better algorithm

;; colors : 
;; black => belongs to mandelbrot set
;; blue => many iterations needed to diverge
;; green => little less
;; yellow => even less
;; orange => few needed
;; red => zero or one

; step 1 : plot only black
;; webgl bindings
;; pixels

;;webgl canva bindings
