(ns fractalis.fractal)

(defn add-complex [z1 z2]
  {:real (+ (z1 :real) (z2 :real))
   :imaginary (+ (z1 :imaginary) (z2 :imaginary))})


(defn multiply-complex [z1 z2]
  {:real (- (* (z1 :real) (z2 :real))
            (* (z1 :imaginary) (z2 :imaginary)))
   :imaginary (+ (* (z1 :real) (z2 :imaginary)
                  (* (z1 :imaginary) (z2 :real))))})


(defn next-complex-number
	([c]
		(next-complex-number {:real 0 :imaginary 0} c))
	([z c]
  	(add-complex (multiply-complex z z) c)))





(defn ensemble-mandelbrot
  ([c n]
    (let [z0 {:real 0 :imaginary 0}]
      (ensemble-mandelbrot [z0] c n)))
  ([ensemble-precedent c n]
    (if (= n 0)
      ensemble-precedent
      (let [last-complex-number (last ensemble-precedent)
            next-complex-number (next-complex-number last-complex-number c)
            nouvel-ensemble (conj ensemble-precedent next-complex-number)]
        (recur nouvel-ensemble c (- n 1))))))
;; liste des nombres complexes de mandelbrot de z0 à zn

(defn read-complex [string])
(defn write-complex [z]
	(if (< (z :imaginary) 0)
		(str (z :real) " - " (- (z :imaginary)) "i")
		(str (z :real) " + " (z :imaginary) "i")))



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
z1

(def z2 (next-complex-number z1 c))
z2

(def z3-test
	{:real 1
	 :imaginary -97})

(def z4-test
	{:real 1
	 :imaginary -107})

(def multiplied (multiply-complex z0 z0))
(add-complex multiplied c)



(def ensemble-mandelbrot-test 
	(map write-complex 
		(ensemble-mandelbrot c 4)))
ensemble-mandelbrot-test


(write-complex z0)
(write-complex z1)
(write-complex z2-test)
(write-complex z3-test)
(write-complex z4-test)



(ensemble-mandelbrot c 4)

;;api
(defn mandelbrot-data [c n]
  (ensemble-mandelbrot c n))


(def c11
  {:real 1
   :imaginary 1})

(mandelbrot-data c11 1)

(defn number-of-points-on-row [delta]
	(int (Math/ceil 
					(+ 1 
						(/ 4 delta)))))

(defn number-of-points-on-col [delta]
	(number-of-points-on-row delta))


(defn number-of-points-to-check [delta]
	(* (number-of-points-on-row delta)
		 (number-of-points-on-col delta)))


; (number-of-points-on-row 0.9)
; (number-of-points-on-col 0.9)
; (int (Math/ceil 0.9))
; (number-of-points-to-check 0.9)


; x times from left to right
; y times from top to bottom
(defn point-to-check [x y delta]
	[(+ -2 (* x delta))  (+ 2 (- (* y delta)))])

(+ -2 (* 1 1))
(+ 2 (- (* 0 1)))
(point-to-check (- (number-of-points-on-row 1) 1)
                 0
                 1)


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
		[2 (nth point 1)])
	)

(defn next-point-to-check-on-col [point delta]
	(if (not-before-last-point-on-col point delta)
		(let [x (nth point 0)
			  	y (nth point 1)]
			[x (+ y (- delta))])
		[-2 (nth point 0)])
	)

(defn is-last-point-on-row [point]
	(if (= (nth point 0) 2)
		true
		false))

(is-last-point-on-row [1.9 2])


; perfect
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

(Math/abs (- -2 1.5))

(defn first-point-on-next-row [last-point-on-row delta]
	(let [y (nth last-point-on-row 1)
				distance-with-end (Math/abs (- -2 y))]
		(if (< distance-with-end delta)
			[-2 -2]
			[-2 (+ y (- delta))]
			)))

; (Math/abs (- -2 1))
; (> (Math/abs (- -2 1)) 0.5)

(first-point-on-next-row [2 1] 0.5)


(defn next-point-to-check [point delta]
		(if (is-last-point-on-row point)
			(first-point-on-next-row point delta)
			(next-point-to-check-on-row point delta)
			))

; perfect
(next-point-to-check [1 1] 0.5)
(next-point-to-check [1.5 1] 0.5)
(next-point-to-check [2 1] 0.5)
(next-point-to-check [-2 0.5] 0.5)
; ...

(defn next-points [points delta]
	(let [last-point (last points)
				next-point (next-point-to-check last-point delta)]
		(conj points next-point)))

(next-points [[1 1] [1.5 1] [2 1]] 0.5)


; given delta precision, generates the points to check later
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

; (= [2 -2.0] [2 -2])


(def delta-used 3)
; (println (points-to-check delta-used))
(count (points-to-check delta-used))
(number-of-points-to-check delta-used)



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





; (defn is-borned [])
; try getting the next, until stop
; if infinite loop (10 turns almost equal => true)
; if goes out of range (try fail => false)
; else continue


; turn [1 1] into [{:x 1 :y 1}]

(defn point-data [point]
	{:x (nth point 0)
	 :y (nth point 1)})

; (point-data [1 1])
; (def points [[-2 2] [-1.5 -2] [-1.0 -2]])
; (map point-data points)
; (type (map point-data points))
; (vec (map point-data points))

;; api
(defn points-to-check-data [delta]
	(let [points (points-to-check delta)]
		(vec (map point-data points))))


(points-to-check-data 1)

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

(next-complex-number {:real 0 :imaginary 0} {:real 1 :imaginary 1})

(def initial-point [1 1])
(next-point initial-point)
(next-point [0 0] initial-point)
(next-point [1 1] initial-point)
(next-point [1 2] initial-point)
;; [-2 5]
;; 5 > 2 => diverges
;; [1 1] is not part of mandelbrot set


(next-point [1 1] initial-point)
(next-point [1 1])
(next-point [1 2])

(defn out-of-2-by-2-square [point]
	(if (or (> (point 0) 2) (> (point 1) 2)
					(< (point 0) -2) (< (point 1) -2))
		true
		false))

(out-of-2-by-2-square [1 1])
(out-of-2-by-2-square [2 2])
(out-of-2-by-2-square [2.1 2.1])
(out-of-2-by-2-square [2.1 1.9])
(out-of-2-by-2-square [1.9 2.1])
(out-of-2-by-2-square [0 0])
(out-of-2-by-2-square [-2 -2])
(out-of-2-by-2-square [-2.1 -0])
(out-of-2-by-2-square [-2.1 -2.1])


(def point [1 1])
(def sequence-point {:point [1 1] :initial-point [1 1] :n 1})

(defn next-sequence-point [sequence-point]
	(let [next-point (next-point (sequence-point :point) 
															 (sequence-point :initial-point))
				next-n (+ 1 (sequence-point :n))]
		(assoc sequence-point 
					 :point next-point
					 :n next-n)))

(next-sequence-point sequence-point)
(if (out-of-2-by-2-square point)
	false
	(next-point point))

(defn diverges? [sequence-point]
	(if (out-of-2-by-2-square (sequence-point :point))
		true
		(if (> (sequence-point :n) 100)
			false
			(recur (next-sequence-point sequence-point)))))

(diverges? {:n 1 :initial-point [1 1] :point [1 1]})
(diverges? {:n 1 :initial-point [0 0] :point [0 0]})
(diverges? {:n 1 :initial-point [-1 0] :point [-1 0]})


(defn belongs-to-mandelbrot-set [point]
	(let [x (point 0)
				first-sequence-point {:n 1 :initial-point point :point point}
				diverges? (diverges? first-sequence-point)]
		(not diverges?)))


(belongs-to-mandelbrot-set [-2 2])
(belongs-to-mandelbrot-set [-2 0])
(belongs-to-mandelbrot-set [0 2])
(belongs-to-mandelbrot-set [0 0])
(belongs-to-mandelbrot-set [0 1])
(belongs-to-mandelbrot-set [0 0.5])
(belongs-to-mandelbrot-set [0.8 0])
(belongs-to-mandelbrot-set [-1 0.4])


(next-sequence-point {:n 1 :initial-point [0.8 0] :point [0.8 0]})
(next-sequence-point {:n 1 :initial-point [0.8 0] :point [1.44 0]})
;; should be 3.68, is 2.87

(defn mandelbrot-set [delta]
	(filter belongs-to-mandelbrot-set (points-to-check delta)))


(mandelbrot-set 2)
(mandelbrot-set 1)
(mandelbrot-set 0.5)


;;api
(defn mandelbrot-set-data [delta]
	(let [points (mandelbrot-set delta)]
		(vec (map point-data points))))


(comment 
	(mandelbrot-set-data 2)
	(mandelbrot-set-data 0.2)
	(count 
		(mandelbrot-set-data 0.2)))

([2 0] 0)
(nth [2 0] 0)


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