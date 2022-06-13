(ns fractalis.fractal)

(defn add-complex [z1 z2]
  {:real (+ (z1 :real) (z2 :real))
   :imaginary (+ (z1 :imaginary) (z2 :imaginary))})


(defn multiply-complex [z1 z2]
  {:real (- (* (z1 :real) (z2 :real))
            (* (z1 :imaginary) (z2 :imaginary)))
   :imaginary (+ (* (z1 :real) (z2 :imaginary)
                  (* (z1 :imaginary) (z2 :real))))})


(defn next-complex-number [z c]
  (add-complex (multiply-complex z z) c))



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


;wip
; (defn generate-points-to-check [delta]
; 	(for [y (range 0 (+ 1 (number-of-points-on-col delta)))]
; 		(for [x (range 0 (+ 1 (number-of-points-on-row delta)))]
; 			(conj)
; 		)))


; [[-2 2] [0 2] [2 2]
;  [-2 0] [0 0] [2 0]
;  [-2 -2] [0 -2] [2 -2]])

(range 1 (+ 1 (number-of-points-on-row 0.5)))
(range 1 4)
(generate-point-to-check 1)

(defn is-borned [])
; try getting the next, until stop
; if infinite loop (10 turns almost equal => true)
; if goes out of range (try fail => false)
; else continue