(ns fractalis.equation-parser)

;; end goal : 
;; (parse-eq "2z+1")
;; (parse-eq "(2z+1)(2z+2)")
;; (parse-eq "((2z+1)+1)(2z+2)")


;; input : string
;; output : next-complex-function, with complex input [1 2]

;; patterns : 
;; z*z : square
;; z+1 : real addition
;; z+i : imaginary addition
;; z   : identity
;; z/2 : real division
;; (z+1)/2 : composition


(defn not-closing-paren? [char]
	(not= \) char))

(defn not-opening-paren? [char]
	(not= \( char))

(defn closing-paren? [char]
	(= \) char))

(defn opening-paren? [char]
	(= \( char))

(defn contains-opening-paren? [string]
	(clojure.string/includes? string "("))

(defn contains-closing-paren? [string]
	(clojure.string/includes? string ")"))

(defn leaf-paren? [pos equation]
	(let [char (nth equation pos)
				after-paren (drop (+ 1 pos) equation)
				content-seq (take-while not-closing-paren? after-paren)
				content (apply str content-seq)
				]
		(if (not-opening-paren? char)
			false
			(if (contains-opening-paren? content)
				false
				true))))
	

(leaf-paren? 0 "((2z+1)*z)+1)") ;;=> false
(leaf-paren? 1 "((2z+1)*z)+1)") ;;=> true
(leaf-paren? 6 "((2z+1)*z)+1)") ;;=> false
(leaf-paren? 8 "((2z+1)*(z+1))") ;;=> true


(defn first-leaf-paren-pos [equation]
	(first 
		(filter #(leaf-paren? % equation) 
			(range (count equation)))))


(first-leaf-paren-pos "((2z+1)*z)+1)")

(defn no-paren-equation? [equation]
	(and (not (contains-opening-paren? equation)) 
		(not (contains-closing-paren? equation))))

(no-paren-equation? "2z+1)")


(defn first-leaf-expr [equation]
	(if (no-paren-equation? equation)
		equation
		(let [pos (first-leaf-paren-pos equation)
					after-paren (drop (+ 1 pos) equation)
					content-seq (take-while not-closing-paren? after-paren)
					leaf-expr (apply str content-seq)
					]
			leaf-expr
			)))

(first-leaf-expr "((2z+1)*z)+1)") ;;=> 2z+1
(first-leaf-expr "(2z+1)") ;; => 2z+1
(first-leaf-expr "2z+1") ;; => 2z+1
(first-leaf-expr "2z+1)") ;; => error


"((2z+1)*z)+1)"
;; [(2z+1), *z, +1]
;; [[2, *z, +1], *z, +1]
;; [2, *z, +1, *z, +1]


;; (2z+1)

;; not used for now
(defn remove-paren [eq]
	(if (and (= \( (first eq))
					(= \) (last eq)))
		(apply str
			(take (- (count eq) 2) 
				(rest eq)))
		eq))

(remove-paren "(2z+1)")
(remove-paren "2z+1")

(first "(2z+1)")

(defn add-to-string [string char]
	(apply str
		(conj (vec string) char)))

(add-to-string "string" "a")
(add-to-string "string" \a)
(add-to-string "string" "ab")


(defn in? [string char]
	(clojure.string/includes? string (str char)))

(in? "abcdefghijklmnopqrstuvwxyz" \z)
(in? "abcdefghijklmnopqrstuvwxyz" \0)
(in? "0123456789" \z)
(in? "0123456789" \0)

(defn letter-or-number? [char]
	(if (or (in? "abcdefghijklmnopqrstuvwxyz" char)
				(in? "0123456789" char))
		true
		false))

(letter-or-number? \+)
(letter-or-number? \*)
(letter-or-number? \/)
(letter-or-number? \a)
(letter-or-number? \1)
(letter-or-number? \0)
(letter-or-number? \))
(letter-or-number? \()


(defn should-add-* [string char]
 	(if (opening-paren? (last string))
 		false
 		(if (and (closing-paren? (last string))
 						 (opening-paren? char))
 			true
	 		(if (and (letter-or-number? (last string))
							 (letter-or-number? char))
	 			true
	 			false))))

(should-add-* "2" \z)
(should-add-* "2z" \+)
(should-add-* "2z+" \1)
(should-add-* "2" "z")
(should-add-* "2z" "+")
(should-add-* "2z+" "1")
(should-add-* "(2z+1)" "(")
(should-add-* "(2z+1)" "z")
(should-add-* "(2z+1)" "*")
(should-add-* "(" "z")



(defn add-*-if-needed [acc b]
	(if (should-add-* acc b)
		(add-to-string acc (str "*" b))
		(add-to-string acc b)))

(add-*-if-needed "2" "z")
(add-*-if-needed "2*z" "+")
(add-*-if-needed "2*z+" "1")

(defn remove-first-char-if-* [eq]
	(if (= \* (first eq))
		(apply str
			(rest eq))
		eq))

(remove-first-char-if-* "*2z+1")
(remove-first-char-if-* "2z+1")

(defn add-*-where-needed [eq]
	(apply str
		(remove-first-char-if-*
			(reduce add-*-if-needed "" (vec eq)))))

(add-*-where-needed "2z+1")
(add-*-where-needed "(2z+1)(2z+1)")

;; end goal of decompose: 
;; sequential list of operations
;; recursive if needed
;; best if not recursive
(defn decompose [eq]
	(-> eq
		add-*-where-needed))


(decompose "2z+1")
(decompose "(2z+1)")
(decompose "(2z+1)(2z+1)")
(decompose "((2z+1)(2z+1))")   ;;excess paren
(decompose "(((2z+1)(2z+1)))") ;;excess paren 2
(decompose "(2z+1)^2(2z+1)")   ;;priority power and paren multiply
(decompose "z+1*2") ;; priority + and *
(decompose "(z+1)*2") ;; priority + and * with paren
(decompose "z-1*2") ;; priority - and *
(decompose "z/2") ;; division not allowed, multiply by real if needed
(decompose "z*0.5") ;; real multiplication
(decompose "-1*z") ;; minus as first symbol, yet legit equation
(decompose "-z") ;; should be same as -1*z
(decompose "+z") ;; plus as first symbol, yet legit equation
(decompose "+z") ;; should be same as +z





;;decompose in leaves-exprs
;;leaf-expr-to-fn
;;compose-leaves-exprs








