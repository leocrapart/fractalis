(ns fractalis.views
  (:require
   [re-frame.core :as re-frame]
   [fractalis.subs :as subs]
   [reagent.core :as r]
   ["recharts" :as recharts]
   [fractalis.fractal :as fractal]))
  

;; converters
(defn point-data [point]
	{:x (nth point 0)
	 :y (nth point 1)})

(defn x-y-data [points]
	(vec (map point-data points)))



(def delta 0.2)
(def points-to-check-data
	(conj (x-y-data (fractal/points-to-check delta))
				{:x -5 :y -5}
				{:x 5 :y 5}))

(def mandelbrot-set-data
	(conj (x-y-data (fractal/mandelbrot-set delta))
				{:x -5 :y -5}
				{:x 5 :y 5}))


; (defn points-to-check-data [delta]
; 	(let [points (points-to-check delta)]
; 		(vec (map point-data points))))

; (defn mandelbrot-set-data [delta]
; 	(let [points (mandelbrot-set delta)]
; 		(vec (map point-data points))))


;; idea
;; get the complex function as input 
;; and draw the fractal graph 
;; asynchronously, to avoid ressource overload
;; with good colors (artistic)
;; user can also modify the precision (distance between points, and refresh rate)




(def data
  [{:name "Page A" :uv 300 :pv 2600 :amt 3400}
   {:name "Page B" :uv 400 :pv 4367 :amt 6400}
   {:name "Page C" :uv 300 :pv 1398 :amt 2400}
   {:name "Page D" :uv 200 :pv 9800 :amt 2400}
   {:name "Page E" :uv 278 :pv 3908 :amt 2400}
   {:name "Page F" :uv 189 :pv 4800 :amt 2400}
   {:name "Page G" :uv 189 :pv 4800 :amt 2400}])
  


(def data2
  [{:x 100 :y 200 :z 200}
   {:x 110 :y 280 :z 200}
   {:x 120 :y 100 :z 260}
   {:x 140 :y 250 :z 280}
   {:x 150 :y 400 :z 500}
   {:x 170 :y 300 :z 400}])
   

(def data3
	[{:x 200 :y 260 :z 240}
	 {:x 180 :y 280 :z 260}
	 {:x 190 :y 290 :z 250}
	 {:x 198 :y 250 :z 210}
	 {:x 210 :y 220 :z 230}
	 {:x 240 :y 290 :z 220}
	 ])

data2

(def c11
  {:real 1
   :imaginary 1})

(def c22
  {:real 2
   :imaginary 2})

(def c-1-1
	{:real 0.1
   :imaginary 0.1})

(defn scatter-chart []
	[:> recharts/ScatterChart
		{:width 730
		 :height 250}
		[:> recharts/CartesianGrid {:strokeDasharray "3 3"}]
		[:> recharts/XAxis {:type "number" :dataKey "x" :name "stature" :unit "cm"}]
		[:> recharts/YAxis {:type "number" :dataKey "y" :name "weight" :unit "kg"}]
		[:> recharts/ZAxis {:dataKey "Z" :range [64, 144] :name "score" :unit "km"}]
		[:> recharts/Tooltip {:cursor {:strokeDasharray "3 3"}}]
		[:> recharts/Legend]
		[:> recharts/Scatter {:name "A school" :data data2 :fill "#8884d8"}]
		[:> recharts/Scatter {:name "B school" :data data3 :fill "#82ca9d"}]
	])


(defn points-to-check-scatter-chart []
	[:> recharts/ScatterChart
		{:width 500
		 :height 500}
		[:> recharts/CartesianGrid {:strokeDasharray "3 3"}]
		[:> recharts/XAxis {:type "number" :dataKey "x"}]
		[:> recharts/YAxis {:type "number" :dataKey "y"}]
		[:> recharts/Tooltip {:cursor {:strokeDasharray "3 3"}}]
		[:> recharts/Scatter {:name "Points to check" :data points-to-check-data :fill "#8884d8"}]
	])

(defn mandelbrot-set-scatter-chart []
	[:> recharts/ScatterChart
		{:width 500
		 :height 500}
		[:> recharts/CartesianGrid {:strokeDasharray "3 3"}]
		[:> recharts/XAxis {:type "number" :dataKey "x"}]
		[:> recharts/YAxis {:type "number" :dataKey "y"}]
		[:> recharts/Tooltip {:cursor {:strokeDasharray "3 3"}}]
		[:> recharts/Scatter {:name "Mandelbrot set" :data mandelbrot-set-data :fill "#8884d8"}]
	])

(defn get-real-size [el]
  (let [bb (.getBoundingClientRect el)]
    [(.-width bb) (.-height bb)]))

;; real-size canvas with render function
(defn Canvas [{:keys [width height render]}]
  (let [state (atom nil)]
    (r/create-class
      {:reagent-render	(fn []
                          (let [update-size (fn [el]
                                              (when el
                                                  (let [size (get-real-size el)
                                                        ctx (.getContext el "2d")]
                                                      (swap! state assoc :size size)
                                                      (render ctx size))))]
                            (fn [] (let [{:keys [size]} @state]
                                       [:canvas {:style  {:width width :height height}
                                                 :ref    update-size
                                                 :width  (nth size 0)
                                                 :height (nth size 1)}]))))
       :component-did-mount (fn [] (reset! state {:size nil}))
       })))

(defn draw-pixel [ctx point]
	(.fillRect ctx (point 0) (point 1) 1 1))

(defn draw-pixels [ctx points]
	(doseq [point points]
		(draw-pixel ctx point)))

(defn draw-cube [ctx point]
	(.fillRect ctx (point 0) (point 1) 6 6))

(defn draw-cubes-at [ctx points]
	(doseq [point points]
		(draw-cube ctx point)))







(defn x-scale-factor [min-width max-width]
	(float (/ (- max-width
							 min-width)
				    4)))

(defn y-scale-factor [min-height max-height]
	(float (/ (- max-height
							 min-height)
				    4)))

(defn scaled-x [x dimensions]
	(let [min-width (dimensions :min-width)
				max-width (dimensions :max-width)
				x-scale-factor (x-scale-factor min-width max-width)
				semi-width (/ (- max-width min-width) 2)]
		(+ semi-width 
			 (* x x-scale-factor))))

(defn scaled-y [y dimensions]
	(let [min-height (dimensions :min-height)
				max-height (dimensions :max-height)
				y-scale-factor (y-scale-factor min-height max-height)
				semi-height (/ (- max-height min-height) 2)]
		(+ semi-height 
			 (* y y-scale-factor))))

(defn scaled-point [point dimensions]
	[(scaled-x (point 0) dimensions)
	 (scaled-y (point 1) dimensions)])



(defn scaled-mandelbrot-set [delta dimensions]
	(let [mandelbrot-set (fractal/mandelbrot-set delta)]
		(map (fn [point] (scaled-point point dimensions))
				 mandelbrot-set)))



(comment 
	(def dimensions {
		:min-width 0
		:max-width 500
		:min-height 0
		:max-height 200
		})
	(scaled-x -3 dimensions)  ; = min-width
	(scaled-x -2 dimensions) 
	(scaled-x 0 dimensions) ; = (max-min)/2
	(scaled-x 3 dimensions) ; = max-width

	(scaled-point [0 0] dimensions) ; mid mid = 250 100
	(scaled-point [-3 3] dimensions)  ; min max = 0 200
	(scaled-point [-3 -3] dimensions) ; min min = 0 0
	(scaled-point [3 3] dimensions)  ; max max = 500 200

	(fractal/mandelbrot-set 0.2)
	(scaled-mandelbrot-set 0.2 dimensions)
	)





;; 3 by 3 square
;; min width <=> -3       *width /-3
;; (max - min)/2 <=> 0
;; max width <=> 3

;; min height <=> -3
;; (max - min)/2 <=> 0
;; max height <=> 3


(defn draw-mandelbrot [el]
	(let [ctx (.getContext el "2d")
				dimensions {
					:min-width 0  :max-width 500
					:min-height 0 :max-height 200
					}
				scaled-mandelbrot-set (scaled-mandelbrot-set 0.5 dimensions) ]
		; (draw-pixel ctx [0 0])
		; (draw-pixel ctx [10 0])
		; (draw-pixel ctx [20 0])
		; (draw-pixel ctx [30 0])

		; (draw-pixels ctx [[0 50] [10 50] [20 50] [30 50]])

		; (draw-cube ctx [100 100])
		; (draw-cube ctx [200 100])
		; (draw-cube ctx [300 100])
		; (draw-cube ctx [400 100])
		; (draw-cube ctx [500 100])
		(draw-cube ctx [0 0])
		(draw-cube ctx [0 200])

		(draw-cube ctx [250 100])

		(draw-cube ctx [500 0])
		(draw-cube ctx [500 200])


		(draw-pixels ctx scaled-mandelbrot-set)
		))

(defn real-size-canvas [])

(defn custom-canvas []
	[:canvas {:style {:width 500 :height 200}
					 	:ref draw-mandelbrot
					 }
	]
)

(def dimensions {
		:min-width 0
		:max-width 500
		:min-height 0
		:max-height 100
		})

(defn draw-frame [ctx dimensions]
	(let [min-width (dimensions :min-width)
				min-height (dimensions :min-height)
				max-width (dimensions :max-width)
				max-height (dimensions :max-height)
				mid-width (/ (- max-width min-width) 2)
				mid-height (/ (- max-height min-height) 2)
				]
		(draw-cube ctx [min-width min-height])
		(draw-cube ctx [mid-width min-height])
		(draw-cube ctx [max-width min-height])

		(draw-cube ctx [min-width mid-height])
		(draw-cube ctx [mid-width mid-height])
		(draw-cube ctx [max-width mid-height])

		(draw-cube ctx [min-width max-height])
		(draw-cube ctx [mid-width max-height])
		(draw-cube ctx [max-width max-height])
	))

(defn main-panel []
  (let [name (re-frame/subscribe [::subs/name])
  			data [["A" 250] ["B" 500]]]
    [:div
     ; [:h1
     ;  "Hello from " @name " 4"]
     [:h1.text-blue-500.text-3xl.font-bold "Fractalis"]

     [:div "Mandelbrot fractal"]
	   ; [custom-canvas]
	   (let [dimensions {:min-width 0
											:max-width 400
											:min-height 0
											:max-height 400}]
		   [:canvas#canvas-1 {:width (dimensions :max-width)
		   					 :height (dimensions :max-height)	   	         
		   					 :ref (fn [el]
			        					(let [ctx (.getContext el "2d")]
			        						(draw-pixels ctx (scaled-mandelbrot-set 0.04 dimensions))))}])


	   ; [:div "end custom canvas"]	


     [:div.flex
	     [points-to-check-scatter-chart]
	     [:div.px-8]
	     [mandelbrot-set-scatter-chart]
	   ]
	   
	   [:div "custom canva"]
	   ; [custom-canvas]
	   [:div#canvas-2 ""]
	   [Canvas {:width "100%"
			        :height "100%"
			        :render (fn [ctx [w h]]
			        					; (scaled-mandelbrot-set 0.5 dimensions)
			             ;      ; (.fillRect ctx 0 0 100 100)
			             ;      (draw-pixel ctx [250 100])
			             ;      (draw-pixels ctx (scaled-mandelbrot-set 0.5 dimensions))
			             			(draw-frame ctx dimensions)
			                  )}]
	   ; [:div "end custom canvas"]
	   
     ]))


;; next steps :

;; create mandelbrot fractal data (n c)
; (def mandelbrot-data
; 	[
; 	 {:n 0 :real 0 :imaginary 0}
; 	 {:n 1 :real 1 :imaginary 2}
; 	 {:n 2 :real 4 :imaginary 1}
; 	 {:n 3 :real 3 :imaginary 6}
; 	 {:n 4 :real 9 :imaginary 12}
; 	 ])

; where colors come from ?


; re-frame : 
; - user can select n
; - user can select c