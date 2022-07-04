(ns fractalis.views
  (:require
   [re-frame.core :as re-frame]
   [fractalis.subs :as subs]
   [reagent.core :as r]
   ["recharts" :as recharts]
   [fractalis.fractal :as fractal]))
  

(def delta 0.2)
(def points-to-check-data
	(conj (fractal/points-to-check-data delta)
				{:x -5 :y -5}
				{:x 5 :y 5}))

(def mandelbrot-set-data
	(conj (fractal/mandelbrot-set-data delta)
				{:x -5 :y -5}
				{:x 5 :y 5}))

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

(def mandelbrot-data 
	(fractal/mandelbrot-data c-1-1 10))

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



(defn fractal-scatter-chart []
	[:> recharts/ScatterChart
		{:width 730
		 :height 250}
		[:> recharts/CartesianGrid {:strokeDasharray "3 3"}]
		[:> recharts/XAxis {:type "number" :dataKey "real"}]
		[:> recharts/YAxis {:type "number" :dataKey "imaginary"}]
		[:> recharts/Tooltip {:cursor {:strokeDasharray "3 3"}}]
		[:> recharts/Scatter {:name "A school" :data mandelbrot-data :fill "#8884d8"}]
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


(defn main-panel []
  (let [name (re-frame/subscribe [::subs/name])
  			data [["A" 250] ["B" 500]]]
    [:div
     ; [:h1
     ;  "Hello from " @name " 4"]
     [:h1.text-blue-500.text-3xl.font-bold "Fractalis"]
     [:div.flex
	     [points-to-check-scatter-chart]
	     [:div.px-8]
	     [mandelbrot-set-scatter-chart]
	   ]
	   [:div.py-16]
     [fractal-scatter-chart]
     (for [x (range 0 (count mandelbrot-data))]
     	 [:div (str (nth (map fractal/write-complex mandelbrot-data) x))])
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