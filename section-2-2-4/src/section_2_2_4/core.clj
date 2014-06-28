(ns section-2-2-4.core)

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

;; beside - Takes two painters and draws first painter's image in left
;; half of frame and second painter's in right half.

;; below - Takes two painters and produces a compound painter that
;; draws the first painter's image below the second painter's image.

;; flip-vert takes a painter and produces a painter that draws its
;; image upside-down.

;; flip-horiz produces a painter that draws the original painter's
;; image left-to-right reversed.

(def wave2 (beside wave (flip-vert wave)))

;(def wave4 (below wave2 wave2))

(defn flipped-pairs [painter]
  "Pattern in wave4 abstracted"
  (let [painter2 (beside painter (flip-vert painter))]
    (below painter2 painter2)))

(def wave4 (flipped-pairs wave))

(defn right-split [painter n]
  (if (zero? n)
    painter
    (let [smaller (right-split painter (dec n))]
      (beside painter (below smaller smaller)))))

(defn corner-split [painter n]
  (if (zero? n)
    painter
    (let [up (up-split painter (dec n))
          right (right-split painter (dec n))
          top-left (beside up up)
          bottom-right (below right right)
          corner (corner-split painter (dec n))]
      (beside (below painter top-left)
              (below bottom-right corner)))))

(defn square-limit [painter n]
  (let [quarter (corner-split painter n)
        half (beside (flip-horiz quarter) quarter)]
    (below (flip-vert half) half)))

(defn up-split [painter n]
  (if (zero? n)
    painter
    (let [smaller (up-split painter (dec n))]
      (below painter (beside smaller smaller)))))


(defn square-of-four [tl tr bl br]
  "tl tr bl br are the transformations to apply to the top left copy,
  the bottom left copy, and the bottom right copy."
  (fn [painter]
    (let [top (beside (tl painter) (tr painter))
          bottom (beside (bl painter) (br painter))]
      (below bottom top))))

(defn flipped-pairs [painter]
  (let [combine4 (square-of-four identity flip-vert
                                 identity flip-vert)]
    (combine4 painter)))

(defn square-limit [painter n]
  (let [combine4 (square-of-four flip-horiz identity
                                 rotate180 flip-vert)]
    (combine4 (corner-split painter n))))

;; Exercise 2.45

(defn right-split [painter n]
  (if (zero? n)
    painter
    (let [smaller (right-split painter (dec n))]
      (beside painter (below smaller smaller)))))

(defn up-split [painter n]
  (if (zero? n)
    painter
    (let [smaller (up-split painter (dec n))]
      (below painter (beside smaller smaller)))))

(defn split [f1 f2]
  (fn my-split [painter n]
    (if (zero? n)
      painter
      (let [smaller (my-split painter (dec n))]
        (f1 painter (f2 smaller smaller))))))

(def right-split (split beside below))
(def up-split (split below beside))

(defn make-frame
  "Create a new frame, which is three points/vectors--origin and two
   edge points.  Thus, this is an arbitrary parallelogram"
  [origin edge1 edge2]
  ...)

(defn origin-frame [frame] ...)

(defn edge1-frame [frame] ...)

(defn edge2-frame [frame] ...)

;; images are defined in the unit square (0 <= y, y <= 1)

;; frame coordinate map goes from unit square to the frame by mapping
;; v=(x, y) to vector sum Origin(frame) + x . edge1(frame) + y . edge2(frame)

(defn frame-coord-map [frame]
  (fn [v]
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))

;; note that:
((frame-coord-map a-frame) (make-vect 0 0))
;; always returns same vector as:
(origin-frame a-frame)

;; Exercise 2.46

(def make-vect cons)
(def xcor-vect first)
(def ycor-vect second)

(defn add-vect [v1 v2]
  (make-vect (+ (xcor-vect v1)
                (xcor-vect v1))
             (+ (ycor-vect v1)
                (xcor-vect v2))))

(defn add-vect [v1 v2]
  (make-vect (- (xcor-vect v1)
                (xcor-vect v1))
             (- (ycor-vect v1)
                (xcor-vect v2))))

(defn scale-vect [v scale]
  (make-vect (* (xcor-vect v)
                scale)
             (* (ycor-vect v)
                scale)))

;; Exercise 2.47

(defn make-frame [origin edge1 edge2]
  (list origin edge1 edge2))

(def origin-frame first)

(def edge1-frame second)

(def edge2-frame third)

(defn make-frame [origin edge1 edge2]
  (cons origin (cons edge1 edge2)))

(def origin-frame first)

(def edge1-frame ffirst)

(def edge2-frame sfirst)

;; Painters

;; If p is a painter and f is a frame, we produce p's image in f by
;; calling p with f as an argument

; (painter frame) ;=> image

(defn segments->painter [segment-list]
  (fn [frame]
    (doall
     (map
      (fn [segment]
        (draw-line
         ((frame-coord-map frame) (start-segment segment)
          (frame-coord-map frame) (end-segment segment))))
      segment-list))))

;; Exercise 2.48

(defn make-segment [v1 v2]
  (list v1 v2))

(def start-segment first)

(def end-segment second)

;; Exercise 2.49


;; a
(defn outline-of-frame-painter [frame]
  (let [vec-1 (origin-frame frame)
        vec-2 (edge1-frame frame)
        vec-3 (edge2-frame frame)
        vec-4 (add-vect vec-1 vec-2)
        s1 (make-segment vec-1 vec-2)
        s2 (make-segment vec-2 vec-3)
        s3 (make-segment vec-3 vec-4)
        s4 (make-segment vec-4 vec-1))
  (segments->painter (list s1 s2 s3 s4))))

(defn x-of-frame-painter [frame]
  (let [vec-1 (origin-frame frame)
        vec-2 (edge1-frame frame)
        vec-3 (edge2-frame frame)
        vec-4 (add-vect vec-1 vec-2)
        s1 (make-segment vec-1 vec-3)
        s2 (make-segment vec-2 vec-4)]
  (segments->painter (list s1 s2))))

(defn midpoint-vecs [v1 v2]
  (scale-vect (add-vect v1 v2) 0.5))

(defn diamond-shape [frame]
  (let [vec-1 (origin-frame frame)
        vec-2 (edge1-frame frame)
        vec-3 (edge2-frame frame)
        vec-4 (add-vect vec-1 vec-2)
        mid-1 (midpoint-vecs [vec1 vec2])
        mid-2 (midpoint-vecs [vec2 vec3])
        mid-3 (midpoint-vecs [vec3 vec4])
        mid-4 (midpoint-vecs [vec4 vec1])
        s1 (make-segment mid-1 mid-2)
        s2 (make-segment mid-2 mid-3)
        s3 (make-segment mid-3 mid-4)
        s4 (make-segment mid-4 mid-1))
  (segments->painter (list s1 s2 s3 s4))))

;; wave painter


(defn transform-painter [painter origin corner1 corner2]
  "arguments are points (represented as vectors) that specify corner
   of new frame"
  (fn [frame]
    (let [m (frame-coord-map frame)
          new-origin (m origin)]
      (painter
       (make-frame new-origin
                   (sub-vect (m corner1) new-origin)
                   (sub-vect (m corner2) new-origin))))))

(defn flip-vert [painter]
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(defn shrink-to-upper-right [painter]
  (transform-painter painter
                     (make-vect 0.5 0.5)
                     (make-vect 1.0 0.5)
                     (make-vect 0.5 1.0)))

(defn rotate90 [painter]
  (transform-painter painter
                     (make-vect 1.0 0.0) ;; new origin
                     (make-vect 1.0 1.0) ;; new edge 1
                     (make-vect 0.0 0.0))) ;; new edge 2

(defn squash-inwards [painter]
  (transform-painter painter
                     (make-vect 0.0 0.0)
                     (make-vect 0.65 0.35)
                     (make-vect 0.35 0.65)))

(defn beside [painter1 painter2]
  (let [split-point (make-vect 0.5 0.0)
        paint-left (transform-painter painter1
                                      (make-vect 0.0 0.0)
                                      split-point
                                      (make-vect 0.0 1.0))
        paint-right (transform-painter painter2
                                       split-point
                                       (make-vect 1.0 0.0)
                                       (make-vect 0.5 1.0))]
    (fn [frame]
      (paint-left frame)
      (paint-right frame))))

(defn flip-horiz [painter]
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(defn rotate-180 [painter]
  (transform-painter painter
                     (make-vect 1.0 1.0)
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 1.0)))


(defn rotate-270 [painter]
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

;; Exercise 2.51

(defn below [painter1 painter2]
  (let [split-point (make-vect 0.0 0.5)
        paint-top (transform-painter painter1
                                     split-point
                                     (make-vect 1.0 0.5)
                                     (make-vect 0.0 1.0))
        paint-bottom (transform-painter painter2
                                        (make-vect 0.0 0.0)
                                        (make-vect 1.0 0.0)
                                        split-point)]
    (fn [frame]
      (paint-top frame)
      (paint-bottom frame))))

(defn below [painter1 painter2]
  (rotate-270 (beside (rotate-90 painter1) (rotate-90 painter2))))


