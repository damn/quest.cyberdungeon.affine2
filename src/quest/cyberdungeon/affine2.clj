(ns quest.cyberdungeon.affine2
  "2D affine transform as a plain map of six floats
  (`:m00` `:m01` `:m02` `:m10` `:m11` `:m12`).

  Linear part + translation; see
  https://en.wikipedia.org/wiki/Affine_transformation

  Pure values — callers choose storage (actor attr, atom, local binding).")

(defn blank
  "Identity transform."
  []
  {:m00 (float 1) :m01 (float 0) :m02 (float 0)
   :m10 (float 0) :m11 (float 1) :m12 (float 0)})

(defn pre-mul
  "Premultiply: returns `other * a`."
  [a other]
  (let [tmp00 (+ (* (:m00 other) (:m00 a)) (* (:m01 other) (:m10 a)))
        tmp01 (+ (* (:m00 other) (:m01 a)) (* (:m01 other) (:m11 a)))
        tmp02 (+ (* (:m00 other) (:m02 a)) (* (:m01 other) (:m12 a)) (:m02 other))
        tmp10 (+ (* (:m10 other) (:m00 a)) (* (:m11 other) (:m10 a)))
        tmp11 (+ (* (:m10 other) (:m01 a)) (* (:m11 other) (:m11 a)))
        tmp12 (+ (* (:m10 other) (:m02 a)) (* (:m11 other) (:m12 a)) (:m12 other))]
    {:m00 (float tmp00) :m01 (float tmp01) :m02 (float tmp02)
     :m10 (float tmp10) :m11 (float tmp11) :m12 (float tmp12)}))

(defn trn-rot-scl
  "Translate + rotate (degrees) + scale."
  [x y degrees scale-x scale-y]
  (let [x (float x) y (float y)
        scale-x (float scale-x) scale-y (float scale-y)]
    (if (zero? (float degrees))
      {:m02 x :m12 y
       :m00 scale-x :m01 (float 0)
       :m10 (float 0) :m11 scale-y}
      (let [sin (float (Math/sin (Math/toRadians (double degrees))))
            cos (float (Math/cos (Math/toRadians (double degrees))))]
        {:m02 x :m12 y
         :m00 (float (* cos scale-x))
         :m01 (float (* (- sin) scale-y))
         :m10 (float (* sin scale-x))
         :m11 (float (* cos scale-y))}))))

(defn translate
  "Postmultiply by a translation."
  [a x y]
  (let [x (float x) y (float y)]
    (assoc a
           :m02 (float (+ (:m02 a) (+ (* (:m00 a) x) (* (:m01 a) y))))
           :m12 (float (+ (:m12 a) (+ (* (:m10 a) x) (* (:m11 a) y)))))))

(defn transform-point
  "Apply affine `a` to point `[x y]` → `[x' y']` (local → transformed space)."
  [a x y]
  (let [x (float x) y (float y)]
    [(float (+ (* (:m00 a) x) (* (:m01 a) y) (:m02 a)))
     (float (+ (* (:m10 a) x) (* (:m11 a) y) (:m12 a)))]))
