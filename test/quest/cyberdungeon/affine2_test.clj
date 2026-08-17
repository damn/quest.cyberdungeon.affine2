(ns quest.cyberdungeon.affine2-test
  "What Affine2 transforms: 2D points (e.g. actor-local → parent/stage).

  Functions:
  - `blank` — identity
  - `trn-rot-scl` — build T·R·S (place + scale + optional rotate)
  - `translate` — nudge in the transform's local axes
  - `pre-mul` — compose with a parent (`other * a`)
  - `transform-point` — apply matrix to a point"
  (:require [clojure.test :refer [deftest is]]
            [quest.cyberdungeon.affine2 :as affine2]))

(def ^:private eps 1.0e-5)

(defn- nearly?
  [[x1 y1] [x2 y2]]
  (and (<= (Math/abs (- (double x1) (double x2))) eps)
       (<= (Math/abs (- (double y1) (double y2))) eps)))

(deftest blank-is-identity
  (let [a (affine2/blank)]
    (is (nearly? [3.0 4.0] (affine2/transform-point a 3 4)))))

(deftest trn-rot-scl-translate-and-scale
  (let [a (affine2/trn-rot-scl 10 20 0 2 3)]
    ;; local (1,1) → scaled then translated: (10,20) + (2,3) = (12,23)
    (is (nearly? [12.0 23.0] (affine2/transform-point a 1 1)))
    (is (nearly? [10.0 20.0] (affine2/transform-point a 0 0)))))

(deftest trn-rot-scl-90-degrees
  (let [a (affine2/trn-rot-scl 0 0 90 1 1)]
    ;; 90° CCW: (1,0) → (0,1)
    (is (nearly? [0.0 1.0] (affine2/transform-point a 1 0)))))

(deftest translate-moves-origin
  (let [a (-> (affine2/blank)
              (affine2/translate 5 7))]
    (is (nearly? [5.0 7.0] (affine2/transform-point a 0 0)))
    (is (nearly? [6.0 8.0] (affine2/transform-point a 1 1)))))

(deftest pre-mul-applies-child-then-parent
  ;; child: scale 2; parent: translate (10,0)
  ;; point in child local (1,0) → child (2,0) → parent (12,0)
  (let [child (affine2/trn-rot-scl 0 0 0 2 2)
        parent (affine2/trn-rot-scl 10 0 0 1 1)
        world (affine2/pre-mul child parent)]
    (is (nearly? [12.0 0.0] (affine2/transform-point world 1 0)))))
