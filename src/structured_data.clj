(ns structured-data)

(defn do-a-thing [x]
  (let [x2 (+ x x)]
    (Math/pow x2 x2)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[a b c] v]
    (+ a c)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)))

(defn square? [rectangle]
  (= (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
       [px py] point]
     (and (<= x1 px x2 ) (<= y1 py y2)) ))

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] inner]
    (and (contains-point? outer [x1 y1])
         (contains-point? outer [x2 y2]))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [autores (:authors book)]
    (assoc book :authors (conj autores new-author))))


(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map (defn contar[x] (count x)) collection))

(defn second-elements [collection]
  (let [extract (fn [x] (get x 1))]
    (map extract collection)))

(defn titles [books]
  (let [titulo (fn [x] (:title x))]
    (map titulo books)))

(defn monotonic? [a-seq]
 (or (apply >= a-seq)(apply <= a-seq)))


(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not (= (count a-seq)(count (set a-seq)))))

(defn old-book->new-book [book]
  (let [autores (:authors book)]
    (assoc book :authors (set autores))))

(defn has-author? [book author]
  (let [autores (:authors book)]
    (contains? autores author)))

(defn authors [books]
    (let [autores (fn [book] (map :authors books))]
    (apply clojure.set/union (autores books))))


(defn all-author-names [books]
  (let [autores (authors books)
        nombres (fn [autor] (:name autor))]
    (set (map nombres autores))))


(defn author->string [author]
  (let [name (:name author)
        birth-year (:birth-year author)
        death-year (:death-year author)]
  (str name (if (contains? author :birth-year) (str " (" birth-year " - "
              (if (contains? author :death-year) death-year) ")")))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [titulo (:title book)]
    (str titulo ", written by " (authors->string (:authors book)))))


(defn books->string [books]
  (if (> (count books) 0)
  (str (apply str (if (= 1 (count books)) "1 book. " (str (count books) " books. "))
         (interpose ", " (map book->string books))) ".") "No books."))

(defn books-by-author [author books]
(filter (fn [x] (contains? (:authors x) author))  books))

(defn author-by-name [name authors]
  (first (filter (fn [x] (= name (:name x))) authors)))

(defn living-authors [authors]
  (filter (fn [x] (alive? x )) authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter (fn [x] (has-a-living-author? x)) books))

; %________%
