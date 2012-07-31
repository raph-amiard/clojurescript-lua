;; IComparable
(extend-protocol IComparable
  Subvec
  (-compare [x y] (compare-indexed x y)))