(define-module (gds utils)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (ice-9 vlist)
  #:export (levenshtein-distance
            find-similar-strings
            alist-add
            alist-add-no-duplicates))

(define (levenshtein-distance s1 s2)
  ;; Implementation using dynamic programming, which can be visualised
  ;; through a matrix. s1 is the string along the vertical axis, and
  ;; s2 along the horizontal axis. For example, with s1 as sitting and
  ;; s2 as kitten:
  ;;
  ;;                  (s2)
  ;;            k  i  t  t  e  n
  ;;         0  1  2  3  4  5  6
  ;;      s  1 |1| 2  3  4  5  6
  ;;      i  2  2 |1| 2  3  4  5
  ;;      t  3  3  2 |1| 2  3  4
  ;; (s1) t  4  4  3  2 |1| 2  3
  ;;      i  5  5  4  3  2 |2| 3
  ;;      n  6  6  5  4  3  3 |2|
  ;;      g  7  7  6  5  4  4 |3|
  ;;
  ;; Each row represents values taken by the previousRow and
  ;; currentRow variables. Note that the rows are longer than the
  ;; strings. The lowest cost for each row is indicated in | |. The
  ;; operation can be decoded by looking at the position and cost
  ;; relative to the lowest cost value in the previous row.
  (define (vlist-last vlist)
    (vlist-ref vlist
               (- (vlist-length vlist) 1)))

  ;; So that the rows of the matrix are shorter, reverse s1 and s2 if
  ;; s2 (on the horizontal axis) is longer than s2 (on the vertical
  ;; axis).
  (if (> (string-length s2) (string-length s1))
      (levenshtein-distance s2 s1)

      ;; The distance is the value in the bottom right of the matrix,
      ;; which in this computation is the last entry in the final row
      (vlist-last
       (fold
        (lambda (i1 c1 previousRow)
          ;; Compute the costs for the current row, this is assembled
          ;; using vlist-cons, so as you go to the right across the
          ;; row, the costs are being put at the start the
          ;; list. Therefore, reverse it at the end to order it
          ;; correctly.
          (vlist-reverse
           (fold
            (lambda (i2 c2 currentRow)
              (vlist-cons
               (if (eq? c1 c2)
                   ;; If the characters match, the cost is equal to
                   ;; that in the previous row for the position of the
                   ;; matching character
                   (vlist-ref previousRow i2)

                   ;; Characters don't match, so the cost is one
                   ;; operation, plus the minimum cost up to this
                   ;; point.
                   (+ 1
                      (min
                       ;; The cost of deletion is 1, plus the cost
                       ;; associated with the previous entry the
                       ;; current row
                       (vlist-head currentRow)

                       ;; The cost of insertion is 1, plus the cost
                       ;; associated with the same position in the
                       ;; previous row
                       (vlist-ref previousRow (+ i2 1))

                       ;; The cost of substitution is 1, plus the cost
                       ;; associated with the character before this
                       ;; one in the previous row
                       (vlist-ref previousRow i2))))
               currentRow))
            (vlist-cons (+ i1 1) vlist-null)
            (iota (string-length s2))
            (string->list s2))))

        ;; Initialise the previousRow. This list represents the cost
        ;; of transforming an empty string to a part of s2, so the
        ;; value at index i is the cost to get a string of length i.
        (list->vlist (iota (+ 1 (string-length s2))))

        (iota (string-length s1))
        (string->list s1)))))

(define* (find-similar-strings input strings
                               #:optional #:key
                               (distance-threshold 5))
  (define sort-pairs
    (match-lambda*
      (((d1 . s1) (d2 . s2))
       (> d1 d2))))

  (map cdr
       (stable-sort (filter-map
                     (lambda (string)
                       (let ((distance
                              (levenshtein-distance string input)))
                         (if (< distance distance-threshold)
                             (cons distance string)
                             #f)))
                     strings)
                    sort-pairs)))

(define (alist-add key value alist)
  (if (null? alist)
      (list (cons key (list value)))
      (if (equal? (caar alist) key)
          (cons (cons key
                      (cons value
                            (cdr (first alist))))
                (cdr alist))
          (cons (car alist)
                (alist-add key value (cdr alist))))))

(define* (alist-add-no-duplicates key value alist #:optional (= equal?))
  (if (null? alist)
      (list (cons key (list value)))
      (if (equal? (caar alist) key)
          (cons (cons key
                      (delete-duplicates
                       (cons value
                             (cdr (first alist)))
                       =))
                (cdr alist))
          (cons (car alist)
                (alist-add key value (cdr alist))))))
