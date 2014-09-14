(define false #f)
(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
    'ok)
  (define (dispatch m)
    (cond ((eq? m 'lookup-proc) lookup)
          ((eq? m 'insert-proc!) insert!)
          (else (error "Unknown operation -- TABLE" m))))
  dispatch))

(define tbl (make-table))
(define (lookup table key-1 key-2)
  ((table 'lookup-proc) key-1 key-2))
(define (insert! table key-1 key-2 value)
  ((table 'insert-proc!) key-1 key-2 value))
(lookup tbl 1 2)
;= #f
(insert! tbl 1 2 "1-2")
;= ok
(lookup tbl 1 2)
;= "1-2"
(insert! tbl 3 4 "3-4")
;= ok
(lookup tbl 1 2)
;= "1-2"
(lookup tbl 3 4)
;= "3-4"

;; Exercise 3.24

(define false #f)
(define (make-table same-key?)
  (let ((local-table (list '*table*)))
    (define (assoc key records)
      (cond ((null? records) false)
            ((same-key? key (caar records)) (car records))
            (else (assoc key (cdr records)))))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
    'ok)
  (define (dispatch m)
    (cond ((eq? m 'lookup-proc) lookup)
          ((eq? m 'insert-proc!) insert!)
          (else (error "Unknown operation -- TABLE" m))))
  dispatch))

(define tbl (make-table equal?))
(define (lookup table key-1 key-2)
  ((table 'lookup-proc) key-1 key-2))
(define (insert! table key-1 key-2 value)
  ((table 'insert-proc!) key-1 key-2 value))
(lookup tbl 1 2)
;= #f
(insert! tbl 1 2 "1-2")
;= ok
(lookup tbl 1 2)
;= "1-2"
(insert! tbl 3 4 "3-4")
;= ok
(lookup tbl 1 2)
;= "1-2"
(lookup tbl 3 4)
;= "3-4"
