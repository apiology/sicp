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

(insert! tbl 3 4 "3-4")
;= ok
(lookup tbl 1 2)
;= "1-2"
(lookup tbl 3 4)
;= "3-4"

;; Exercise 3.25



(define false #f)
(define (make-table)
  (make-table equal?))
(define (make-table same-key?)
  (let ((local-table (list '*table*)))
    (define (assoc key records)
      (display "in assoc, key = ")
      (display key)
      (display " records = ")
      (display records)
      (newline)
      (cond ((null? records) 
             (begin
               (display "assoc returning no answer - false")
               (newline)
               false))
            ((same-key? key (caar records)) 
             (begin
               (display "assoc returning ")
               (display (car records))
               (newline)
               (car records)))
            (else (assoc key (cdr records)))))
    (define (lookup-in subtable key-list)
      (display "lookup-in subtable = ")
      (display subtable)
      (display " key-list = ")
      (display key-list)
      (newline)
      (if (null? (cdr subtable))
          false
          (let* ((key (car key-list))
                 (record (assoc key (cdr subtable))))
            (if (not record)
                false
                (if (null? (cdr key-list))
                    ;; no more keys
                    (cdr record)
                    (lookup-in (cdr record) (cdr key-list)))))))
    (define (lookup key-list)
      (display "lookup key-list = ")
      (display key-list)
      (newline)
      (lookup-in local-table key-list))
    (define (insert-in! subtable key-list value)
      (display "insert-in! subtable = ")
      (display subtable)
      (display " key-list = ")
      (display key-list)
      (display " value = ")
      (display value)
      (newline)
      (let* ((key (car key-list))
             (record-or-subsubtable (assoc key (cdr subtable))))
        (if (null? (cdr key-list))
            ;; no more to go.
            (if record-or-subsubtable ; record
                (begin
                  (display "first set-cdr!")
                  (newline)
                  ;; found an entry with the first key
                  (set-cdr! record-or-subsubtable value)
                  (display "after first set-cdr!, local-table is")
                  (display local-table))
                (begin
                  (display "second set-cdr!")
                  (newline)
                  ;; add key
                  (set-cdr! subtable
                            (cons (cons key value)
                                  (cdr subtable)))
                  (display "after second set-cdr!, local-table is")
                  (display local-table)
                  (newline)
                  ))
            ;; more keys to add
            (if record-or-subsubtable ; subsubtable
                ;; found an existing subsubtable
                (insert-in! record-or-subsubtable (cdr key-list) value)
                ;; need to create one
                (begin
                  (display "third set-cdr!")
                  (newline)
                  (let ((new-subsubtable (list '*table*)))
                    (set-cdr! subtable
                              (cons (cons key new-subsubtable)
                                    (cdr subtable)))
                    (display "after third set-cdr!, local-table is ")
                    (display local-table)
                    (newline)
                    (insert-in! new-subsubtable (cdr key-list) value)))))))
    (define (insert! key-list value)
      (display "insert! key-list: ")
      (display key-list)
      (display "value: ")
      (display value)
      (newline)
      (insert-in! local-table key-list value)
      (display "after insert-in!, local-table is ")
      (display local-table)
      (newline)
      'ok)
  (define (dispatch m)
    (cond ((eq? m 'lookup-proc) lookup)
          ((eq? m 'insert-proc!) insert!)
          (else (error "Unknown operation -- TABLE" m))))
  dispatch))

(define (lookup table key-list)
  ((table 'lookup-proc) key-list))
(define (insert! table key-list value)
  ((table 'insert-proc!) key-list value))

(define tbl (make-table equal?))
(lookup tbl '(1 2))
;= #f
(insert! tbl '(1 2) "1-2")
;= ok
(lookup tbl '(1 2))
;= "1-2"
(insert! tbl '(3 4) "3-4")
;= ok
(lookup tbl '(1 2))
;= "1-2"
(lookup tbl '(3 4))
;= "3-4"


;; Exercise 3.26

;; see paper
