;;; ============================================================
;;;  Employee Management System - Scheme Implementation
;;;  MIT/GNU Scheme 9.2
;;;
;;;  Usage:
;;;    (perform "employees.dat" "print")
;;;    (perform "employees.dat" "print" "ge" 2000)
;;; ============================================================


;;; ============================================================
;;;  EMPLOYEE CONSTRUCTORS & ACCESSORS
;;; ============================================================

;; --- Salaried Employee ---
;; Record: (salaried first-name last-name salary)

(define (make-salaried-employee first last salary)
  (list 'salaried first last salary))

(define (salaried? emp)       (eq? (car emp) 'salaried))
(define (emp-first-name emp)  (cadr emp))
(define (emp-last-name  emp)  (caddr emp))
(define (salaried-salary emp) (cadddr emp))


;; --- Hourly Employee ---
;; Record: (hourly first-name last-name hours hourly-rate)

(define (make-hourly-employee first last hours rate)
  (list 'hourly first last hours rate))

(define (hourly? emp)      (eq? (car emp) 'hourly))
(define (hourly-hours emp) (cadddr emp))
(define (hourly-rate  emp) (car (cddddr emp)))


;; --- Commissioned Employee ---
;; Record: (commission first-name last-name min-pay sales commission-rate)

(define (make-commission-employee first last min-pay sales rate)
  (list 'commission first last min-pay sales rate))

(define (commission? emp)         (eq? (car emp) 'commission))
(define (commission-min-pay  emp) (cadddr emp))
(define (commission-sales    emp) (car  (cddddr emp)))
(define (commission-rate     emp) (cadr (cddddr emp)))


;;; ============================================================
;;;  EARNINGS CALCULATION
;;; ============================================================

(define (get-earnings emp)
  (cond
    ((salaried? emp)
     (salaried-salary emp))

    ((hourly? emp)
     (let ((h (hourly-hours emp))
           (r (hourly-rate  emp)))
       (cond
         ((<= h 40) (* h r))
         ((<= h 50) (+ (* 40 r)
                       (* (* r 1.5) (- h 40))))
         (else      (+ (* 40 r)
                       (* (* r 1.5) 10)
                       (* (* r 2)   (- h 50)))))))

    ((commission? emp)
     (let ((earned (* (commission-sales emp) (commission-rate emp)))
           (min-p  (commission-min-pay emp)))
       (if (< earned min-p) min-p earned)))

    (else (error "Unknown employee type" emp))))


;;; ============================================================
;;;  INFO STRING
;;; ============================================================

(define (get-info emp)
  (cond
    ((salaried? emp)
     (string-append
       "Salaried employee: " (emp-first-name emp) " " (emp-last-name emp) "\n"
       "weekly salary: " (number->string (salaried-salary emp))))

    ((hourly? emp)
     (string-append
       "Hourly employee: " (emp-first-name emp) " " (emp-last-name emp) "\n"
       "hours worked: "  (number->string (hourly-hours emp))
       ", hourly rate: " (number->string (hourly-rate  emp))))

    ((commission? emp)
     (string-append
       "Commission employee: " (emp-first-name emp) " " (emp-last-name emp) "\n"
       "minimum salary: "    (number->string (commission-min-pay emp))
       ", sales amount: "    (number->string (commission-sales   emp))
       ", commission rate: " (number->string (exact->inexact (* (commission-rate emp) 100))) "%"))

    (else (error "Unknown employee type" emp))))


;;; ============================================================
;;;  COMPARISON OPERATOR
;;; ============================================================

(define (check-op value threshold op)
  (cond
    ((string=? op "eq") (= value threshold))
    ((string=? op "ne") (not (= value threshold)))
    ((string=? op "ge") (>= value threshold))
    ((string=? op "le") (<= value threshold))
    ((string=? op "gt") (>  value threshold))
    ((string=? op "lt") (<  value threshold))
    (else #f)))


;;; ============================================================
;;;  STRING UTILITIES
;;; ============================================================

(define (string-split str)
  (let loop ((chars   (string->list str))
             (current '())
             (tokens  '()))
    (cond
      ((null? chars)
       (if (null? current)
           (reverse tokens)
           (reverse (cons (list->string (reverse current)) tokens))))
      ((char-whitespace? (car chars))
       (if (null? current)
           (loop (cdr chars) '() tokens)
           (loop (cdr chars) '() (cons (list->string (reverse current)) tokens))))
      (else
       (loop (cdr chars) (cons (car chars) current) tokens)))))

;; Format a number as a string with exactly two decimal places.
(define (format-dollars n)
  (let* ((rounded   (/ (round (* n 100)) 100))
         (truncated (truncate rounded))
         (frac      (round (* (- rounded truncated) 100)))
         (int-part  (inexact->exact truncated))
         (dec-part  (inexact->exact frac))
         (dec-str   (number->string dec-part)))
    (string-append
      (number->string int-part)
      "."
      (cond ((= dec-part 0)  "00")
            ((< dec-part 10) (string-append "0" dec-str))
            (else dec-str)))))


;;; ============================================================
;;;  FILE READER
;;; ============================================================

(define (read-employees filename)
  (let ((port (open-input-file filename)))
    (let loop ((employees '()))
      (let ((line (read-line port)))
        (if (eof-object? line)
            (begin
              (close-input-port port)
              (reverse employees))
            (let ((fields (string-split line)))
              (if (null? fields)
                  (loop employees)
                  (let ((type (car fields)))
                    (cond
                      ((and (string=? type "salaried") (= (length fields) 4))
                       (loop (cons (make-salaried-employee
                                     (list-ref fields 1)
                                     (list-ref fields 2)
                                     (string->number (list-ref fields 3)))
                                   employees)))
                      ((and (string=? type "hourly") (= (length fields) 5))
                       (loop (cons (make-hourly-employee
                                     (list-ref fields 1)
                                     (list-ref fields 2)
                                     (string->number (list-ref fields 3))
                                     (string->number (list-ref fields 4)))
                                   employees)))
                      ((and (string=? type "commission") (= (length fields) 6))
                       (loop (cons (make-commission-employee
                                     (list-ref fields 1)
                                     (list-ref fields 2)
                                     (string->number (list-ref fields 3))
                                     (string->number (list-ref fields 4))
                                     (string->number (list-ref fields 5)))
                                   employees)))
                      (else
                       (loop employees)))))))))))


;;; ============================================================
;;;  ACTIONS
;;; ============================================================

(define (filter-employees employees threshold op)
  (filter (lambda (e) (check-op (get-earnings e) threshold op))
          employees))

(define (action-print employees threshold op)
  (for-each
    (lambda (emp)
      (if (check-op (get-earnings emp) threshold op)
          (begin
            (display (get-info emp))
            (newline)
            (display "earned $")
            (display (format-dollars (get-earnings emp)))
            (newline)
            (newline))
          #f))
    employees))

(define (action-count employees threshold op)
  (length (filter-employees employees threshold op)))

(define (action-min employees threshold op)
  (let* ((matching (filter-employees employees threshold op))
         (min-val  (apply min (map get-earnings matching))))
    (action-print employees min-val "eq")))

(define (action-max employees threshold op)
  (let* ((matching (filter-employees employees threshold op))
         (max-val  (apply max (map get-earnings matching))))
    (action-print employees max-val "eq")))

(define (action-total employees threshold op)
  (let ((total (apply + (map get-earnings (filter-employees employees threshold op)))))
    (display "Total payment is $")
    (display (format-dollars total))
    (newline)))

(define (action-avg employees threshold op)
  (let* ((matching (filter-employees employees threshold op))
         (cnt      (length matching))
         (avg      (/ (apply + (map get-earnings matching)) cnt)))
    (display "Average payment per employee is $")
    (display (format-dollars avg))
    (newline)))


;;; ============================================================
;;;  VALIDATION HELPERS
;;; ============================================================

(define (valid-op? op)
  (or (string=? op "eq") (string=? op "ne") (string=? op "ge")
      (string=? op "le") (string=? op "gt") (string=? op "lt")))

(define (valid-action? action)
  (or (string=? action "print") (string=? action "count") (string=? action "min")
      (string=? action "max")   (string=? action "total") (string=? action "avg")))


;;; ============================================================
;;;  PERFORM  — main entry point
;;;
;;;    (perform filename action)
;;;    (perform filename action op threshold)
;;; ============================================================

(define (perform filename action . rest)
  (cond
    ;; Validate argument count
    ((and (not (null? rest)) (not (= (length rest) 2)))
     (display "Usage: (perform filename action) or (perform filename action op threshold)")
     (newline))

    ;; Validate action
    ((not (valid-action? action))
     (display "Invalid action: ") (display action) (newline)
     (display "Valid actions: print count min max total avg") (newline))

    ;; Validate op when provided
    ((and (not (null? rest)) (not (valid-op? (car rest))))
     (display "Invalid operator: ") (display (car rest)) (newline)
     (display "Valid operators: eq ne gt ge lt le") (newline))

    ;; Validate threshold is a number when provided
    ((and (not (null? rest)) (not (number? (cadr rest))))
     (display "Invalid threshold: ") (display (cadr rest))
     (display " (must be a number)") (newline))

    ;; All inputs valid — run the action
    (else
     (let* ((op        (if (null? rest) "ge" (car rest)))
            (threshold (if (null? rest) 0    (cadr rest)))
            (employees (read-employees filename)))
       (cond
         ((null? employees)
          (display "There are no employees.") (newline))

         ((= (action-count employees threshold op) 0)
          (display "There are no employees that satisfied the specified condition.") (newline))

         ((string=? action "count")
          (display "There are ")
          (display (action-count employees threshold op))
          (display " employees.")
          (newline))

         ((string=? action "print")
          (action-print employees threshold op))

         ((string=? action "min")
          (action-min employees threshold op))

         ((string=? action "max")
          (action-max employees threshold op))

         ((string=? action "total")
          (action-total employees threshold op))

         ((string=? action "avg")
          (action-avg employees threshold op)))))))