
;;; ============================================================
;;;  Employee Management System - Scheme Implementation
;;;  Compatible with MIT/GNU Scheme 9.2
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

(define (hourly? emp)         (eq? (car emp) 'hourly))
(define (hourly-hours emp)    (cadddr emp))
(define (hourly-rate  emp)    (car (cddddr emp)))


;; --- Commissioned Employee ---
;; Record: (commission first-name last-name min-pay sales commission-rate)

(define (make-commission-employee first last min-pay sales rate)
  (list 'commission first last min-pay sales rate))

(define (commission? emp)          (eq? (car emp) 'commission))
(define (commission-min-pay  emp)  (cadddr emp))
(define (commission-sales    emp)  (car  (cddddr emp)))
(define (commission-rate     emp)  (cadr (cddddr emp)))


;;; ============================================================
;;;  EARNINGS CALCULATION  (mirrors getEarnings in Smalltalk)
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
;;;  INFO STRING  (mirrors getInfo in Smalltalk)
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
       "minimum salary: "   (number->string (commission-min-pay emp))
       ", sales amount: "   (number->string (commission-sales   emp))
       ", commission rate: "(number->string (* (commission-rate emp) 100)) "%"))

    (else (error "Unknown employee type" emp))))


;;; ============================================================
;;;  COMPARISON OPERATOR  (mirrors check:comparedTo:given: )
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
;;;  FILE READER  (mirrors readFrom: )
;;; ============================================================

(define (read-employees filename)
  (let ((port (open-input-file filename)))
    (let loop ((employees '()))
      (let ((line (read-line port)))
        (if (eof-object? line)
            (begin
              (close-input-port port)
              (reverse employees))          ; preserve file order
            (let ((fields (string-split line)))
              (if (null? fields)
                  (loop employees)          ; skip blank lines
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
                       (loop employees)))))))))))  ; skip malformed lines


;;; ============================================================
;;;  STRING UTILITIES
;;; ============================================================

;; Split a string on whitespace, returning a list of tokens.
(define (string-split str)
  (let loop ((chars (string->list str))
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

;; Format a real number to two decimal places (e.g. 212.50).
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
      (if (< dec-part 10) (string-append "0" dec-str) dec-str))))


;;; ============================================================
;;;  ACTIONS
;;; ============================================================

;; Filter employees that satisfy the operator/threshold condition.
(define (filter-employees employees threshold op)
  (filter (lambda (e) (check-op (get-earnings e) threshold op))
          employees))

;; Print all matching employees.
(define (action-print employees threshold op)
  (for-each
    (lambda (emp)
      (let ((amount (get-earnings emp)))
        (when (check-op amount threshold op)
          (display (get-info emp)) (newline)
          (display "earned $") (display (format-dollars amount)) (newline)
          (newline))))
    employees))

;; Count matching employees.
(define (action-count employees threshold op)
  (length (filter-employees employees threshold op)))

;; Minimum earnings among matching employees, then print those employees.
(define (action-min employees threshold op)
  (let ((matching (filter-employees employees threshold op)))
    (if (null? matching)
        (display "No matching employees.\n")
        (let ((min-val (apply min (map get-earnings matching))))
          (action-print employees min-val "eq")))))

;; Maximum earnings among matching employees, then print those employees.
(define (action-max employees threshold op)
  (let ((matching (filter-employees employees threshold op)))
    (if (null? matching)
        (display "No matching employees.\n")
        (let ((max-val (apply max (map get-earnings matching))))
          (action-print employees max-val "eq")))))

;; Total earnings of matching employees.
(define (action-total employees threshold op)
  (let ((total (apply + (map get-earnings (filter-employees employees threshold op)))))
    (display "Total payment is $")
    (display (format-dollars total))
    (newline)))

;; Average earnings of matching employees.
(define (action-avg employees threshold op)
  (let* ((matching (filter-employees employees threshold op))
         (cnt      (length matching)))
    (if (= cnt 0)
        (display "No matching employees.\n")
        (let ((avg (/ (apply + (map get-earnings matching)) cnt)))
          (display "Average payment per employee is $")
          (display (format-dollars avg))
          (newline)))))


;;; ============================================================
;;;  MAIN ENTRY POINT
;;; ============================================================

(define (main args)
  ;; args is the list returned by (command-line) minus argv[0],
  ;; i.e. the arguments after the script name.
  (let ((argc (length args)))
    (cond
      ;; ---- wrong argument count ----
      ((and (not (= argc 2)) (not (= argc 4)))
       (display "Usage: mit-scheme --quiet --load main.scm -- employee_file action")
       (newline)
       (display "or")
       (newline)
       (display "Usage: mit-scheme --quiet --load main.scm -- employee_file action operator threshold")
       (newline)
       (display "Valid actions: count print min max total avg")
       (newline)
       (display "Valid operators: eq ne gt ge lt le")
       (newline))

      (else
       (let* ((filename   (list-ref args 0))
              (action-str (list-ref args 1))
              ;; Default: all employees (earnings >= 0)
              (op        (if (= argc 4) (list-ref args 2) "ge"))
              (threshold (if (= argc 4) (string->number (list-ref args 3)) 0))
              (employees (read-employees filename)))

         (cond
           ;; ---- no employees in file ----
           ((null? employees)
            (display "There are no employees.") (newline))

           ;; ---- no employees match condition ----
           ((= (action-count employees threshold op) 0)
            (display "There are no employees that satisfied the specified condition.")
            (newline))

           ;; ---- dispatch on action ----
           ((string=? action-str "count")
            (display "There are ")
            (display (action-count employees threshold op))
            (display " employees.")
            (newline))

           ((string=? action-str "print")
            (action-print employees threshold op))

           ((string=? action-str "min")
            (action-min employees threshold op))

           ((string=? action-str "max")
            (action-max employees threshold op))

           ((string=? action-str "total")
            (action-total employees threshold op))

           ((string=? action-str "avg")
            (action-avg employees threshold op))

           (else
            (display "Invalid action: ") (display action-str) (newline)
            (display "Valid actions: print count min max total avg") (newline))))))))


;;; ============================================================
;;;  LAUNCH  — grab args that follow "--" on the command line
;;; ============================================================

(let* ((all-args  (cdr (command-line)))          ; drop interpreter name
       ;; Drop everything up to and including "--" if present
       (sep       (member "--" all-args))
       (our-args  (if sep (cdr sep) all-args)))
  (main our-args))