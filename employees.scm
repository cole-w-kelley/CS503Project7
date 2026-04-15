
;; Following is for salary emps

(define (createSalariedEmp first last salary) (list 'salaried first last salary))

;; Tells us if empl obj is salaried

(define (salaried? emp) (eq? (car emp) 'salaried))

;; Getting value of fields

(define (empFirstName emp) (cadr emp))
(define (empLastName emp) (caddr emp))
(define (salariedSalary emp) (cadddr emp))

;; End of salaried employee

;; Start hourly employees

(define (createHourlyEmp first last rate hours) (list 'hourly first last rate hours))

;; Tell us if an emp is a hourly emp

(define (hourly? emp) (eq? (car emp) 'hourly))

;; Getting fields

(define (hourlyRate emp) (cadddr emp))
(define (hourlyHours emp) (car (cddddr emp)))

;; End hourly

;; Commissioned employees

(define (createCommissionedEmp first last minPay sales commissionRate) (list 'commission first last minPay sales commissionRate))

;; Is commissioned?

(define (commission? emp) (eq? (car emp) 'commission))

;; fields

(define (commisionEmpMinPay emp)(cadddr emp))
(define (commisionEmpSales emp) (car (cddddr emp)))
(define (commisionEmpRate emp) (cadr (cddddr emp)))

;; End commissioned

;; Earnings for each employee type

(define (getEarning emp)
  (cond 
    ((salaried? emp)
      (salariedSalary emp)
    )

    ((hourly? emp)
      (let ((h (hourlyHours emp))
            (r (hourlyRate emp)))
        (cond
          ((<= h 40) (* h r))
          ((<= h 50) (+ (* 40 r) 
                        (* (* r 1.5) (- h 40))))
          (else (+ (* r 40)
                  (* (* r 1.5) 10) 
                  (* (* r 2) (- h 50))))))
    )
    ((commission? emp)
    (let ((earned (* (commisionEmpSales emp) (commisionEmpRate emp      )))
      (minPay (commisionEmpMinPay emp)))
      (if (< earned minPay) minPay earned)
      )
    )
    (else (error "Unknown employee type" emp))
  )
)

;; Get info for each employee

(define (getInfo emp)
  (cond 
    ((salaried? emp)
      (string-append 
       "Salaried employee: " (empFirstName emp) " " (empLastName emp) "\n"
       "weekly salary: " (number->string (salariedSalary emp))
      )
    )
    ((hourly? emp)
      (string-append
       "Hourly employee: " (empFirstName emp) " " (empLastName emp) "\n"
       "hours worked: " (number->string (hourlyHours emp))
       ", hourly rate: " (number->string (hourlyRate emp))
      )
    )
    ((commission? emp)
      (string-append
        "Commission employee: " (empFirstName emp) " " (empLastName emp) "\n"
        "minimum salary: " (number->string (commisionEmpMinPay emp))
        ", sales ammount: " (number->string (commisionEmpSales emp))
        ", commision rate: " (number->string (exact->inexact (* (commisionEmpRate emp) 100))) "%"
      )
    )
    (else (error "Unknown employee type" emp))
  )
)

;; Process operater characetr given a value

(define (checkOp value threshold op)
  (cond
    ((string=? op "eq") (= value threshold))
    ((string=? op "ne") (not (= value threshold)))
    ((string=? op "ge") (>= value threshold))
    ((string=? op "le") (<= value threshold))
    ((string=? op "gt") (> value threshold))
    ((string=? op "lt") (< value threshold))
    (else #f)
  )
)

;; String stuff with help from provided functions

(define (parse-string str)
  (let ((port (open-input-string str)))
    (let ((result (parse-string-helper port '())))
      (close-input-port port)
      result)))

(define (parse-string-helper port tokens)
  (skip-whitespaces port)
  (let ((ch (peek-char port)))
    (if (eof-object? ch)
      (reverse tokens)
      (let ((token (read-token port "")))
        (parse-string-helper port (cons token tokens))))))

(define (read-token port current)
  (let ((ch (peek-char port)))
    (if (or (eof-object? ch) (char-whitespace? ch))
      current
      (begin
        (read-char port)
        (read-token port (string-append current (string ch)))))))

(define (skip-whitespaces port)
  (let ((ch (peek-char port)))
    (if (and (not (eof-object? ch)) (char-whitespace? ch))
      (begin
        (read-char port)
        (skip-whitespaces port)))))

;; File readin

(define (readEmployees fileName)
  (let ((port (open-input-file fileName)))
    (let loop ((employees '()))
      (let ((line (read-line port)))
        (if (eof-object? line)
          (begin
            (close-input-port port)
            (reverse employees)
          )
          (let ((fields (parse-string line)))
            (if (null? fields)
              (loop employees)
              (let ((type (car fields)))
                (cond 

                  ;; If type is salary
                  ((and (string=? type "salaried") (= (length fields) 4))
                    (loop (cons 
                            (createSalariedEmp 
                              (list-ref fields 1)
                              (list-ref fields 2)
                              (string->number (list-ref fields 3))
                            )
                            employees
                          )
                    )
                  )

                  ;; If type is hourly
                  ((and (string=? type "hourly") (= (length fields) 5))
                    (loop (cons 
                            (createHourlyEmp 
                              (list-ref fields 1)
                              (list-ref fields 2)
                              (string->number (list-ref fields 3))
                              (string->number (list-ref fields 4))
                            )
                            employees
                          )
                    )
                  )

                  ;; If type is commission
                  ((and (string=? type "commission") (= (length fields) 6))
                    (loop (cons 
                            (createCommissionedEmp 
                              (list-ref fields 1)
                              (list-ref fields 2)
                              (string->number (list-ref fields 3))
                              (string->number (list-ref fields 4))
                              (string->number (list-ref fields 5))
                            )
                            employees
                          )
                    )
                  )
                  (else (loop employees))
                )
              )
            )
          )
        )
      )
    )
  )
)


;; Helper functions

(define (getEmployees employees threshold op)
  (filter (lambda (e) (checkOp (getEarning e) threshold op))
   employees
  )
)

(define (printEmp employees threshold op)
  (for-each
    (lambda (emp)
      (if (checkOp (getEarning emp) threshold op)
        (begin
          (display (getInfo emp))
          (newline)
          (display "earned $")
          (display (number->string (getEarning emp)))
          (newline)
          (newline)
        )
        #f
      )
    )
    employees
  )
)

(define (countEmp employees threshold op)
  (length (getEmployees employees threshold op))
)

(define (minEmp employees threshold op)
  (let* ((matching (getEmployees employees threshold op))
        (min-val (apply min (map getEarning matching))))
    (printEmp matching min-val "eq"))
)

(define (maxEmp employees threshold op)
  (let* ((matching (getEmployees employees threshold op))
        (max-val (apply max (map getEarning matching))))
    (printEmp matching max-val "eq"))
)

(define (totalEmp employees threshold op)
  (let ((total (apply + (map getEarning (getEmployees employees threshold op)))))
    (display "Total payment is $")
    (display (number->string total))
    (newline)
    'done
  )
)

;; Get average of emps
(define (avgEmp employees threshold op)
  (let* ((total (apply + (map getEarning (getEmployees employees threshold op))))
        (count (length (getEmployees employees threshold op)))
        (avg (/ total count))
      )
    (display "Average payment per employee is $")
    (display (number->string avg))
    (newline)
    'done
  )
)

;; validation

(define (validOp? op)
  (or (string=? op "eq") (string=? op "ge") (string=? op "ne") 
      (string=? op "lt") (string=? op "gt") (string=? op "le")
  )
)

;; 'Total' might need to be sum
(define (validAction? action)
  (or (string=? action "count") (string=? action "max") (string=? action "min") 
      (string=? action "avg") (string=? action "print") (string=? action "total")
  )
)


;; Main function
(define (perform filename . rest)
  (cond

    ;; Check usage
    ((or (null? rest) (and (not (= (length rest) 1)) (not (= (length rest) 3))))
      ;; idk if this is backward
      (display "Usage: (perform employee_file action)")
      (newline)
      (display "or")
      (newline)
      (display "Usage: (perform employee_file action operator threshold)")
      (newline)
      (display "Valid actions: count print min max total avg")
      (newline)
      (display "Valid operators: eq ne gt ge lt le")
      (newline)
      (display "")
      )
    


    ;; Check action
    ((not (validAction? (car rest)))
      (display "Invalid action: ") (display (car rest)) (newline)
      (display "Valid actions: print count min max total avg")
      (newline)
      'done
    )

    ;; check op
    ((and (= (length rest) 3) (not (validOp? (cadr rest))))
      (display "Invalid operator: ")  (display (cadr rest)) (newline)
      (display "Valid operators:  eq ne gt ge lt le")
      (newline)
      'done
    )

    ;; could validate threhsold but I will assume it gives correct number for sake of this project

    (else
      (let* ((action (car rest))
             (op (if (= (length rest) 1) "ge" (cadr rest)))
             (threshold (if (= (length rest) 1) 0 (caddr rest)))
             (employees (readEmployees filename))
            )
        (cond
          ((null? employees)
            (display "There are no employees.") (newline)
          )

          ((= (countEmp employees threshold op) 0)
            (display "There are no employees that satisfied the specified condition.") (newline)
          )

          ((string=? action "count")
            (newline)
            (newline)
            (display "There are ")
            (display (countEmp employees threshold op))
            (display " employees.")
            (newline)
          )

          ((string=? action "max")
            (newline)
            (newline)
            (maxEmp employees threshold op)
            (newline)
            (display"")
          )

          ((string=? action "min")
            (newline)
            (newline)
            (minEmp employees threshold op)
            (newline)
            (display "")
          )

          ((string=? action "avg")
            (newline)
            (newline)
            (avgEmp employees threshold op)
            (newline)
            (display "")
          )

          ((string=? action "total")
            (newline)
            (newline)
            (totalEmp employees threshold op)
            (newline)
            (display "")
          )

          ((string=? action "print")
            (newline)
            (newline)
            (printEmp employees threshold op)
            (newline)
            (display "")
          )
        )
      )
    )
  )
)