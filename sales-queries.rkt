#lang racket

(require csv-reading)  ;https://docs.racket-lang.org/csv-reading/index.html


(define my-input
  "Video Games Sales.csv")

;Precondition:   File must be a csv file, which matches the  default csv-reader specs
;Postcondition: returns a list of lists,  where each line in the file is a list containing an element for each data attribute
(define (load-data filename)
 (csv->list (open-input-file filename)))

(define (data-header data)
(first data))
