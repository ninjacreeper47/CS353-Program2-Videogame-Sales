#lang racket

(require csv-reading)  ;https://docs.racket-lang.org/csv-reading/index.html

;test function
(define my-input
  "Video Games Sales.csv")

;Precondition:   File must be a csv file, which matches the  default csv-reader specs
;Postcondition: returns a list of lists,  where each line in the file is a list containing an element for each data attribute
(define (load-data filename)
 (csv->list (open-input-file filename)))

;test function
(define my-data-header
  (list "index" "Rank" "Game Title" "Platform" "Year" "Genre" "Publisher" "North America" "Europe" "Japan" "Rest of the World" "Global" "Review"))
(define wii-sports
  (list 0 1 "Wii Sports" "Wii" 2006 "Sports" "Nintendo" 40.43 28.39 3.77 8.54 81.12 76.28))

;Precondition:  takes in a list where each element corresponds to an entry in the header list
;(ex: if "Game Title" is the 3rd spot of header then the input list should have it's title at the 3rd spot)
;Postcondition: returns the element of the list that matches the given attribute
(define (find-attribute data-line header attribute)
  (list-ref data-line (index-of header attribute)))