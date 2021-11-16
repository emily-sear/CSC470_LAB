;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname november16-LAB) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; November 16, 2021 --> CSC 470 Lab

; 1. Write a block-exp that contains exactly 8 numbers
(define block-exp '(1 a (2 b (3 c (4 d (5 e (6 f (7 g (8 h)))))))))
(define block-num 1)
(define block-symbol 'a)
(define block-symbol-block '(a (2 b 7) (5 e 6)))


; 2. Write a scheme function called count-block-exp that takes a block-exp as a parameter and boils down to the sum of all of the numbers in the block-exp
(define count-block-exp
  (lambda (block-exp)
    (cond
      ((null? block-exp) 0)
      ((number? block-exp) block-exp)
      ((symbol? block-exp) 0)
      ((number? (car block-exp)) (+ (car block-exp)
                                    (if (null? (cddr block-exp)) 0
                                   (count-block-exp (caddr block-exp)))))
      (else (+ (count-block-exp (cadr block-exp)) (count-block-exp (caddr block-exp)))))))

(count-block-exp block-exp)
(count-block-exp block-num)
(count-block-exp block-symbol)
(count-block-exp block-symbol-block)

; 3. Write a scheme function called collect-symbols that takes a block-exp as a parameter and returns a list containing all of the symbols found in the block-exp

(define collect-symbols
  (lambda (block-exp)
    (cond
      ((null? block-exp) '())
      ((number? block-exp) '())
      ((symbol? block-exp) block-exp)
      ((number? (car block-exp)) (cons (cadr block-exp)
                                       (if (null? (cddr block-exp)) '()
                                       (collect-symbols (caddr block-exp)))))
      (else (cons (car block-exp) (append (collect-symbols (cadr block-exp))
                                        (collect-symbols (caddr block-exp))))))))

(collect-symbols block-exp)
(collect-symbols block-num)
(collect-symbols block-symbol)
(collect-symbols block-symbol-block)