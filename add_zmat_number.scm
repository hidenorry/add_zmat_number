#!/usr/bin/env gosh
(use srfi-1)
(use srfi-13)

(define (mix-list lis1 lis2)
  ;; (mix-list  '(a b c d) '(1 2 3 4)) => (a 1 b 2 c 3 d 4)
  (letrec ((rec (lambda (l1 l2 acc)
                  (cond ((and (null? l1) (null? l2)) (reverse acc))
                        ((null? l1) (append (reverse acc) l2))
                        ((null? l2) (append (reverse acc) l1))
                        (else (rec l2 (cdr l1)
                                   (cons (car l1) acc)))))))
    (rec lis1 lis2 '())))

(define (change-numbers-in-string str :optional (fn identity))
  ;; (change-numbers-in-string "ab35dc45") => "ab35dc45"
  ;; (change-numbers-in-string "ab35dc45" (lambda (n) (+ (x->number n) 122))) => "ab157dc167"
  ;; (change-numbers-in-string "ab35dc4.0" (lambda (n) (if ((string->regexp "\\.") n) n (+ (string->number n) 122)))) => "ab157dc4.0"
  ;; (change-numbers-in-string "ab35dc4.0" (lambda (n) (if ((string->regexp "\\.") n) n (+ (string->number n) 122)))) => "ab157dc4.0"
  (let ((lstr (map (lambda (n) (if (and (> (string-length n) 1) (string=? (substring n 0 1) "."))
                                   (substring n 1 (string-length n))
                                   n))
                   (filter (lambda (x) (not (string=? "." x)))
                           (string-split str (string->regexp "\\d+")))))
        (lnum (map (lambda (n) (if (string=? n "")
                                   n
                                   (x->string (fn n))))
               (string-split str (string->regexp "[^\\d\.]+")))))
    (fold string-append ""
          (reverse (if (string=? (car lstr) "")
                       (mix-list lstr lnum)
                       (mix-list lnum lstr))))))

(define (read-file file fn)
  (with-input-from-file file
    (lambda ()
      (port-map
       (lambda (line)
         (change-numbers-in-string line fn))
       read-line))))

(define (usage)
  (print "Usage: gosh add_zmat_number.scm <file> <number>")
  (exit 0))


(define (main args)
  (when (not (= (length args) 3)) (usage) (exit 2))
  (let ((file (car (cdr args)))
        (num (string->number (car (cddr args)))))
    (for-each
     print
     (read-file file 
        (lambda (n) (if ((string->regexp "\\.") n) n (+ (string->number n) num)))))))

