#lang racket

(define directory-num 0)
(define file-num 0)

(define envoked-location (current-directory))

(define exe? (lambda (file-path)
    (member 'execute (file-or-directory-permissions file-path ))))

(define file? (lambda (file-path)
    (file-exists? file-path)))

(define dir? (lambda (file-path)
    (directory-exists? file-path)))

(define choose-stem 
    (lambda (i file-list)
        (if (< i (length file-list)) "├" "└")))

(define get-icons (lambda (path)
    (define icons "")
    (if (file? path)
        (and (set! icons (string-append icons " 📄"))
            (if (exe? path)
                (set! icons (string-append icons " ⚙"))
                (set! icons (string-append icons "  "))))
        '())
    (if (dir? path)
        (set! icons (string-append icons " 📁  ")) '())
    (string-append icons " ")))

(define shave-off-last (lambda (lst)
    (reverse (rest (reverse lst)))))

(define walk (lambda (file-path indent-strings)
    (define files (directory-list file-path))
    (define i 0)
    (for ([file files])
        (define adjusted-path (string->path (string-append (path->string file-path) "/" (path->string file))))
        (set! i (+ i 1))
        (printf "~a\n" (string-append (string-join indent-strings "")
                       (choose-stem i files) 
                       "─" 
                       (get-icons adjusted-path)
                       (path->string file)))
        (if (file? adjusted-path) (set! file-num (+ file-num 1)) (set! directory-num (+ directory-num 1)))
        (if (dir? adjusted-path)
            (if (and (= i (length files)) (dir? adjusted-path)) 
                (walk adjusted-path (append indent-strings (list " \t")))
                (walk adjusted-path (append indent-strings (list "│\t")))) '()))))

(printf ".\n")
(walk envoked-location '(""))
(printf "Done. Files: ~a Directories: ~a\n" file-num directory-num)