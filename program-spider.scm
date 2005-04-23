#!/usr/bin/scsh -s
-lel htmlprag/load.scm -o htmlprag -o handle -o conditions -o srfi-13
!#

;;; $Id:$

;;;srfi-1: list library
;;;srfi-13: string operations
;;;handle: error handling
;;;conditions: condition checking
;;;htmlprag: parse html

(define topurl
  "http://www.roskilde-festival.dk/object.php?obj=88000c&Letter=alle&code=1")
(define httpclient '(wget -O -))
;;; Some addresses for find-branch-address
(define bandlisttable '(td table tr td (1 . table)))
(define bandname '((1 . td) a))
(define bandinfourl '(@ href))
(define shortdesc '((1 . td)))
(define photo '(td table tr td (1 . table) tr (1 . td) img @ src))
(define longdescription '(td table (2 . tr) (1 . td) p))
(define soundsamples '(td table tr td (1 . table)
                          tr (2 . td) table tr td a @ href))
(define homepage '(td table tr td (1 . table)
                      tr (2 . td) table (1 . tr) td a @ href))
(define feature '(td table (2 . tr) (1 . td) (1 . p) b a @ href))

(define faktboxstart (rx "faktabox start"))
(define faktboxend (rx "Faktabox slut"))
;;; -------------------------------------------------------------
(define (url-body url)
  (cdr (find-branch-address '(html body)
                            (html->shtml
                             (run/string ,(append httpclient
                                                  (list url)))))))

(define (get-url-string url)
  (run/string ,(append httpclient (list url))))

(define (car-is? el x)
  (and (list? x)
       (equal? el
               (car x))))

(define (find-car el lis)
  (find (lambda (x)
          (car-is? el x))
        lis))

(define (filter-car el lis)
  (filter (lambda (x)
            (car-is? el x))
          lis))

(define split-slash (infix-splitter "/"))

(define split-parens (infix-splitter (rx (| "(" ")" ))))

(define (elements-between pred1
                          pred2
                          lst)
  (if (not (list? lst))
      #f
      (if (null? lst)
          #f
          (if (pred1 (car lst))
              (take-while (lambda (x) (not (pred2 x)))
                          (cdr lst))
              (or (elements-between pred1
                                    pred2
                                    (car lst))
                  (elements-between pred1
                                    pred2
                                    (cdr lst)))))))

(define (comment-match pr re)
  (and (list? pr)
       (eq? (car pr)
            shtml-comment-symbol)
       (regexp-search? re (cadr pr))))

(define (find-branch-address seq tree)
  (if (null? seq)
      tree
      (let ((look (car seq)))
        (if (pair? look)
            (find-branch-address (cdr seq)
                                 (list-ref (filter-car (cdr look)
                                                       tree)
                                           (car look)))
            (find-branch-address (cdr seq)
                                 (find-car look
                                           tree))))))

(define (error-or-f? val)
  (or (error? val)
      (not val)))

(define (call+calc-or-fail thunk successcalc failthunk)
  (let ((val (ignore-errors thunk)))
    (if (error-or-f? val)
        (failthunk)
        (successcalc val))))

(define (faktabox tree)
  (elements-between (lambda (x)
                      (comment-match x faktboxstart))
                    (lambda (x)
                      (comment-match x faktboxend))
                    tree))

(define (bandlist-table tree)
  (filter-car
   'tr
   (find-branch-address
    bandlisttable
    (faktabox tree))))

(define (get-html-branch address tree)
  (with-success-or-fail
   (shtml->html (cdr (find-branch-address address
                                          tree)))
   (lambda (x) x)
   ""))

(define (get-bandinfo url)
  (let ((fbox (faktabox (url-body url))))
    (list (get-html-branch longdescription fbox)
          (get-html-branch photo fbox)
          (get-html-branch homepage fbox)
          (get-html-branch soundsamples fbox)
          (get-html-branch feature fbox))))

(define (url-name-country rest collected)
  (if (null? rest)
      collected
      (let* ((el (car rest))
             (newel
              (with-success-or-fail
               (find-branch-address bandname el)
               (lambda (el)
                 (do-or-f
                  (cons (second (find-branch-address bandinfourl
                                                     el))
                        (let ((y (split-parens (third el))))
                          (list (string-trim-both (car y))
                                (split-slash (second y)))))))
               #f)))
        (if newel
            (short-description (cdr rest)
                               collected
                               newel)
            (url-name-country (cdr rest)
                              collected)))))

(define (short-description rest collected passover)
  (if (null? rest)
      collected
      (let* ((el (car rest))
             (newel (do-or-f
                     (let ((strlist
                            (filter string?
                                    (drop (find-branch-address
                                           shortdesc
                                           el)
                                          2))))
                       (if (not (null? strlist))
                           (reduce string-append
                                   ""
                                   strlist)
                           #f)))))
        (if newel
            (url-name-country (cdr rest)
                              (cons (append (cdr passover)
                                            (list newel)
                                            (get-bandinfo (car passover)))
                                    collected))
            (short-description (cdr rest)
                               collected
                               passover)))))

(define (band-url-name-country-shortdesc-list tree)
  (url-name-country tree '()))

(define (band-url-name-country-shortdesc-list-old tree)
  (filter-map
   (lambda (x)
     (with-success-or-fail
      (find-branch-address bandname x)
      (lambda (x)
        (do-or-f
         (cons (second (find-branch-address bandinfourl
                                            x))
               (let ((y (split-parens (third x))))
                 (list (string-trim-both (car y))
                       (split-slash (second y)))))))
      (do-or-f
       (let ((strlist (filter string?
                              (drop (find-branch-address shortdesc
                                                         x)
                                    2))))
         (if (not (null? strlist))
             (reduce string-append
                     ""
                     strlist)
             #f)))))
   tree))

(define-syntax with-success-or-fail
  (syntax-rules ()
    ((with-success-or-fail body successcalc failbody)
     (call+calc-or-fail (lambda () body)
                        successcalc
                        (lambda () failbody)))))

(define-syntax do-or-f
  (syntax-rules ()
    ((do-or-f body)
     (with-success-or-fail body (lambda (x) x) #f))))

;;; just for debugging convenience, I could use format, but I don't wanna!
(define (display-list lst)
  (for-each (lambda (x)
              (newline)
              (write x)
              (newline))
            lst))

;;(display-list (get-first-level))

;;; $Log:$
