#!/home/nemo/bin/bin/csi -s
;;; json2csv.scm --- Covert large json files to csv

;; Copyright 2015, Nicholas M. Van Horn

;; Author: Nicholas M. Van Horn <vanhorn.nm@gmail.com>
;; Keywords: json csv convert conversion cli terminal command line
;; Version: 1.0.1

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA. (or visit http://www.gnu.org/licenses/)
;;

;;; Commentary:

;;; This program provides a simple and efficient command line
;;; interface for quickly listing and managing video files stored in
;;; nested directories on a file system. Run dvr.scm -h for
;;; information on how to use the program. For up to date information,
;;; see the github page for this program at
;;; https://github.com/n3mo/dvr 

(require-extension args)
(require-extension files)
(require-extension srfi-13)
(require-extension srfi-1)
(require-extension medea)

;;; To provide consistent access to JSON elements, we turn JSON arrays
;;; to lists instead of vectors (code taken from medea egg example)
;; Parsing JSON arrays as lists instead of vectors
;; (define array-as-list-parser
;;   (cons 'array (lambda (x) x)))
;; (json-parsers (cons array-as-list-parser (json-parsers)))

;;; Version with list flattening
;; (define array-as-list-parser
;;   (cons 'array (lambda (x) (flatten x))))
;; (json-parsers (cons array-as-list-parser (json-parsers)))

;;; The following list contains all defined command line options
;;; available to the user. For example, (h help) makes all of the
;;; following equivalent options available at runtime: -h, -help, --h,
;;; --help. These are used by the "args" egg.
(define opts
  (list (args:make-option (h help)    #:none "Help information"
			  (usage))
	(args:make-option (i input)   #:none  "Input file")
	(args:make-option (o output)  #:none  "Output file"
			  (set! out-to-file? #t))
	(args:make-option (l list)    #:none "List available data fields"
			  (set! display-fields? #t))
	(args:make-option (k keep)    #:none "Data fields to keep"
			  (set! keep? #t))
	(args:make-option (r remove)  #:none "Data fields to remove"
			  (set! keep? #f))
	))

;;; This procedure is called whenever the user specifies the help
;;; option at runtime OR whenever an unexpected command line option or
;;; operand is passed to this script.
(define (usage)
 (with-output-to-port (current-error-port)
   (lambda ()
     (print "Usage: json2csv [file] [options...]")
     (newline)
     (print (args:usage opts))
     (print "Converts json files to csv")
     (print "Report bugs to nemo1211 at gmail.")))
 (exit 1))

;;; This function strips operands (rather than "-" prefixed options)
;;; from the command line arguments and returns a list of all operands
(define (list-operands myargs)
  (cond
   ((null? myargs) '())
   ((string-prefix? "-" (car myargs))
    (list-operands (cdr myargs)))
   (else (cons (car myargs) (list-operands (cdr myargs))))))

;;; Return keys from alist that do not correspond to values that are
;;; lists themselves. This provides a mechanism for removing nested
;;; json entries (the easy solution for now)
;; (define (non-nested-keys alist #!key (flds #f) (keep? #f))
;;   (let ([keys (remove null? 
;;                       (map (lambda (apair)
;;                              (if (not (list? (cdr apair)))
;;                                (car apair)
;;                                '()))
;;                            alist))])
;;     (cond
;;       [(null? flds) keys]
;;       [keep? (lset-intersection eq? flds keys)]
;;       [else (lset-difference eq? keys flds)])))

;;; -----------------------------------------------------------------
;;; Code for finding nested alist keys
;;; -----------------------------------------------------------------

(define (tree-label    tree) (if (pair? (car tree))
                                 (caar tree)
                                 (car tree)))
(define (tree-children tree) (cdr tree))
(define (tree-leaf?    tree) (null? (cdr tree)))

(define (mapcan fun list)
  (if (null? list)
      '()
      (append (fun (car list)) (mapcan fun (cdr list)))))

(define (paths-to-leaves tree . rest)
    (let* ((path (if (null? rest)
                     '()
                     (car rest)))
           (path-to-tree (append path (list (tree-label tree)))))
      (if (tree-leaf? tree)
          (list path-to-tree)
          (mapcan (lambda (child)
                    (paths-to-leaves child path-to-tree))
                  (tree-children tree)))))

;;; Consider a tree structure bar, view each function's behavior with:
;; (map (lambda (tree) (display (list
;;       (tree-leaf? tree) (tree-label tree) (tree-children tree))) (newline))
;;       bar)

;;; Recursively find keys in a potentially-nested alist
(define (build-nested-key alist key)
  (if (or (not (list? alist)) (not (list? key)))
      key
      (let ((myvalue (alist-ref (car key) alist)))
	(if (not (list? myvalue))
	    key
	    (cons key
		  (get-nested-alist-keys myvalue))))))

(define (get-nested-alist-keys alist)
  (if (not (list? alist))
      '()
      (let ((keys (map
		   (lambda (x) (list (car x))) alist)))
	(map (lambda (x) (build-nested-key alist x)) keys))))

;; (define (nested-keys-parser family)
;;   (cond [(null? family) '()]
;; 	[(list? (car family))
;; 	 (cons (car (car family))
;; 	       (nested-keys-parser (cdr (car family))))]
;; 	[else (cons (car family)
;; 		    (nested-keys-parser (cdr family)))]))


;;; This is the procedure to call
;; (define (nested-alist-keys alist)
;;   (let ((keys (get-nested-alist-keys alist)))
;;     (join (map paths-to-leaves keys))))
(define (nested-alist-keys alist #!key (flds #f) (keep? #f))
  (let* ((raw-keys (get-nested-alist-keys alist))
	 (keys (join (map paths-to-leaves raw-keys))))
    (cond
     [(null? flds) keys]
     [keep? (lset-intersection equal? flds keys)]
     [else (lset-difference equal? keys flds)])))

;;; Grab the value of a key from a nested alist
(define (nested-alist-ref keys nested-alist)
  (cond ((null? (cdr keys))
	 (alist-ref (car keys) nested-alist))
	(else (nested-alist-ref (cdr keys) (alist-ref (car keys) nested-alist)))))

;;; Same as string->symbol, but works for a list of lists (of strings)
(define (nested-string->symbol lists)
  (cond [(null? lists) '()]
	[else (cons (map string->symbol (car lists))
		    (nested-string->symbol (cdr lists)))]))

;;; Make a list of lists out of an alist. This will be used for
;;; preparing the data for writing to csv file.
;; (define (alist->nested-list alist keys)
;;   (map
;;    (lambda (curr-key) (alist-ref curr-key alist)) keys))
(define (alist->nested-list alist keys)
  (map
   (lambda (curr-key) (nested-alist-ref curr-key alist)) keys))

;;; Missing and true/false data can be adjusted here. Without this
;;; step, true/false data would be written to file as #t/#f and
;;; missing data as null. We change those behaviors to TRUE/FALSE and
;;; NA here.
(define (format-csv-record record)
  (cond
   [(eq? record #t) 'TRUE]
   [(eq? record #f) 'FALSE]
   [(eq? record 'null) 'NA]
   [else record]))

;;; This writes a list of lists (record) to disk. This procedure
;;; writes to the current-output-port
(define (write-csv records)
  ;; Outer loop across rows
  (for-each (lambda (row)
	      ;; Inner loop across columns
	      (let column-loop ((fields row))
		(if (null? fields)
		    (newline)
		    (let ((curr-field (format-csv-record (car fields)))
			  (final? (null? (cdr fields))))
		      (write curr-field)
		      (if (not final?) (display ","))
		      (column-loop (cdr fields))))))
	    records))

;;; This gets the work done
;; (define (json->csv in-file #!key (flds #f) (keep? #f))
;;   ;; DEBUG
;;   ;; (print flds)
;;   ;; (print keep?)
;;   ;; (exit 0)

;;   (let ((keys (with-input-from-file in-file
;; 		(lambda ()
;; 		  (nested-alist-keys (read-json (read-line)) #:flds
;; 				     flds #:keep? keep?)))))
    
;;     ;; Write the csv header
;;     ;; (write-csv (list keys))
;;     (write-csv (list (map (lambda (x) (string-join (map symbol->string (flatten x)) ":")) keys)))
;;     (call-with-input-file in-file
;;       (lambda (in)
;; 	(let loop ((in in))
;; 	  (receive (object remainder)
;; 	      (read-json in consume-trailing-whitespace: #f chunk-size: (* 5 1024))
;; 	    (when object
;; 	      (write-csv (list (alist->nested-list object keys)))
;; 	      (loop remainder))))))))
(define (json->csv in-file #!key (flds #f) (keep? #f))
  (let ((keys (nested-alist-keys (read-json (read-line)) #:flds
				 flds #:keep? keep?)))
    
    ;; Write the csv header
    ;; (write-csv (list keys))
    (write-csv (list (map (lambda (x) (string-join (map symbol->string (flatten x)) ":")) keys)))
    
    (let loop ((in (current-input-port)))
      (receive (object remainder)
	  (read-json in consume-trailing-whitespace: #f chunk-size: (* 5 1024))
	(when object
	  (write-csv (list (alist->nested-list object keys)))
	  (loop remainder))))))

;;; Print available json fields (including nested fields)
;; (define (print-fields)
;;   (let ((in-file (alist-ref 'input options)))
;;     (if (not (file-exists? in-file))
;; 	(begin (print (string-append "Abort: Cannot find file " in-file))
;; 	       (exit 1))
;; 	(with-input-from-file in-file
;; 	  (lambda ()
;; 	    (for-each
;; 	     (lambda (x)
;; 	       (print (string-join (map symbol->string x) ":")))
;; 	     (nested-alist-keys (read-json
;; 				 (read-line)))))))
;;     (exit 0)))
(define (print-fields)
  (for-each
   (lambda (x)
     (print (string-join (map symbol->string x) ":")))
   (nested-alist-keys (read-json
		       (read-line))))
  (exit 0))

;; (define (task-dispatch #!optional keep)
;;   (let* ((in-file (alist-ref 'input options))
;; 	 (out-file
;; 	  (if in-file
;; 	      (pathname-replace-extension in-file "csv")
;; 	      #f))
;; 	 (field-args (nested-string->symbol
;; 		      (map (lambda (x) (string-split x ":"))
;; 			   operands))))

;;     (if display-fields? (print-fields))

;;     (cond
;;      [out-to-file?
;;       (if (not (file-exists? in-file))
;; 	  (begin (print (string-append "Abort: Cannot find file " in-file))
;; 		 (exit 1))
;; 	  (if (file-exists? out-file)
;; 	      (begin (print (string-append "Abort: Output file " in-file
;; 					   " already exists!"))
;; 		     (exit 1))
;; 	      ;; If we've made it here, things are good to
;; 	      ;; go. Start processing the file
;; 	      (with-output-to-file out-file
;; 		(lambda ()
;; 		  (json->csv in-file #:flds field-args
;; 			     #:keep? keep?)))))]
;;      [(json->csv in-file #:flds field-args #:keep? keep?)]))
;;   (exit 0))
(define (task-dispatch #!optional keep)
  (let* ((in-file (alist-ref 'input options))
	 (out-file (alist-ref 'output options))
	 (field-args (nested-string->symbol
		      (map (lambda (x) (string-split x ":"))
			   operands))))

    (cond
     ;; User has supplied an input file name. Read from disk
     [in-file
      (begin
	(if (not (file-exists? in-file))
	     (begin (print (string-append "Abort: Cannot find file " in-file))
		    (exit 1)))

	(with-input-from-file in-file
	  (lambda ()
	    
	    (cond
	     [out-to-file?
	      (begin
		(if (file-exists? out-file)
		    (begin (print (string-append "Abort: Output file " out-file
						 " already exists!"))
			   (exit 1))
		    ;; If we've made it here, things are good to
		    ;; go. Start processing the data
		    (with-output-to-file out-file
		      (lambda ()
			;; User want to see available fields? Print and exit
			(if display-fields? (print-fields))

			(json->csv in-file #:flds field-args
				   #:keep? keep?)))))]
	     ;; User does NOT want to write to disk. Write to stdout
	     [else (begin
		     ;; User want to see available fields? Print and exit
		     (if display-fields? (print-fields))
		     
		     (json->csv in-file #:flds field-args #:keep? keep?))]))))]
     ;; No input file was supplied. Read from stdin 
     [else
      (begin
	(with-input-from-port (current-input-port)
	  (lambda ()
	    ;; User want to see available fields? Print and exit
	    (if display-fields? (print-fields))
	    
	    (cond
	     [out-to-file?
	      (begin
		(if (file-exists? out-file)
		    (begin (print (string-append "Abort: Output file " out-file
						 " already exists!"))
			   (exit 1))
		    ;; If we've made it here, things are good to
		    ;; go. Start processing the file
		    (with-output-to-file out-file
		      (lambda ()
			(json->csv in-file #:flds field-args
				   #:keep? keep?)))))]
	     ;; User does NOT want to write to disk. Write to stdout
	     [else (json->csv in-file #:flds field-args #:keep? keep?)]))))]))
  (exit 0))



;; (define (keep-fields)
;;   (task-dispatch "keep")
;;   (exit 1))

;; (define (remove-fields)
;;   (task-dispatch "remove")
;;   (exit 1))


;;; Just what you think. This gets things done when you don't supply
;;; any command line options
(define (main)
  (task-dispatch)
  (exit 1))

;;; This gets the ball rolling and handles exceptiouns (and must come
;;; last) 
;; (receive (options operands)
;;     (args:parse (command-line-arguments) opts)
;;   (handle-exceptions exn (usage) (main)))
(define options)
(define operands)
(define keep?)
(define display-fields? #f)
(define out-to-file? #f)

(set!-values (options operands)
	     (args:parse (command-line-arguments) opts))

;;(handle-exceptions exn (usage) (main))
(main)

