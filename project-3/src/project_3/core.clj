(ns project-3.core
  (:gen-class)
  (:require [clojure.math.numeric-tower :refer [floor]]))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(defn make-graph-element [node children contents]
  (list :graph-element node children contents))

(defn graph-element? [element]
  (and (list? element) (= :graph-element (first element))))

(defn error [& rest]
  (throw (IllegalStateException. (apply str rest))))

(defn graph-element->node [element]
  (if (not (graph-element? element))
    (error "object not element: " element)
    (second element)))

(defn graph-element->children [element]
  (if (not (graph-element? element))
    (error "object not element: " element)
    (second (rest element))))

(defn graph-element->contents [element]
  (if (not (graph-element? element))
    (error "object not element: " element)
    (nth (rest element) 2)))

(defn make-graph [elements]
  (cons :graph elements))

(defn graph? [graph]
  (and (seq? graph)
       (= :graph (first graph))))

(defn graph-elements [graph]
  (if (not (graph? graph))
    (error "object not graph: " graph)
    (rest graph)))

(defn graph-root [graph]
  (let [elements (graph-elements graph)]
    (if (empty? elements)
      false
      (graph-element->node (first elements)))))

(def test-graph
  (make-graph (list
               (make-graph-element 'a '(b i m) '(some words))
               (make-graph-element 'b '(c d e h) '(more words))               
               (make-graph-element 'c '() '(at c node some words))               
               (make-graph-element 'd '() '())               
               (make-graph-element 'e '(f g) '(and even more words))               
               (make-graph-element 'f '() '())               
               (make-graph-element 'g '() '())               
               (make-graph-element 'h '() '())               
               (make-graph-element 'i '(j k l) '(more words yet))               
               (make-graph-element 'j '() '())               
               (make-graph-element 'k '() '())               
               (make-graph-element 'l '() '()))))

(defn find-graph-element [graph node]
  (letfn [(find [elements]
            (cond (empty? elements)
                  '()
                  
                  (= (graph-element->node (first elements)) node)
                  (first elements)
                  
                  :else
                  (recur (rest elements))))]
    (find (graph-elements graph))))

(defn find-node-children [graph node]
  "Returns list of the nodes in graph that can be reached in one step by "
  "outbound edges from node"
  (let [element (find-graph-element graph node)]
    (if (not (empty? element))
      (graph-element->children element)
      '())))

(defn find-node-contents [graph node]
  "Returns content of the node"
  (let [element (find-graph-element graph node)]
    (if (not (empty? element))
      (graph-element->contents element)
      '())))

(defn find-URL-links 
  "Returns a list of the URLs that are outbound links from URL"
  [web url] 
  (find-node-children web url))

(defn find-URL-text
  "Returns an alphabetized list of all of the words occurring in the document at url"
  [web url]
  (find-node-contents web url))

(def ^:dynamic *search-debug* false)

(defn search [initial-state goal? successors merge graph]
  "initial-state is the start state of the search.  goal? is the
   predicate that determines whether we have reached the goal.
   successors computes form the current state all successor states.
   merge combines new states with the set of states still to explore."
  (letfn [(search-inner [still-to-do]
            (if (empty? still-to-do)
              false
              (let [current (first still-to-do)]
                (if *search-debug*
                  (println :now-at current))
                (if (goal? current)
                  true
                  (recur (merge (successors graph current)
                                (rest still-to-do)))))))]
    (search-inner (list initial-state))))

(defn DFS-simple [start goal? graph]
  (search start
          goal?
          find-node-children
          (fn [new old] (concat new old))
          graph))

(DFS-simple 'a
            #(= % 'l)
            test-graph)


(defn make-index []
  (atom (list :index)))

(defn index? [index-coll]
  (and (seq? index-coll) (= :index (first index-coll))))

(defn assv [key-to-find coll]
  (if (empty? coll)
    false
    (let [p (first coll)
          key (first p)
          value (second p)]
      (if (= key key-to-find)
        p
        (recur key-to-find (rest coll))))))
      
(defn find-entry-in-index [index key]
   (if (not (index? @index))
     (error "object not an index: " @index)
     (assv key (rest @index))))

(defn cadr [coll]
  (first (rest coll)))

(defn find-in-index [index key] ; Index,Key -> list<Val>
  (let [index-entry (find-entry-in-index index key)]
    (if index-entry
      (cadr index-entry)
      '())))

(defn add-to-index [alist new-key new-value]
  (if (empty? alist)
    (list (list new-key (list new-value)))
    (let [entry (first alist)
          key (first entry)
          values (second entry)]
      (if (= key new-key)
        (if (some #{new-value} values)
          alist ; already there
          (cons (list key (conj values new-value)) (rest alist)))
        (cons entry 
              (add-to-index (rest alist) new-key new-value))))))

(defn add-to-index! [index key value] ; Index,Key,Val -> Index
  (swap! index #(cons (first %) (add-to-index (rest %) key value)))
  index)

;; example use
(def test-index (make-index))
(add-to-index! test-index 'key1 'value1)
(add-to-index! test-index 'key2 'value2)
(add-to-index! test-index 'key1 'another-value1)

(find-in-index test-index 'key1)
;; ;Value: (another-value1 value1)

(find-in-index test-index 'key2)
;Value: (value2)

;; Warmup exercise 1

;; doing these as atoms - will generate new list for each.

;; Warmup exercise 2

;; IT has cycles

;; Computer Exercise 1

(defn BFS-simple [start goal? graph]
  (search start
          goal?
          find-node-children
          (fn [new old] (concat old new))
          graph))


(BFS-simple 'a
            #(= % 'l)
            test-graph)

;= true

;; works by trying the existing nodes first before trying new ones it finds

;; Computer Exercise 2

(defn search-with-cycles [initial-state goal? successors merge graph]
  "initial-state is the start state of the search.  goal? is the
   predicate that determines whether we have reached the goal.
   successors computes form the current state all successor states.
   merge combines new states with the set of states still to explore.
   Allows searches fully cyclic graphs without livelock."
  (letfn [(search-inner [still-to-do already-visited]
            (if (empty? still-to-do)
              false
              (let [current (first still-to-do)]
                (if *search-debug*
                  (println :now-at current))
                (if (goal? current)
                  current
                  (if (contains? already-visited current)
                    (recur (rest still-to-do) already-visited)
                    (recur (merge (successors graph current)
                                  (rest still-to-do))
                           (conj already-visited current)))))))]
    (search-inner (list initial-state) #{})))



(defn DFS [start goal? graph]
  (search-with-cycles start
          goal?
          find-node-children
          (fn [new old] (concat new old))
          graph))


(def test-cycle
  (make-graph (list
               (make-graph-element 'a '(b c) '(words for node a))
               (make-graph-element 'b '(c) '(words for node b))
               (make-graph-element 'c '(a) '(words for node c)))))


;(binding [*search-debug* true]
;  (DFS 'a
;       #(= % 'blah)
;       test-cycle))
; :now-at b
; :now-at c
; :now-at a
; :now-at c
; false

(defn BFS [start goal? graph]
  (search-with-cycles start
          goal?
          find-node-children
          (fn [new old] (concat old new))
          graph))

;(binding [*search-debug* true]
;  (BFS 'a
;       #(= % 'blah)
;       test-cycle))
; :now-at b
; :now-at c
; :now-at c
; :now-at a
; false

;;;; generate.scm

;;;; This file, part of project 3, just contains things like the
;;;; list of words in various files.  Don't waste your time looking at
;;;; it.

(defn tosym [x]
  (cond (symbol? x) x
        (string? x) (symbol x)
        (number? x) (symbol (str x))
        :else (error x)))

(def the-web
  (make-graph (list
               (make-graph-element 
                (symbol "http://sicp.csail.mit.edu/")
                (list (symbol "http://sicp.csail.mit.edu/SchemeImplementations")
                      (symbol "http://sicp.csail.mit.edu/psets"))
                (map tosym '("18:30:02" 2004 "6001-WEBMASTER@CSAIL.MIT.EDU" 8 
                             ABOUT ALL AM AND ANNOUNCEMENTS ANSWERS ARE ASSIGNMENT 
                             ASSIGNMENTS BY CALENDAR CAN CHANGE COLLABORATIVE 
                             COMMENTS COMPUTER COPYRIGHT CURRENT DO DOCUMENTATION 
                             EDT FALL FIND FOR GENERAL GET GETTING GUIDELINES HELP
                             HOW I IN INDIVIDUAL INFORMATION INSTITUTE INTERPRETATION 
                             IS LAST LECTURE MASSACHUSETTS ME MICROQUIZZES MODIFIED 
                             MY NEW NOTES OCT OF ON "ON-LINE" ORAL OWN PAST POLICY 
                             POSTED PRESENTATIONS PREVIOUS PROBLEM PROGRAMS 
                             RECITATION RECITATIONS RECORDS RESERVED RIGHTS SCHEME 
                             SECTION SECTIONS SEND SET SETS SITE SOFTWARE STAFF
                             STRUCTURE SUBJECT TECHNOLOGY TELL TERMS THE THIS THU TO
                             UP USE WEEK WHAT WHERE WHICH WORK WRITING)))
               (make-graph-element
                (symbol "http://sicp.csail.mit.edu/SchemeImplementations")
                (list (symbol "http://sicp.csail.mit.edu/getting-help.html")
                      (symbol "http://sicp.csail.mit.edu/lab-use.html")
                      '*the-goal*)
                (map tosym '("11:09" 2004 "3.1" "34-501" "4.0" "6.001" 
                             "6001-WEBMASTER@CSAIL.MIT.EDU" 
                             7 "7.5A" 95 98 A ABOUT ACCESS ADDITION ALL ALSO AN AND ANY 
                             ARE ASSIGNMENTS ASSISTANTS AT ATHENA AVAILABLE BASED BE 
                             BEAR BEAUTY BECAUSE BEEN BEFORE BETWEEN BUT BY CAN 
                             CAPABLE CERTAIN COME COMFORT COMMENTS CONVENIENT COPY 
                             COPYRIGHT COURSE CROWDED DEBUGGER DISK DISTRIBUTIONS
                             DO DOCUMENTATION DONE DRSCHEME DUE EDITOR EDSCHEME EITHER
                             ENJOY ETC EXTENSIONS FEBRUARY FEE FEEL FELLOW FILES FIND 
                             FLOPPY FOLLOWING FOR FREE FROM GET GNU GRANT HAS HAVE HELP
                             HERE HOME IDENTICAL IF IMPLEMENTATIONS IN INC INCLUDED 
                             INCLUDING INSTALL INSTITUTE IS IT JUST LAB LAST LIKE LINUX 
                             LOCKER LOT MAC MACHINE MASSACHUSETTS MAY MIND MODIFIED MUST 
                             NEED NEWER NOT NOTE NT OBTAINED OF OFTEN OLDER ON OPTIONS 
                             OR OTHER OUT OWN PAGE PARTIALLY PC PERHAPS PLATFORMS PLEASE 
                             PM PREPARED PREVIOUS PREVIOUSLY PROBLEM PROGRESS REALIZE 
                             REPRODUCE REQUIRES RESERVED RETURN RICE RIGHTS ROOM ROOT 
                             RUN RUNNING SAVE SCENIC SCHEME SCHEMERS SEE SEMESTERS SEND 
                             SETS SEVERAL SHOULD SINCE SITE SMALL SOMEONE SPRING STAFF 
                             STUDENTS SUGGEST SUPPORTED SYSTEM SYSTEMS TECHNOLOGY TESTED 
                             THAN THAT THE THEM THERE THESE THIS TIME TO TOOK TRANSFERRING
                             TRY UNIVERSITY UNIX UNSUPPORTED UPDATE USE USED USING VERSION
                             VERSIONS VERSON VERY VIRTUALLY WAIVERS WANT WARNED WE WEB 
                             WHERE WHO WILL WINDOWS WITH WORK WORKSTATIONS WOULD YOU YOUR)))
               (make-graph-element
                (symbol "http://sicp.csail.mit.edu/psets")
                '()
                (map tosym '(0 1 15 2004 2 20 "23:32:29 28" 3 4 5 "6.001"
                               "6001-WEBMASTER@CSAIL.MIT.EDU" 98 A ABOUT ALL ALSO AND ARE AS 
                               ASSIGNMENTS ATHENA AVAILABLE BETWEEN BOTH BY CAN COLLABORATIVE 
                               COMMENTS COPYRIGHT DISTRIBUTED EDT FALL FILES FOR FORMAT GET 
                               GHOSTVIEW HELP HERE HOME HOW HTML I IN INSTITUTE IS LAB LAST 
                               LECTURE LOCKER MASSACHUSETTS MODIFIED MY OCT OF ON PAGE POLICY 
                               POSTED POSTSCRIPT PRINTING PROBLEM REQUIRES RESERVED RETURN 
                               RIGHTS SCREEN SEND SEPT SET SETS SHOULD SITE SOLUTIONS SUCH 
                               TECHNOLOGY THE THEY THIS THU TO TRANSFER TUESDAYS UP VIEWER
                               VIEWING WEB WHAT WHERE WHICH WITH WORK WRITE)))
               (make-graph-element
                (symbol "http://sicp.csail.mit.edu/getting-help.html")
                (list (symbol "http://sicp.csail.mit.edu/")
                      (symbol "http://sicp.csail.mit.edu/SchemeImplementations"))
                (map tosym '("09:38:18 1 10 2004" 2 23 24 "339-0052" 4 5 6 "6.001" 
                             "6001-HELP@MIT.EDU"
                             "6001-WEBMASTER@CSAIL.MIT.EDU" 8 "947-2394" 98 A ABLE ABOUT 
                             ADJUSTING ADMINISTRATIVE ADVANTAGE ALL ALSO AM AN AND 
                             ANY ARE AS ASSISTANTS AT ATTENTION BE BEEPER BEING BROUGHT
                             BY CAN CANNOT CELL COMMENTS COMPUTER COPYRIGHT COURSE 
                             CURRENTLY DAY DEMAND DISCUSSION DOES DONE DURING DUTY
                             EARLY EDT EDUCATION EMAIL FALL FEEL FOLLOWS FOR FORUM 
                             FRIDAY GET GETTING GRIPE HAS HAVE HELP HERE HOME HOMEWORK
                             HOURS HOW IF IN INFORMATION INSTALL INSTITUTE INSTRUCTOR 
                             IS IT LAB LAST LECTURERS LINE LOST MASSACHUSETTS MIDNIGHT
                             MIDNITE MIGHT MINORITY MODIFIED MONDAY NATURE NEED NIGHT
                             NOT OF OFFICE ON OPEN OPERATES OR OTHER PAGE PERSONAL 
                             PHONE PHONING PLEASE PM PROBLEM PROBLEMS PROGRAM REACH
                             RECITATION REQUEST RESERVED RESPONSE REST RETURN RIGHTS 
                             SATURDAY SCHEME SCREAMS SECRETARY SEE SEMESTER SEND SEP
                             SET SETUP SHOULD SITE SOME SPECIFIC STAFFED STAFFING 
                             START STATEMENT STUDENT SUNDAY TECHNOLOGY THAT THE THESE
                             THINGS THIS THURSDAY TO TOUCH TRY TUESDAY TUTOR TUTORING
                             UNDERSTAND UNTIL WAIT WAY WE WED WEDNESDAY WHICH WILL
                             WITH YOU YOUR)))
               (make-graph-element
                (symbol "http://sicp.csail.mit.edu/lab-use.html")
                '()
                (map tosym '(1 2004 2 24 "34-501" "4:33" "6.001" 
                               "6001-WEBMASTER@CSAIL.MIT.EDU" 7 8 A
                               ABLE ABOUT ACCESSIBLE ADDITIONAL AFTER ALL ALLOWING ALSO AM AN 
                               AND ANY APPRECIATED ARE ARRANGED AS ASSISTANT AT BE BETWEEN 
                               BOSTON BROUGHT BY CAB CAMPUS CAREFUL CAUTION CLASSES CLEAN CODE
                               COMMENTS COMMON COMPONENTS CONSIDER CONSIDERATION CONTACT 
                               COOPERATION COPYRIGHT CORRIDOR DAY DAYS DETAILS DO DOING
                               DOORS DRINK DURING EDWIN ESCORT ESPECIALLY EXERCISE EXPERIMENT 
                               EXTEND FEBRUARY FOLLOWING FOOD FOR FRATS FUTURE GETTING GO 
                               GREATLY HAVE HERE HOME HOURS HOWEVER IF IN INFORMATION INNER 
                               INSTITUTE INSTRUMENT INTO IS ISSUES KEYBOARDS LAB LABORATORY 
                               LAST LATE LEADING LIVE LOOK MACHINES MANUAL MASSACHUSETTS
                               MIDNIGHT MODIFIED MUST NEAR NEED NIGHT NOT NOTE OF OFF ON ONE 
                               OPEN OPERATION OTHER OUT OUTER PAGE PANIC PAST PATROL PERSONAL
                               PLEASE PM POLICY PROBLEM PROVIDED REMEMBER RESERVED
                               RESPONSIBILITY RETURN RETURNING RIDE RIGHTS RIVER ROOM SAFE
                               SAFETY SCHEME SECOND SEE SEND SENSE SET SHOW SIDE SITE SMOOTHLY
                               SO SPELLED STAFFED STARTED SUCH TAKING TECHNOLOGY TERM THAT 
                               THE THEM THINGS THIS TIME TO UP USE USING WE WEEK WILL WITH 
                               WORKING YOU YOUR YOURSELF ))))))

(def ^:dynamic *all-words*
  (vec
   (map tosym 
        '(0 "09:38:18" 1 10 "11:09" 15 "18:30:02" 2004 2 20 23 "23:32:29"
            24 28 3 3.1 "339-0052" "34-501" 4 "4.0" "4:33" 5 6 "6.001"
            "6001-HELP@MIT.EDU" "6001-WEBMASTER@CSAIL.MIT.EDU" 7 "7.5A" 8 "947-2394"
            95 98 A ABLE ABOUT ACCESS ACCESSIBLE ADDITION ADDITIONAL
            ADJUSTING ADMINISTRATIVE ADVANTAGE AFTER ALL ALLOWING ALSO AM
            AN AND ANNOUNCEMENTS ANSWERS ANY APPRECIATED ARE ARRANGED AS
            ASSIGNMENT ASSIGNMENTS ASSISTANT ASSISTANTS AT ATHENA
            ATTENTION AVAILABLE BASED BE BEAR BEAUTY BECAUSE BEEN BEEPER
            BEFORE BEING BETWEEN BOSTON BOTH BROUGHT BUT BY CAB CALENDAR
            CAMPUS CAN CANNOT CAPABLE CAREFUL CAUTION CELL CERTAIN
            CHANGE CLASSES CLEAN CODE COLLABORATIVE COME COMFORT COMMENTS
            COMMON COMPONENTS COMPUTER CONSIDER CONSIDERATION CONTACT
            CONVENIENT COOPERATION COPY COPYRIGHT CORRIDOR COURSE CROWDED
            CURRENT CURRENTLY DAY DAYS DEBUGGER DEMAND DETAILS DISCUSSION
            DISK DISTRIBUTED DISTRIBUTIONS DO DOCUMENTATION DOES DOING
            DONE DOORS DRINK DRSCHEME DUE DURING DUTY EARLY EDITOR
            EDSCHEME EDT EDUCATION EDWIN EITHER EMAIL ENJOY ESCORT
            ESPECIALLY ETC EXERCISE EXPERIMENT EXTEND EXTENSIONS FALL
            FEBRUARY FEE FEEL FELLOW FILES FIND FLOPPY FOLLOWING FOLLOWS
            FOOD FOR FORMAT FORUM FRATS FREE FRIDAY FROM FUTURE GENERAL
            GET GETTING GHOSTVIEW GNU GO GRANT GREATLY GRIPE GUIDELINES
            HAS HAVE HELP HERE HOME HOMEWORK HOURS HOW HOWEVER HTML I
            IDENTICAL IF IMPLEMENTATIONS IN INC INCLUDED INCLUDING
            INDIVIDUAL INFORMATION INNER INSTALL INSTITUTE INSTRUCTOR
            INSTRUMENT INTERPRETATION INTO IS ISSUES IT JUST KEYBOARDS
            LAB LABORATORY LAST LATE LEADING LECTURE LECTURERS LIKE LINE
            LINUX LIVE LOCKER LOOK LOST LOT MAC MACHINE MACHINES MANUAL
            MASSACHUSETTS MAY ME MICROQUIZZES MIDNIGHT MIDNITE MIGHT MIND
            MINORITY MODIFIED MONDAY MUST MY NATURE NEAR NEED NEW NEWER
            NIGHT NOT NOTE NOTES NT OBTAINED OCT OF OFF OFFICE OFTEN
            OLDER ON "ON-LINE" ONE OPEN OPERATES OPERATION OPTIONS OR ORAL
            OTHER OUT OUTER OWN PAGE PANIC PARTIALLY PAST PATROL PC
            PERHAPS PERSONAL PHONE PHONING PLATFORMS PLEASE PM POLICY
            POSTED POSTSCRIPT PREPARED PRESENTATIONS PREVIOUS PREVIOUSLY
            PRINTING PROBLEM PROBLEMS PROGRAM PROGRAMS PROGRESS PROVIDED
            REACH REALIZE RECITATION RECITATIONS RECORDS REMEMBER
            REPRODUCE REQUEST REQUIRES RESERVED RESPONSE RESPONSIBILITY
            REST RETURN RETURNING RICE RIDE RIGHTS RIVER ROOM ROOT RUN
            RUNNING SAFE SAFETY SATURDAY SAVE SCENIC SCHEME SCHEMERS
            SCREAMS SCREEN SECOND SECRETARY SECTION SECTIONS SEE SEMESTER
            SEMESTERS SEND SENSE SEP SEPT SET SETS SETUP SEVERAL SHOULD
            SHOW SIDE SINCE SITE SMALL SMOOTHLY SO SOFTWARE SOLUTIONS
            SOME SOMEONE SPECIFIC SPELLED SPRING STAFF STAFFED STAFFING
            START STARTED STATEMENT STRUCTURE STUDENT STUDENTS SUBJECT
            SUCH SUGGEST SUNDAY SUPPORTED SYSTEM SYSTEMS TAKING
            TECHNOLOGY TELL TERM TERMS TESTED THAN THAT THE THEM THERE
            THESE THEY THINGS THIS THU THURSDAY TIME TO TOOK TOUCH
            TRANSFER TRANSFERRING TRY TUESDAY TUESDAYS TUTOR TUTORING
            UNDERSTAND UNIVERSITY UNIX UNSUPPORTED UNTIL UP UPDATE USE
            USED USING VERSION VERSIONS VERSON VERY VIEWER VIEWING
            VIRTUALLY WAIT WAIVERS WANT WARNED WAY WE WEB WED WEDNESDAY
            WEEK WHAT WHERE WHICH WHO WILL WINDOWS WITH WORK
            WORKING WORKSTATIONS WOULD WRITE WRITING YOU YOUR YOURSELF))))

(def ^:dynamic *random-web-debugging* false)
(def symbol->string str)
(def number->string str)
(def string->symbol symbol)

(def rnd (java.util.Random.))
(defn random [n]
  (.nextInt rnd n))

(defn memv [object list]
  "return the first pair of list whose car is object"
  (if (empty? list)
    nil
    (if (= object (first list))
      list
      (recur object (rest list)))))

(def memq memv)

(defn vector-length [v]
  (count v))

(defn string-append [ & rest]
  (apply str rest))

(defn string<? [a b]
  (< (compare a b) 0))
(def vector-ref nth)
(def list-ref nth)
(def null? empty?)
(def car first)
(def cadr second)
(defn caddr [coll] (nth coll 2))
(def write-line println)
(def length count)

(defn for-each [f coll]
  (doall (map f coll)))


(defn make-random-web [size guarantee-path-to-all-nodes?]
  (letfn [(->string [obj]
           (if (symbol? obj)
             (symbol->string obj)
             (number->string obj)))
          (choose [n out-of]
            (loop [numbers '()
                   to-go n]
              (if (zero? to-go)
                numbers
                (let [choice (random out-of)]
                  (if (memv choice numbers)
                    (recur numbers to-go)
                    (recur (cons choice numbers)
                      (- to-go 1)))))))]
    (if (> size 200)
      (error "200 nodes is the maximum..."))
    (let [n-total-names (vector-length *all-words*)]
      (if *random-web-debugging* (println "Choosing node names..."))
      (let [node-names
             (map (fn [n]
                    (string->symbol
                     (string-append
                      "http://sicp.csail.mit.edu/"
                      (->string
                       (vector-ref *all-words* n)))))
                  (choose (- size 1) n-total-names))
            nodes (atom '())
            size (- size 1)]
        (letfn
            [(adjoin [entry list]
               (if (memq entry list)
                 list
                 (cons entry list)))
             (make-node [name]
               (let [exits (choose (min 10 (random size)) size)
                     words (choose (+ 50 (random 200)) n-total-names)]
                 (if *random-web-debugging*
                   (write-line (list name (length exits) (length words))))
                 (list name
                       (let [random-exits
                             (map (fn [n] (list-ref node-names n))
                                  exits)]
                         (if (and (not (null? @nodes))
                                  guarantee-path-to-all-nodes?)
                           (adjoin (car
                                    (list-ref @nodes (random (length @nodes))))
                                   random-exits)
                           random-exits))
                       (sort
                        (fn [a b]
                          (string<? (->string a)
                                    (->string b)))
                        (map (fn [n] (vector-ref *all-words* n))
                             words)))))
             (make-node! [name]
               (let [node (make-node name)]
                 (reset! nodes (cons node @nodes))))]
          (if *random-web-debugging* (write-line "creating nodes..."))
          (for-each make-node! node-names)
          (make-node! '*start*)
          (make-graph (map (fn [node]
                             (make-graph-element (car node)
                                                 (cadr node)
                                                 (caddr node)))
                           @nodes)))))))


(defn generate-random-web [size]
  (make-random-web size true))

                                        
(generate-random-web 3)


;(binding [*search-debug* true]
;  (DFS (symbol "http://sicp.csail.mit.edu/")
;       (fn [_] false)
;       the-web))

; :now-at http://sicp.csail.mit.edu/
; :now-at http://sicp.csail.mit.edu/SchemeImplementations
; :now-at http://sicp.csail.mit.edu/getting-help.html
; :now-at http://sicp.csail.mit.edu/
; :now-at http://sicp.csail.mit.edu/SchemeImplementations
; :now-at http://sicp.csail.mit.edu/lab-use.html
; :now-at *the-goal*
; :now-at http://sicp.csail.mit.edu/psets

;(binding [*search-debug* true]
;  (BFS (symbol "http://sicp.csail.mit.edu/")
;       (fn [_] false)
;       the-web))

; :now-at http://sicp.csail.mit.edu/
; :now-at http://sicp.csail.mit.edu/SchemeImplementations
; :now-at http://sicp.csail.mit.edu/psets
; :now-at http://sicp.csail.mit.edu/getting-help.html
; :now-at http://sicp.csail.mit.edu/lab-use.html
; :now-at *the-goal*
; :now-at http://sicp.csail.mit.edu/
; :now-at http://sicp.csail.mit.edu/SchemeImplementations
; false


;; 6. Indexing the web

(find-URL-text the-web (symbol "http://sicp.csail.mit.edu/"))

;; Computer Exercise 3


;; see code above filled in

;; Computer Exercise 4: A Web index

(defn add-word-to-index! [word index url]
  (add-to-index! index word url))

(defn add-document-to-index! [index web url]
  "Add an entry in the index for each word in the contents of the URL,
  so that the key for that entry is a word, and the data in the entry
  is the url"
  (let [words-in-url (find-URL-text web url)]
    (for-each #(add-word-to-index! % index url) words-in-url ))
  true)

(def the-web-index (make-index))
(add-document-to-index! the-web-index
                        the-web
                        (symbol "http://sicp.csail.mit.edu/"))
(find-in-index the-web-index 'HELP)
;= (http://sicp.csail.mit.edu/)

(find-in-index the-web-index '*magic*)
;= ()

;; Compute Exercise 5: Crawling the Web to Build an Index

(defn search-with-cycles-and-visitor [initial-state goal? successors merge graph visit!]
  "initial-state is the start state of the search.  goal? is the
   predicate that determines whether we have reached the goal.
   successors computes form the current state all successor states.
   merge combines new states with the set of states still to explore.
   Allows searches fully cyclic graphs without livelock.
   Calls visitor on each node visited"
  (letfn [(search-inner [still-to-do already-visited]
            (if (empty? still-to-do)
              false
              (let [current (first still-to-do)]
                (if *search-debug*
                  (println :now-at current))
                (if (goal? current)
                  current
                  (if (contains? already-visited current)
                    (recur (rest still-to-do) already-visited)
                    (do
                      (visit! current)
                      (recur (merge (successors graph current)
                                    (rest still-to-do))
                             (conj already-visited current))))))))]
    (search-inner (list initial-state) #{})))


(defn BFS [start goal? graph visit!]
  (search-with-cycles-and-visitor start
          goal?
          find-node-children
          (fn [new old] (concat old new))
          graph
          visit!))

(defn all-documents-from-web [web start-url]
  (let [documents (atom '())]
    (BFS start-url
         (fn [_] false)
         web
         #(swap! documents conj %))
    @documents))

(defn make-web-index [web start-url]
  (let [index (make-index)
        all-documents (all-documents-from-web web start-url)]
    (for-each #(add-document-to-index! index web %) all-documents)
    (fn [keyword] (find-in-index index keyword))))

(def find-documents (make-web-index the-web (symbol "http://sicp.csail.mit.edu/")))

(find-documents 'COLLABORATIVE)
  
;(binding [*search-debug* true]
;  (BFS (symbol "http://sicp.csail.mit.edu/")
;       (fn [_] false)
;       the-web))

; Computer Exercise 6

(defn search-any [web start-node word]
  "Searches or traverses the indicated word (using a breadth-first
   strategy) and returns the first document that it finds that
   contains the given word.  It should stop searching as soon as it
   finds such a document"
  (BFS start-node
       (fn [url] (let [all-words-in-url (find-URL-text web url)]
                   (some #{word} all-words-in-url)))
       web
       (fn [_] true)))
       


(search-any the-web (symbol "http://sicp.csail.mit.edu/psets") 'COLLABORATIVE)

(defn search-all [web start-node word]
  "Searches the *entire* web (using a breadth-first strategy) and
   returns *all* documents that contain the given word"
  (let [all-results (atom '())]
    (BFS start-node
         (fn [_] false)
         web
         (fn [url] (let [all-words-in-url (find-URL-text web url)]
                     (if (some #{word} all-words-in-url)
                       (swap! all-results conj url)))))
    @all-results))

(search-all the-web (symbol "http://sicp.csail.mit.edu/") 'COLLABORATIVE)

;; Computer Exercise 7


(def w10 (generate-random-web 10))
(def w20 (generate-random-web 20))
(def w30 (generate-random-web 20))
(def w100 (generate-random-web 100))
(def the-root-page (symbol "http://sicp.csail.mit.edu/psets"))
(defn print-timing [desc proc]
  (println desc)
  (let [out (time (proc))]
    out))

(defn generate-timings [web-sym]
  (println "Timings for" web-sym)
  (let [web (eval web-sym)
        my-root-page '*start*]
    (print-timing "search-any for 'HELP"
                  #(search-any web my-root-page 'HELP))
    (print-timing "search-any for 'NOT_THERE"
                  #(search-any web my-root-page 'NOT_THERE))
    (print-timing "search-all for 'HELP"
                  #(search-all web my-root-page 'HELP))    
    (let [find-documents (print-timing "make-web-index"
                                       #(make-web-index web my-root-page))]
      (print-timing "find-documents for docs with 'HELP"
                    #(find-documents 'HELP))
      (print-timing "find-documents for docs with 'NOT_THERE"
                    #(find-documents 'NOT_THERE)))))

(defn generate-timings-for-all [web-syms]
  (for-each #(generate-timings %) web-syms))

; (generate-timings-for-all ['w10 'w20 'w30 'w100])

;; indexing once is better than searching more than once.

;; Computer Exercise 8

(defn optimize-value-list [value-list]
  (vec (sort value-list)))

(defn optimize-values [key-values]
  (loop [optimized-index '()
         key-values key-values]
    (if (empty? key-values)
      optimized-index
      (let [first-entry (first key-values)
            key (first first-entry)
            value-list (second first-entry)]
        (recur (conj optimized-index (list key (optimize-value-list value-list)))
               (rest key-values))))))

(defn optimize-keys [key-values]
  ; (println "(optimize-keys " key-values ")")
  (vec (sort-by first key-values)))

(defn optimize-index [index]
  (if (not (index? @index))
    (error "object not an index: " @index)
    (let [list-with-optimized-values (optimize-values (rest @index))]
      (list :optimized-index
            (optimize-keys list-with-optimized-values)))))

(defn make-regular-web-index [web start-url]
  (let [index (make-index)
        all-documents (all-documents-from-web web start-url)]
    (for-each #(add-document-to-index! index web %) all-documents)
    index))


(defn find-entry-in-optimized-entries [k entries start end]
  ; (println "(find-entry-in-optimized-entries k=" k ", start=" start ", end=" end)
;  (println "entries=" entries)
  (let [midpoint (+ start (floor (/ (- end start) 2)))
        midpoint-entry (nth entries midpoint)
        midpoint-key (first midpoint-entry)
        midpoint-values (second midpoint-entry)]
    ; (println "find-entry-in-optimized-entries: start=" start ", end=" end ", midpoint=" midpoint "midpoint-key=" midpoint-key)
    (if (= midpoint-key k)
      midpoint-values
      (if (= midpoint start end)
        nil ; we're done
        (do
          ;(println "midpoint-key=" midpoint-key ", k=" k)
          (if (< (compare (str midpoint-key) (str k)) 0)
            (recur k entries (inc midpoint) end)
            (recur k entries start (dec midpoint))))))))

(defn find-entry-in-optimized-index [optind k] ; Optimized-Index, Key -> List<Val>
  ; (println "find-entry-in-optimized-index - optind is " optind)
  (let [entries (first (rest optind))]
    (find-entry-in-optimized-entries k entries 0 (dec (count optind)))))

(defn generate-timings [web-sym]
  (println)
  (println "Timings for" web-sym)
  (let [web (eval web-sym)
        my-root-page '*start*]
    (print-timing "search-any for 'HELP"
                  #(search-any web my-root-page 'HELP))
    (print-timing "search-any for 'NOT_THERE"
                  #(search-any web my-root-page 'NOT_THERE))
    (print-timing "search-all for 'HELP"
                  #(search-all web my-root-page 'HELP))    
    (let [find-documents (print-timing "make-web-index"
                                       #(make-web-index web my-root-page))]
      (print-timing "find-documents for docs with 'HELP"
                    #(find-documents 'HELP))
      (print-timing "find-documents for docs with 'NOT_THERE"
                    #(find-documents 'NOT_THERE)))
    (let [index (print-timing "make-regular-web-index"
                              #(make-regular-web-index web my-root-page))
          optimized-index (print-timing "optimize-index"
                                        #(optimize-index index))]
      ; (println "Regular index is " index)
      ; (println "Optimized index is " optimized-index)
      (print-timing "10000x find-entry-in-optimized-index for docs with 'HELP"
                    #(dotimes [n 10000] (find-entry-in-optimized-index optimized-index 'HELP)))
      (print-timing "10000x find-entry-in-index for docs with 'HELP"
                    #(dotimes [n 10000] (find-entry-in-index index 'HELP)))
       
      )))

(generate-timings-for-all ['w10 'w20 'w30 'w100])
