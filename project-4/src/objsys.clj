(ns objsys)

;; XXX audit use of make-handler to make sure last argument is really
;; its last argument

;;; OBJSYS.SCM
;;;
;;; MIT 6.001                                    Spring, 2005
;;;
;;; This file provides a basic object system, and
;;; a clock for objects in a simulation world.  Additional
;;; utility procedures are also provided.

;; Some terminology:
;;
;; "instance" of an object -- each individual object has
;; its own identity. The instance knows its type, and has
;; a message handler associated with it. One can "ask" an
;; object to do something, which will cause the object
;; to use the message handler to look for a method to
;; handle the request and then invoke the method on the
;; arguments.
;;
;; "make" an object message handler -- makes a new message
;; handler to inherit the state information and methods of the
;; specified class. The message handler is not a full "object
;; instance" in our system; the message handler needs to be
;; part of an instance object (or part of another message
;; handler that is part of an instance object). All
;; procedures that define class objects should take a self pointer (a
;; pointer to the enclosing instance) as the first argument.
;;
;; "create" an object -- this does three things: it makes
;; a new instance of the object, it makes and sets the
;; message handler for that instance, and finally it INSTALL's
;; that new object into the world.
;;
;; "install" an object -- this is a method in the object, by
;; which the object can initialize itself and insert itself
;; into the world by connecting itself up with other related
;; objects in the world.

(declare method?)
(declare method-list?)
(declare handler?)
(declare ask)
(declare method-names)
(declare get-method)
(declare method-lookup)
(declare find-method-from-handler-list)
(declare type-extend)

(defn error [& msg] (throw (IllegalStateException. (apply str (interpose " " msg)))))

;;------------------------------------------------------------
;; Instance

;; instance is a tagged data structure which holds the "self" of a normal
;; object instance.  It passes all messages along to the handler
;; procedure that it contains.
;;

(defn make-instance []
  "An instance is a list of the tag 'instance, and then an atom
  holding the handler function."
  (list 'instance (atom nil)))

(defn tag [tagged-list]
  (first tagged-list))

(defn is-tagged-list-of? [tag-type tagged-list]
  (and (seq? tagged-list) (= (tag tagged-list) tag-type)))

(defn instance? [x]
  (is-tagged-list-of? 'instance x))

(defn instance-handler
  "Returns the handler function for an instance.  See 'make-handler'
  for documentation on handler functions."
  [instance] @(second instance))

(defn set-instance-handler! [instance handler]
  "Overwrite the current handler function on an instance."
  (reset! (second instance) handler))

(defn create-instance [maker & args]
  "An instance is a list of the tag 'instance, and then an atom
  holding the handler function.  'maker' is a function that takes an
  instance and arguments and returns a handler--it's the \"class\"
  function, so to speak."
  (let [instance (make-instance)
        handler (apply maker instance args)]
    (set-instance-handler! instance handler)
    (if (method? (get-method 'INSTALL instance))
      (ask instance 'INSTALL))
    instance))

;;------------------------------------------------------------
;; Handler
;; handler is a procedure which responds to messages with methods
;; it automatically implements the TYPE and METHODS methods.
;;
;;; A handler is a function with metadata :type of :handler.  This
;;; function takes in a message, which is just an all-caps symbol
;;; indicating the method name, and returns the function (unevaluated)
;;; corresponding to that method.

(defn make-handler [typename methods & super-parts]
  "typename is an all-lowercase symbol that identifies the type being
  created.  methods is a structure in the form returned by
  'make-methods'--a tagged object whose only member is a clojure map
  of ALLCAPS method symbol to implementation (function).  super-parts
  is list of the lower-case symbols representing the types which are
  the superclasses.  Standard methods are implemented, including
  'TYPE', which returns a list of all types implemented, including
  super-classes.  METHODS returns a list of the symbols representing
  methods available (does not return implementation)"
  (cond (not (symbol? typename))    ;check for possible programmer errors
        (error "bad typename" typename)

        (not (method-list? methods))
        (error "bad method list" methods)
        
        (and super-parts (not (filter handler? super-parts)))
        (error "bad part list" super-parts)
        
        :else
        ^{:type :handler} (fn [message]
                            (case message
                              TYPE
                              #(type-extend typename super-parts)
                              
                              METHODS
                              #(concat (method-names methods)
                                       (mapcat (fn [x] (ask x 'METHODS))
                                               super-parts))


                              ;; do the actual lookup of the method name
                              (let [entry (method-lookup message methods)]
                                (if entry
                                  (second entry) ;; 
                                  (find-method-from-handler-list message super-parts)))))))

(defn handler? [x]
  (and (fn? x)
       (= :handler (:type (meta x)))))

(defn ->handler [x]
  (cond
    (instance? x)
    (instance-handler x)
    
    (handler? x)
    x

    :else
    (error "I don't know how to make a handler from " x)))


(defn make-methods [& args]
  "Returns a object tagged as 'methods containing a Clojure map of
  METHOD_SYMBOL tofunction implementing that method"
  (let [validate!
        (fn [lst]
          (cond
            (empty? lst)
            nil

            ;; error catching
            (empty? (rest lst))
            (error "unmatched method (name,proc) pair")
            
            (not (symbol? (first lst)))
            (do
              (if (fn? (first lst))
                (println (first lst)))
              (error "invalid method name" (first lst)))
            
            (not (fn? (second lst)))
            (error "invalid method procedure" (second lst))

            :else
            (recur (rest (rest lst)))))]
    (validate! args)
    (list 'methods (apply assoc {} args))))

(defn method-list? [methods]
  (is-tagged-list-of? 'methods methods))

(defn empty-method-list? [methods]
  (empty? (second methods)))

(defn method-lookup [message methods]
  "Returns a list with two elements - method name and method
  implementation (fn), or nil if not found"
  (let [implementation (get (second methods) message)]
    (if implementation
      (list message implementation))))

(defn method-names [methods]
  (keys (second methods)))

;;------------------------------------------------------------
;; Root Object

;; Root object.  It contains the IS-A method.
;; All classes should inherit (directly or indirectly) from root.
;;
(defn root-object [self]
  (make-handler
   'ROOT
   (make-methods
    'IS-A
    (fn [type]
      (some #{type} (ask self 'TYPE))))))

;;------------------------------------------------------------
;; Object Interface
;; ask
;;
;; We "ask" an object to invoke a named method on some arguments.
;;
(declare safe-ask)

(defn ask [object message & args]
  (let [method (get-method message object)]
    (cond (method? method)
          (apply method args)

          :else
          (error "No method for" message 'in
                 (safe-ask 'UNNAMED-OBJECT
                           object 'NAME)))))

;; Safe (doesn't generate errors) method of invoking methods
;; on objects.  If the object doesn't have the method,
;; simply returns the default-value.  safe-ask should only
;; be used in extraordinary circumstances (like error handling).
;;
(defn safe-ask [default-value obj msg & args]
  (let [method (get-method msg obj)]
    (if (method? method)
      (apply ask obj msg args)
      default-value)))

;;--------------------
;; Method Interface
;;
;; Objects have methods to handle messages.

;; Gets the indicated method from the object or objects.
;; This procedure can take one or more objects as
;; arguments, and will return the first method it finds
;; based on the order of the objects.
;;
(defn get-method [message & objects]
  (find-method-from-handler-list message (map ->handler objects)))

(declare no-method)

(defn find-method-from-handler-list [message objects]
  (if (empty? objects)
    (no-method)
    (let [method ((first objects) message)]
      (if (not (= method (no-method)))
        method
        (recur message (rest objects))))))

(defn method? [x]
  (cond (fn? x) true
        (= x (no-method)) false
        :else (error "Object returned this non-message:" x)))

(def no-method
  (let [tag (list 'NO-METHOD)]
    (fn [] tag)))

(declare remove-duplicates)
;; used in make-handler to build the TYPE method for each handler
;;
(defn type-extend [type parents]
  (cons type
        (remove-duplicates
         (mapcat #(ask % 'TYPE) parents))))

;;------------------------------------------------------------
;; Utility procedures

(def rnd (java.util.Random.))
(defn random [n]
  (.nextInt rnd n))

(defn random-number [n]
  ;; Generate a random number between 1 and n
  (+ 1 (random n)))

(defn pick-random [lst]
  (if (empty? lst)
    nil
    (nth lst (random (count lst)))))

(defn find-all [source type]
  (filter (fn [x] (ask x 'IS-A type))
          (ask source 'THINGS)))

(defn delq [item lst]
  (cond (empty? lst) '()
        (= item (first lst)) (delq item (rest lst))
        :else
        (cons (first lst) (delq item (rest lst)))))

(defn filter [predicate lst]
  (cond (empty? lst)
        '()

        (predicate (first lst))
        (cons (first lst) (filter predicate (rest lst)))
        
        :else
        (filter predicate (rest lst))))

(defn fold-right [op init lst]
  (if (empty? lst)
    init
    (op (first lst)
        (fold-right op init (rest lst)))))

(defn remove-duplicates [lst]
  (if (empty? lst)
    '()
    (cons (first lst)
          (remove-duplicates (filter (fn [x]
                                       (not (= x (first lst))))
                                     lst)))))

;; utility for finding all the people in the world
(declare all-rooms)
(defn all-people []
  (mapcat #(find-all % 'person) all-rooms))


;;------------------------------------------------------------
;; Support for Objects in a Simulation World

;;--------------------
;; Clock
;;
;; A clock is an object with a notion of time, which it
;; imparts to all objects that have asked for it.  It does
;; this by invoking a list of CALLBACKs whenever the TICK
;; method is invoked on the clock.  A CALLBACK is an action to
;; invoke on each tick of the clock, by sending a message to an object
(declare create-clock-callback)
(declare screen)
(defn clock [self & args]
  (let [root-part (root-object self)
        name (if (not (empty? args))
               (first args)
               'THE-CLOCK)
        the-time-atom (atom 0)
        callbacks-atom (atom '())
        removed-callbacks-atom (atom '())]
    (make-handler
     'CLOCK
     (make-methods
      'INSTALL
      (fn []
        ;; By default print out clock-ticks
        ;; -- note how we are adding a callback
        ;;    to a method of the clock object
        (ask self 'ADD-CALLBACK
             (create-clock-callback 'tick-printer self 'PRINT-TICK)))
      'NAME      (fn [] name)
      'THE-TIME  (fn [] @the-time-atom)
      'RESET     (fn []
                   (reset! the-time-atom 0)
                   (reset! callbacks-atom '()))
      'TICK
      (fn []
        (reset! removed-callbacks-atom '())
        (doseq [x (reverse @callbacks-atom)]
          (if (not (some #{x} @removed-callbacks-atom))
            (ask x 'ACTIVATE)))
        (swap! the-time-atom inc))
      'ADD-CALLBACK
      (fn [cb]
        (cond (not (ask cb 'IS-A 'CLOCK-CALLBACK))
              (error "Non callback provided to ADD-CALLBACK - actual types are " (ask cb 'TYPE))
              
              (empty? (filter (fn [x] (ask x 'SAME-AS? cb))
                              @callbacks-atom))
              (do
                (swap! callbacks-atom conj cb)
                'added)
              
              :else
              'already-present))
      'REMOVE-CALLBACK
      (fn [obj cb-name]
        (reset! callbacks-atom
                (filter (fn [x]
                          (cond (and (= (ask x 'NAME) cb-name)
                                     (= (ask x 'OBJECT) obj))
                                (do
                                  (swap! removed-callbacks-atom
                                         conj x)
                                  false)
                                :else
                                true))
                        @callbacks-atom))
        'removed)
      'PRINT-TICK
      ;; Method suitable for a callback that prints out the tick
      (fn []
        (ask screen 'TELL-WORLD
             (list "---" (ask self 'NAME) "Tick" (ask self 'THE-TIME) "---"))))
     root-part)))

(defn create-clock [& args]
  (apply create-instance clock args))

;; Clock callbacks
;;
;; A callback is an object that stores a target object,
;; message, and arguments.  When activated, it sends the target
;; object the message.  It can be thought of as a button that executes an
;; action at every tick of the clock.

(defn clock-callback [self name object msg & data]
  (let [root-part (root-object self)]
    (make-handler
     'CLOCK-CALLBACK
     (make-methods
      'INSTALL  (fn [] 'INSTALLED)
      'NAME     (fn [] name)
      'OBJECT   (fn [] object)
      'MESSAGE  (fn [] msg)
      'ACTIVATE (fn [] (apply ask object msg data))
      'SAME-AS? (fn [cb]
                  (and (ask cb 'IS-A 'CLOCK-CALLBACK)
                       (= (ask self 'NAME)
                          (ask cb 'NAME))
                       (= object (ask cb 'OBJECT)))))
     root-part)))

(defn create-clock-callback [name object msg & data]
  (apply create-instance clock-callback name object msg data))

(declare create-clock)
;; Setup global clock object
(def clock (create-clock))

;; Get the current time
(defn current-time []
  (ask clock 'THE-TIME))

;; Advance the clock some number of ticks
(defn run-clock [n]
  (cond (= n 0) 'DONE
        :else
        (do
          (ask clock 'TICK)
          ;; remember that this activates each item in callback list
          (recur (dec n)))))

;; Using the clock:
;;
;; When you want the object to start being aware of the clock
;; (during initialization of autonomous-person, for example),
;; add a callback to the clock which activates a method on the
;; object:
;; (ask clock 'ADD-CALLBACK
;;      (create-clock-callback 'thingy self 'DO-THINGY))
;; The first argument is a name or descriptor of the callback.
;; The second argument is the object to which to send the message.
;; The third argument is the message (method-name) to send.
;; Additional arguments can be provided and they are sent to
;; the object with the message when the callback is activated.
;; In this case, the method do-thingy should be descriptive of
;; the behavior the object will exhibit when time passes.
;; When the object's lifetime expires (sometimes this is taken
;; literally!), it should remove its callback(s) from the clock.
;; This can be done with
;; (ask clock 'REMOVE-CALLBACK
;;      self 'thingy)
;;
;; An example of using callback names and additional arguments:
;; (ask clock 'ADD-CALLBACK
;;      (create-clock-callback 'whoopee me 'SAY '("Whoopee!")))
;; (ask clock 'ADD-CALLBACK
;;      (create-clock-callback 'fun me 'SAY '("I am having fun!")))
;; This causes the avatar to say two things every time the clock
;; ticks.

;;-----------
;; screen
;;
;; This is a singleton object (only one object of this type in
;; existence at any time), which deals with outputting text to
;; the user.
;;
;; If the screen is in deity-mode, the user will hear every message,
;; regardless of the location of the avatar.  If deity-mode is
;; false, only messages sent to the room which contains the avatar
;; will be heard.
;;
;; network-mode is something set only by the network code.

(declare display-net-message)
(declare display-message)
(defn screen [self]
  (let [deity-mode-atom (atom true)
        network-mode-atom (atom false)
        me-atom (atom nil)
        root-part (root-object self)]
    (make-handler
     'SCREEN
     (make-methods
      'TYPE   (fn [] (type-extend 'screen root-part))
      'NAME   (fn [] 'THE-SCREEN)
      'SET-ME (fn [new-me] (reset! me-atom new-me))
      'TELL-ROOM    (fn [room msg]
                      (if (or @deity-mode-atom
                              (= room (safe-ask false @me-atom 'LOCATION)))
                        (if @network-mode-atom
                          (display-net-message msg)
                          (display-message msg))))
      'TELL-WORLD   (fn [msg]
                      (if @network-mode-atom
                        (display-net-message msg)
                        (display-message msg)))
      'DEITY-MODE   (fn [value] (reset! deity-mode-atom value))
      'NETWORK-MODE (fn [value] (reset! network-mode-atom value))
      'DEITY-MODE?  (fn [] @deity-mode-atom))
     root-part)))

(def screen
  (create-instance screen))

;;--------------------
;; Utilities for our simulation world
;;

(defn display [x]
  (print x))

(defn display-message [list-of-stuff]
  (if (not (empty? list-of-stuff)) (newline))
  (doseq [s list-of-stuff]
    (display s)
    (display " "))
  'MESSAGE-DISPLAYED)

(declare server-port)
(declare flush-output)

(defn display-net-message [list-of-stuff]
  (doseq [s list-of-stuff]
    (display s server-port) (display " " server-port))
  (display "\n" server-port)
  (flush-output server-port)
  'MESSAGE-DISPLAYED)

;; Grab any kind of thing from avatar's location,
;; given its name.  The thing may be in the possession of
;; the place, or in the possession of a person at the place.
;; THING-NAMED SHOULD NEVER BE USED IN OBJTYPES OR ANY OBJECT
;; YOU CREATE.
(defn thing-named [name]
  (let [place (ask @me-atom 'LOCATION)
        things (ask place 'THINGS)
        peek-stuff (ask @me-atom 'PEEK-AROUND)
        my-stuff (ask @me-atom 'THINGS)
        all-things (concat things (concat my-stuff peek-stuff))
        things-named (filter (fn [x] (= name (ask x 'NAME)))
                             all-things)]
    (cond (empty? things-named)
          (error "In here there is nothing named" name)
          
          (empty? (rest things-named))   ; just one thing
          (first things-named)

          :else
          (do
            (display-message (list "There is more than one thing named"
                                   name "here. Picking one of them."))
            (pick-random things-named)))))



;;--------------------
;; show
;;
;; Some utilities.
;;
;; Treat these as gifts from the (Scheme) Gods.
;; Don't try to understand these procedures!

;; (declare show-handler)
;; (defn show [obj]
;;   (let [show-guts (fn [obj]
;;                     (format true "INSTANCE ~A~% TYPE: ~A~%" obj (ask obj 'TYPE))
;;                     (show-handler (->handler obj))
;;                     'instance)]
;;     (if (instance? obj)
;;       (show-guts obj)
;;       (show-handler obj))))

;; (defn show-handler [proc]
;;   (let [show-frame (fn [frame depth]
;;                      (define *max-frame-depth* 1)
;;                      (if (global-environment? frame)
;;                        (display (env-name frame))
;;                        (let* ((bindings (environment-bindings frame))
;;                               (parent   (environment-parent frame))
;;                               (names    (cons "Parent frame"
;;                                               (map symbol->string (map car bindings))))
;;                               (values   (cons (env-name parent)
;;                                               (map cadr bindings)))
;;                               (width    (reduce max 0 (map string-length names))))
;;                              (for-each (fn [n v] (pp-binding n v width depth))
;;                                        names values)
;;                              (if (and (not (global-environment? parent))
;;                                       (< depth *max-frame-depth*))
;;                                (show-frame parent (+ depth 1))))))
;;         global-environment? (fn [frame]
;;                               (environment->package frame))
;;         env-name (fn [env]
;;                    (if (global-environment? env) 'GLOBAL-ENVIRONMENT env))
;;         pp-binding (fn [name value width depth]
;;                      (let ((value* (with-string-output-port
;;                                      (fn [port]
;;                                        (if (pair? value)
;;                                          (pretty-print value port false (+ width 2))
;;                                          (display value port))))))
;;                        (display-spaces (* 2 (+ depth 1)))
;;                        (display name) (display ": ")
;;                        (display (make-string (- width (string-length name)) " "))
;;                        (if (pair? value)
;;                          (display (substring value* (+ width 2) (string-length value*)))
;;                          (display value*))
;;                        (newline)))
;;         display-spaces (fn [num]
;;                          (if (> num 0) (begin (display " ") (display-spaces (- num 1)))))]
;;     (if (handler? proc)
;;       (fluid-let ((*unparser-list-depth-limit* 5)
;;                   (*unparser-list-breadth-limit* 6))
;;                  (let ((methods (environment-lookup (procedure-environment proc)
;;                                                     'methods))
;;                        (parts   (environment-lookup (procedure-environment proc)
;;                                                     'super-parts))
;;                        (type    (environment-lookup (procedure-environment proc)
;;                                                     'typename)))
;;                    (format true " HANDLER: ~A~%" proc)
;;                    (format true " TYPE: ~A~%" type)
;;                    (format true "~A~%" (with-output-to-string
;;                                          (fn [] (pretty-print methods))))
;;                    (if (cdr methods)
;;                      (show-frame (procedure-environment (cadadr methods)) 0)
;;                      (format true " PARENTS: ~A~%" parts))
;;                    ;;(display " HANDLER PROCEDURE:\n")
;;                    ;;(pretty-print (procedure-lambda proc) (current-output-port) true 2)
;;                    'handler))
;;       'not-a-handler)))



;;; OBJTYPES.SCM
;;;
;;; MIT 6.001                                    Spring, 2005
;;;
;;; This file defines object types for use in our simulation
;;; world.  The full world is created in setup.scm.

;;--------------------
;; named-object
;;
;; Named objects are the basic underlying object type in our
;; system. For example, persons, places, and things will all
;; be kinds of (inherit from) named objects.
;;
;; Behavior (messages) supported by all named objects:
;;  - Has a NAME that it can return
;;  - Handles an INSTALL message
;;  - Handles a DESTROY message

(declare named-object)

(defn create-named-object [name]      ; symbol -> named-object
  (create-instance named-object name))

(defn named-object [self name]
  (let [root-part (root-object self)]
    (make-handler
     'NAMED-OBJECT
     (make-methods
      'NAME    (fn [] name)
      'INSTALL (fn [] 'installed)
      'DESTROY (fn [] 'destroyed))
     root-part)))

(defn names-of [objects]
;; Given a list of objects, returns a list of their names.
  (map (fn [x] (ask x 'NAME)) objects))


;;--------------------
;; container
;;
;; A container holds THINGS.
;;
;; This class is not meant for "stand-alone" objects; rather,
;; it is expected that other classes will inherit from the
;; container class in order to be able to contain things.
;; For this reason, there is no create-container procedure.

(defn container [self]
  (let [root-part (root-object self)
        things-atom (atom '())]
    (make-handler
     'CONTAINER
     (make-methods
      'THINGS      (fn [] @things-atom)
      'HAVE-THING? (fn [thing]
                     (some #{thing} @things-atom))
      'ADD-THING   (fn [thing]
                     (if (not (ask self 'HAVE-THING? thing))
                       (swap! things-atom conj thing))
                     'DONE)
      'DEL-THING   (fn [thing]
                     (swap! things-atom remove #{thing})
                     'DONE))
     root-part)))


;;--------------------
;; thing
;;
;; A thing is a named-object that has a LOCATION
;;
;; Note that there is a non-trivial INSTALL here.  What does it do?

(declare thing)

(defn create-thing [name location]    ; symbol, location -> thing
  (create-instance thing name (atom location)))

(defn thing [self name location-atom]
  (let [named-part (named-object self name)]
    (make-handler
     'THING
     (make-methods
      'INSTALL  (fn []
                  (ask named-part 'INSTALL)
                  (ask (ask self 'LOCATION) 'ADD-THING self))
      'LOCATION (fn [] @location-atom)
      'DESTROY  (fn []
                  (ask (ask self 'LOCATION) 'DEL-THING self))
      'EMIT     (fn [text]
                  (ask screen 'TELL-ROOM (ask self 'LOCATION)
                       (concat (list "At" (ask (ask self 'LOCATION) 'NAME))
                               text))))
     named-part)))

;;--------------------
;; mobile-thing
;;
;; A mobile thing is a thing that has a LOCATION that can change.

(declare mobile-thing)
(defn create-mobile-thing [name location]
;; symbol, location -> mobile-thing
  (create-instance mobile-thing name location))

(defn mobile-thing [self name location]
  (let [location-atom (atom location)
        thing-part (thing self name location-atom)]
    (make-handler
     'MOBILE-THING
     (make-methods
      'LOCATION  (fn [] @location-atom) ; This shadows message to thing-part!
      'CHANGE-LOCATION
      (fn [new-location]
        (ask @location-atom 'DEL-THING self)
        (ask new-location 'ADD-THING self)
        (reset! location new-location))
      'ENTER-ROOM    (fn [] true)
      'LEAVE-ROOM    (fn [] true)
      'CREATION-SITE (fn [] (ask thing-part 'LOCATION)))
     thing-part)))

;;--------------------
;; place
;;
;; A place is a container (so things may be in the place).
;;
;; A place has EXITS, which are passages from one place
;; to another.  One can retrieve all of the exits of a
;; place, or an exit in a given direction from place.
(declare place)
(defn create-place [name]     ; symbol -> place
  (create-instance place name))

(declare find-exit-in-direction)
(defn place [self name]
  (let [named-part (named-object self name)
        container-part (container self)
        exits-atom (atom '())]
    (make-handler
     'PLACE
     (make-methods
      'EXITS (fn [] @exits-atom)
      'EXIT-TOWARDS
      (fn [direction]
        (find-exit-in-direction @exits-atom direction))
      'ADD-EXIT
      (fn [exit]
        (let [direction (ask exit 'DIRECTION)]
          (if (ask self 'EXIT-TOWARDS direction)
            (error (list name "already has exit" direction))
            (swap! exits-atom conj exit))
          'DONE)))
     container-part named-part)))

;;------------------------------------------------------------
;; exit
;;
;; An exit leads FROM one place TO another in some DIRECTION.
(declare exit)
(defn create-exit [from direction to]
;; place, symbol, place -> exit
  (create-instance exit from direction to))

(defn exit [self from direction to]
  (let [named-object-part (named-object self direction)]
    (make-handler
     'EXIT
     (make-methods
      'INSTALL
      (fn []
        (ask named-object-part 'INSTALL)
        (if (not (empty? (ask self 'FROM)))
          (ask (ask self 'FROM) 'ADD-EXIT self)))
      'FROM         (fn [] from)
      'TO           (fn [] to)
      'DIRECTION    (fn [] direction)
      'USE
      (fn [whom]
        (ask whom 'LEAVE-ROOM)
        (ask screen 'TELL-ROOM (ask whom 'LOCATION)
             (list (ask whom 'NAME)
                   "moves from"
                   (ask (ask whom 'LOCATION) 'NAME)
                   "to"
                   (ask to 'NAME)))
        (ask whom 'CHANGE-LOCATION to)
        (ask whom 'ENTER-ROOM)))
     named-object-part)))

(defn find-exit-in-direction [exits dir]
  "Given a list of exits, find one in the desired direction."
  (cond (empty? exits)
        false

        (= dir (ask (first exits) 'DIRECTION))
        (first exits)
        
        :else
        (find-exit-in-direction (rest exits) dir)))

(defn random-exit [place]
  (pick-random (ask place 'EXITS)))

(declare fold-right)

;;--------------------
;; person
;;
;; There are several kinds of person:
;;   There are autonomous persons, including vampires, and there
;;   is the avatar of the user.  The foundation is here.
;;
;; A person can move around (is a mobile-thing),
;; and can hold things (is a container). A person responds to
;; a plethora of messages, including 'SAY to say something.
;;
(declare person)
(defn create-person [name birthplace] ; symbol, place -> person
  (create-instance person name birthplace))

(defn person [self name birthplace]
  (let [mobile-thing-part (mobile-thing self name birthplace)
        container-part    (container self)
        health-atom       (atom 3)
        strength-atom     (atom 1)]
    (make-handler
     'PERSON
     (make-methods
      'STRENGTH (fn [] @strength-atom)
      'HEALTH (fn [] @health-atom)
      'SAY
      (fn [list-of-stuff]
              (ask screen 'TELL-ROOM (ask self 'LOCATION)
                   (concat (list "At" (ask (ask self 'LOCATION) 'NAME)
                                 (ask self 'NAME) "says --")
                           list-of-stuff))
              'SAID-AND-HEARD)
      'HAVE-FIT
      (fn []
        (ask self 'SAY '("Yaaaah! I am upset!"))
        'I-feel-better-now)

      'PEOPLE-AROUND        ; other people in room...
      (fn []
        (delq self (find-all (ask self 'LOCATION) 'PERSON)))
      
      'STUFF-AROUND         ; stuff (non people) in room...
      (fn []
        (let [in-room (ask (ask self 'LOCATION) 'THINGS)
              stuff (filter (fn [x] (not (ask x 'IS-A 'PERSON))) in-room)]
          stuff))
      'PEEK-AROUND          ; other people's stuff...
      (fn []
        (let [people (ask self 'PEOPLE-AROUND)]
          (fold-right concat '() (map #(ask % 'THINGS) people))))
      'TAKE
      (fn [thing]
        (cond (ask self 'HAVE-THING? thing)  ; already have it
              (do
                (ask self 'SAY (list "I am already carrying"
                                     (ask thing 'NAME)))
                false)
              
              (or (ask thing 'IS-A 'PERSON)
                  (not (ask thing 'IS-A 'MOBILE-THING)))
              (do
                (ask self 'SAY (list "I try but cannot take"
                                     (ask thing 'NAME)))
                false)
              :else
              (let [owner (ask thing 'LOCATION)]
                (ask self 'SAY (list "I take" (ask thing 'NAME)
                                     "from" (ask owner 'NAME)))
                (if (ask owner 'IS-A 'PERSON)
                  (ask owner 'LOSE thing self)
                  (ask thing 'CHANGE-LOCATION self))
                thing)))

      'LOSE
      (fn [thing lose-to]
        (ask self 'SAY (list "I lose" (ask thing 'NAME)))
        (ask self 'HAVE-FIT)
        (ask thing 'CHANGE-LOCATION lose-to))

      'DROP
      (fn [thing]
        (ask self 'SAY (list "I drop" (ask thing 'NAME)
                             "at" (ask (ask self 'LOCATION) 'NAME)))
        (ask thing 'CHANGE-LOCATION (ask self 'LOCATION)))

      'GO-EXIT
      (fn [exit] (ask exit 'USE self))
      'GO
      (fn [direction] ; symbol -> boolean
        (let [exit (ask (ask self 'LOCATION) 'EXIT-TOWARDS direction)]
          (if (and exit (ask exit 'IS-A 'EXIT))
            (ask self 'GO-EXIT exit)
            (do (ask screen 'TELL-ROOM (ask self 'LOCATION)
                     (list "No exit in" direction "direction"))
                false))))
      'SUFFER
      (fn [hits perp]
        (ask self 'SAY (list "Ouch!" hits "hits is more than I want!"))
        (swap! health-atom #(- % hits))
        (if (<= @health-atom 0) (ask self 'DIE perp))
        @health-atom)

      'DIE          ; depends on global variable "death-exit"
      (fn [perp]
        (doseq [item (ask self 'THINGS)]
          (ask self 'LOSE item (ask self 'LOCATION)))
        (ask screen 'TELL-WORLD
             '("An earth-shattering, soul-piercing scream is heard..."))
        (ask self 'DESTROY))
      'ENTER-ROOM
      (fn []
        (let [others (ask self 'PEOPLE-AROUND)]
          (if (seq others)
            (ask self 'SAY (cons "Hi" (names-of others)))))
        true))
     mobile-thing-part container-part)))

;;--------------------
;; autonomous-person
;;
;; activity determines maximum movement
;; miserly determines chance of picking stuff up
(declare autonomous-person)
(defn create-autonomous-person [name birthplace activity miserly]
  (create-instance autonomous-person name birthplace activity miserly))

(defn autonomous-person [self name birthplace activity miserly]
  (let [person-part (person self name birthplace)]
    (make-handler
     'AUTONOMOUS-PERSON
     (make-methods
      'INSTALL
      (fn []
        (ask person-part 'INSTALL)
        (ask clock 'ADD-CALLBACK
             (create-clock-callback 'move-and-take-stuff self
                                    'MOVE-AND-TAKE-STUFF)))
      'MOVE-AND-TAKE-STUFF
      (fn []
        ;; first move
        (loop [moves (random-number activity)]
          (if (= moves 0)
            'done-moving
            (do
             (ask self 'MOVE-SOMEWHERE)
             (recur (- moves 1)))))
        ;; then take stuff
        (if (= (random miserly) 0)
          (ask self 'TAKE-SOMETHING))
        'done-for-this-tick)
      'DIE
      (fn [perp]
        (ask clock 'REMOVE-CALLBACK self 'move-and-take-stuff)
        (ask self 'SAY '("SHREEEEK!  I, uh, suddenly feel very faint..."))
        (ask person-part 'DIE perp))
      'MOVE-SOMEWHERE
      (fn []
        (let [exit (random-exit (ask self 'LOCATION))]
          (if (not (nil? exit)) (ask self 'GO-EXIT exit))))
      'TAKE-SOMETHING
      (fn []
        (let [stuff-in-room (ask self 'STUFF-AROUND)
              other-peoples-stuff (ask self 'PEEK-AROUND)
              pick-from (concat stuff-in-room other-peoples-stuff)]
          (if (seq pick-from)
            (ask self 'TAKE (pick-random pick-from))
            false))))
     person-part)))

;;
;; hall-monitor
;;
(declare hall-monitor)
(defn create-hall-monitor [name birthplace speed irritability]
  (create-instance hall-monitor name birthplace speed irritability))

(defn hall-monitor [self name birthplace speed irritability]
  (let [auto-part (autonomous-person self name birthplace speed 10)]
    (make-handler
     'HALL-MONITOR
     (make-methods
      'INSTALL
      (fn []
        (ask auto-part 'INSTALL)
        (ask clock 'ADD-CALLBACK
             (create-clock-callback 'irritate-students self
                                    'IRRITATE-STUDENTS)))
      'IRRITATE-STUDENTS
      (fn []
        (if (= (random irritability) 0)
          (let [people (ask self 'PEOPLE-AROUND)]
            (if people
              (do
               (ask self 'SAY '("What are you doing still up?"
                                "Everyone back to their rooms!"))
               (doseq [person people]
                 (ask person 'EMIT
                      (list (ask person 'NAME) "goes home to"
                            (ask (ask person 'CREATION-SITE) 'NAME)))
                 (ask person 'CHANGE-LOCATION
                      (ask person 'CREATION-SITE)))
               'grumped)
              (ask self 'SAY '("Grrr... When I catch those students..."))))
          (if (ask self 'PEOPLE-AROUND)
            (ask self 'SAY '("I'll let you off this once...")))))
      'DIE
      (fn [perp]
        (ask clock 'REMOVE-CALLBACK self 'irritate-students)
        (ask auto-part 'DIE perp)))
     auto-part)))

;;
;; troll
;;
(declare troll)
(defn create-troll [name birthplace speed hunger]
  (create-instance troll name birthplace speed hunger))

(defn troll [self name birthplace speed hunger]
  (let [auto-part (autonomous-person self name birthplace speed 10)]
    (make-handler
     'TROLL
     (make-methods
      'INSTALL
      (fn []
              (ask auto-part 'INSTALL)
              (ask clock 'ADD-CALLBACK
                   (create-clock-callback 'eat-people self
                                          'EAT-PEOPLE)))
      'EAT-PEOPLE
      (fn []
              (if (= (random hunger) 0)
                (let [people (ask self 'PEOPLE-AROUND)]
                  (if people
                    (let [victim (pick-random people)]
                      (ask self 'EMIT
                           (list (ask self 'NAME) "takes a bite out of"
                                 (ask victim 'NAME)))
                      (ask victim 'SUFFER (random-number 3) self)
                      'tasty)
                    (ask self 'EMIT
                         (list (ask self 'NAME) "'s belly rumbles"))))
                'not-hungry-now))
      'DIE
      (fn [perp]
              (ask clock 'REMOVE-CALLBACK self 'eat-people)
              (ask auto-part 'DIE perp)))
     auto-part)))

;;
;; spell
;;
(declare spell)
(defn create-spell [name location incant action]
  (create-instance spell name location incant action))

(defn spell [self name location incant action]
  (let [mobile-part (mobile-thing self name location)]
    (make-handler
     'SPELL
     (make-methods
      'INCANT
      (fn [] incant)
      'ACTION
      (fn [] action)
      'USE
      (fn [caster target]
              (action caster target)))
     mobile-part)))

(defn clone-spell [spell newloc]
  (create-spell (ask spell 'NAME)
                newloc
                (ask spell 'INCANT)
                (ask spell 'ACTION)))

;;--------------------
;; avatar
;;
;; The avatar of the user is also a person.

(declare avatar)
(defn create-avatar [name birthplace]
  "symbol, place -> avatar"
  (create-instance avatar name birthplace))

(defn avatar [self name birthplace]
  (let [person-part (person self name birthplace)]
    (make-handler
     'AVATAR
     (make-methods
      'LOOK-AROUND          ; report on world around you
      (fn []
        (let [place (ask self 'LOCATION)
              exits (ask place 'EXITS)
              other-people (ask self 'PEOPLE-AROUND)
              my-stuff (ask self 'THINGS)
              stuff (ask self 'STUFF-AROUND)]
          (ask screen 'TELL-WORLD (list "You are in" (ask place 'NAME)))
          (ask screen 'TELL-WORLD
               (if (empty? my-stuff)
                 '("You are not holding anything.")
                 (concat '("You are holding:") (names-of my-stuff))))
          (ask screen 'TELL-WORLD
               (if (empty? stuff)
                 '("There is no stuff in the room.")
                 (concat '("You see stuff in the room:") (names-of stuff))))
          (ask screen 'TELL-WORLD
               (if (empty? other-people)
                 '("There are no other people around you.")
                 (concat '("You see other people:") (names-of other-people))))
          (ask screen 'TELL-WORLD
               (if (not (empty? exits))
                 (concat '("The exits are in directions:") (names-of exits))
                 ;; heaven is only place with no exits
                 '("There are no exits... you are dead and gone to heaven!")))
          'OK))
      'GO
      (fn [direction]  ; Shadows person's GO
        (let [success? (ask person-part 'GO direction)]
          (if success? (ask clock 'TICK))
          success?))
      'DIE
      (fn [perp]
        (ask self 'SAY (list "I am slain!"))
        (ask person-part 'DIE perp)))
     person-part)))

;;; SETUP.SCM
;;;
;;; MIT 6.001                                    Spring, 2005
;;; PROJECT 4

;;;========================================================================
;;; You can extend this file to extend your world.
;;;========================================================================

;;------------------------------------------------------------
;; Utils to connect places by way of exits

(defn can-go-both-ways [from direction reverse-direction to]
  (create-exit from direction to)
  (create-exit to reverse-direction from))

;;------------------------------------------------------------
;; Create our world...

(defn create-world []
;; Create some places
  (let [ten-250 (create-place 'ten-250)
        lobby-10 (create-place 'lobby-10)
        grendels-den (create-place 'grendels-den)
        barker-library (create-place 'barker-library)
        lobby-7 (create-place 'lobby-7)
        eecs-hq (create-place 'eecs-hq)
        eecs-ug-office (create-place 'eecs-ug-office)
        edgerton-hall (create-place 'edgerton-hall)
        thirtyfour-301 (create-place 'thirtyfour-301)
        stata-center (create-place 'stata-center)
        sixthousandandone-lab (create-place 'sixthousandandone-lab)
        building-13 (create-place 'building-13)
        great-court (create-place 'great-court)
        student-center (create-place 'student-center)
        bexley (create-place 'bexley)
        baker (create-place 'baker)
        legal-seafood (create-place 'legal-seafood)
        graduation-stage (create-place 'graduation-stage)]
    ;; Connect up places
    (can-go-both-ways lobby-10 'up 'down ten-250)
    (can-go-both-ways grendels-den 'up 'down lobby-10)
    (can-go-both-ways ten-250 'up 'down barker-library)
    (can-go-both-ways lobby-10 'west 'east lobby-7)
    (can-go-both-ways lobby-7 'west 'east student-center)
    (can-go-both-ways student-center 'south 'north bexley)
    (can-go-both-ways bexley 'west 'east baker)
    (can-go-both-ways lobby-10 'north 'south building-13)
    (can-go-both-ways lobby-10 'south 'north great-court)
    (can-go-both-ways building-13 'north 'south edgerton-hall)
    (can-go-both-ways edgerton-hall 'up 'down thirtyfour-301)
    (can-go-both-ways thirtyfour-301 'up 'down eecs-hq)
    (can-go-both-ways thirtyfour-301 'east 'west stata-center)
    (can-go-both-ways stata-center 'north 'south stata-center)
    (can-go-both-ways stata-center 'up 'down stata-center)
    (can-go-both-ways eecs-hq 'west 'east eecs-ug-office)
    (can-go-both-ways edgerton-hall 'north 'south legal-seafood)
    (can-go-both-ways eecs-hq 'up 'down sixthousandandone-lab)
    (can-go-both-ways legal-seafood 'east 'west great-court)
    (can-go-both-ways great-court 'up 'down graduation-stage)
    ;; Create some things
    (create-thing 'blackboard ten-250)
    (create-thing 'lovely-trees great-court)
    (create-thing 'flag-pole great-court)
    (create-mobile-thing 'tons-of-code baker)
    (create-mobile-thing 'problem-set ten-250)
    (create-mobile-thing 'recitation-problem ten-250)
    (create-mobile-thing 'sicp stata-center)
    (create-mobile-thing 'engineering-book barker-library)
    (create-mobile-thing 'diploma graduation-stage)

    (list ten-250 lobby-10 grendels-den barker-library lobby-7
          eecs-hq eecs-ug-office edgerton-hall thirtyfour-301 sixthousandandone-lab
          building-13 great-court stata-center
          student-center bexley baker legal-seafood
          graduation-stage)))

;; all spells exist in the chamber-of-stata.  When placing a spell
;; in the outside world, the original spell from the chamber-of stata
;; is cloned (using clone-spell; see objtypes.scm).
;; There are no entrances, exits, or people in the chamber, preventing
;;  the spells there from being stolen.
(defn instantiate-spells []
  (let [chamber (create-place 'chamber-of-stata)]
    (create-spell
     'boil-spell
     chamber
     "habooic katarnum"
     (fn [caster target]
       (ask target 'EMIT
            (list (ask target 'NAME) "grows boils on their nose"))))
    (create-spell
     'slug-spell
     chamber
     "dagnabbit ekaterin"
     (fn [caster target]
       (ask target 'EMIT (list "A slug comes out of" (ask target 'NAME) "'s mouth."))
       (create-mobile-thing 'slug (ask target 'LOCATION))))
    chamber))

(declare chamber-of-stata-atom)
(defn populate-spells [rooms]
  (doseq [room rooms]
    (clone-spell (pick-random (ask @chamber-of-stata-atom 'THINGS)) room)))


(defn populate-players [rooms]
  (let [students (map (fn [name]
                        (create-autonomous-person name
                                                  (pick-random rooms)
                                                  (random-number 3)
                                                  (random-number 3)))
                      '(ben-bitdiddle alyssa-hacker
                                      course-6-frosh lambda-man))
;;uncomment after writing professors
;; (profs (map (lambda (name)
;;       (create-wit-professor name
;;     (pick-random rooms)
;;     (random-number 3)
;;     (random-number 3)))
;;     '(susan-hockfield eric-grimson)))
        monitors (map (fn [name]
                        (create-hall-monitor name
                                             (pick-random rooms)
                                             (random-number 3)
                                             (random-number 3)))
                      '(dr-evil mr-bigglesworth))
        trolls (map (fn [name]
                      (create-troll name
                                    (pick-random rooms)
                                    (random-number 3)
                                    (random-number 3)))
                    '(grendel registrar))]

    (concat students
;;    profs        ;uncomment after writing wit-professor
            monitors trolls)))

(def me-atom (atom 'will-be-set-by-setup))
(def all-rooms-atom (atom 'will-be-set-by-setup))
(def chamber-of-stata-atom (atom 'will-be-set-by-setup))


; (method-lookup message)
; (((->handler clock) 'RESET))
(map ->handler [clock])
;; XXX this doesn't work- returns NO_HANLER
(find-method-from-handler-list 'RESET (map ->handler [clock]))
;; XXX this doesn't work
; (get-method 'RESET clock)
;; XXX this doesn't work
(ask clock 'RESET)

(defn setup [name]
  (ask clock 'RESET)
  (ask clock 'ADD-CALLBACK
       (create-clock-callback 'tick-printer clock 'PRINT-TICK))
  (let [rooms (create-world)]
    (reset! chamber-of-stata-atom (instantiate-spells))

    (populate-spells rooms)

    (populate-players rooms)

;;uncomment after writing chosen one
;;    (create-chosen-one 'hairy-cdr (pick-random rooms)
;;       (random-number 3) (random-number 3))

    (reset! me-atom (create-avatar name (pick-random rooms)))
    (ask screen 'SET-ME me)
    (reset! all-rooms-atom rooms)
    'ready))

;; Some useful example expressions...

(setup 'ben-bitdiddle)
;; (run-clock 5)
;; (ask screen 'DEITY-MODE #f)
;; (ask screen 'DEITY-MODE #t)
;; (ask me 'look-around)
;; (ask me 'take (thing-named 'engineering-book))
;; (ask me 'go 'up)
;; (ask me 'go 'down)
;; (ask me 'go 'north)
;;
;; (show me)
;; (show screen)
;; (show clock)
;; (pp me)
