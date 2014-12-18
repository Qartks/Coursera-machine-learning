;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname trees) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; Problem Set 05 - Question 1

;; Designing and implementing a system for a graphical interface for trees.
;; The system will allow the user to create and manipulate trees on a
;; canvas.

;; start with run(any)
;; The argument is ignored and an initial world is generated

;; e.g.  run (0)

(require "extras.rkt")
(require "sets.rkt")
(require 2htdp/image)
(require 2htdp/universe)
(require rackunit)

(provide run
         initial-world
         world-after-mouse-event
         world-after-key-event
         world-to-roots
         node-to-center
         node-to-sons
         node-to-selected?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MAIN FUNCTION ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; run :  Any -> World
;; GIVEN: any value
;; EFFECT: runs a copy of an initial world
;; RETURNS: the final state of the world.  The given value is ignored.

(define (run any)
  (big-bang (initial-world any)
            (on-mouse world-after-mouse-event)
            (on-key world-after-key-event)
            (on-draw world-to-scene)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CONSTANTS  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Canvas
(define CANVAS-BASE 0)
(define CANVAS-HEIGHT 400)
(define CANVAS-WIDTH 400)
(define CANVAS-HALF-HEIGHT (/ CANVAS-HEIGHT 2))
(define CANVAS-HALF-WIDTH (/ CANVAS-WIDTH 2))
(define EMPTY-SCENE (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))

;; Node
(define NODE-SIDE 20)
(define NODE-HALF-SIDE (/ NODE-SIDE 2))

;; Colors
(define NODE-COLOR "green")
(define NODE-COLOR-NO-ROOM "red")
(define LINE-COLOR "blue")
(define VERTICLE-LINE-COLOR "red")

;; Image Modes
(define NODE-SELECTED-MODE "solid")
(define NODE-UNSELECTED-MODE "outline")

;; MouseEvents
(define BUTTON-DOWN-EVENT "button-down")
(define DRAG-EVENT "drag")
(define BUTTON-UP-EVENT "button-up")
(define OTHER-MOUSE-EVENT "enter")

;; KeyEvents
(define N-KEY-EVENT "n")
(define D-KEY-EVENT "d")
(define T-KEY-EVENT "t")
(define U-KEY-EVENT "u")
(define OTHER-KEY-EVENT "q")

;; Selection Modes
(define SELECTED true)
(define UNSELECTED false)


;; Square Modes
(define SELECTED-SQUARE 
  (square NODE-SIDE NODE-SELECTED-MODE NODE-COLOR))

(define UNSELECTED-SQUARE 
  (square NODE-SIDE NODE-UNSELECTED-MODE NODE-COLOR))

(define SELECTED-SQUARE-NO-ROOM 
  (square NODE-SIDE NODE-SELECTED-MODE NODE-COLOR-NO-ROOM))

;; List
(define EMPTY-LIST empty)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DATA DEFINITION  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Node
(define-struct node (x-pos y-pos selected? sub-tree))

;; A Node is a
;;   (make-node Real Real Boolean ListOf<Node>)
;; Interp:
;; x-pos is the x co-ordinate of the center of the Node on the canvas.
;; y-pos is the y co-ordinate of the center of the Node on the canvas.
;; selected? represents whether or not a Node is selected.
;; sub-tree is the ListOf<Node> which will be the children of the current 
;; Node.

;; Template:
;; node-fn : Node -> ??
;; (define (node-fn n)
;;   (... (node-x-pos n)
;;        (node-y-pos n)
;;        (node-y-selected? n)
;;        (lon-fn(node-sub-tree n))))

;; Examples:
(define node-initial (make-node CANVAS-HALF-WIDTH NODE-HALF-SIDE UNSELECTED
                                EMPTY-LIST))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; a ListOf<Node> (LoN) is one of
;; -- empty 
;; Interp: The list can be empty i.e. have no nodes.
;; -- (cons Node ListOf<Node>)
;; Interp: It represents a sequence whose first element is a Node and whose
;; other elements are represented by ListOf<Node>.

;; Template:
;; lon-fn : ListOf<Node> -> ??
;; (define (lon-fn lon)
;;   (cond
;;     [(empty? lon) ...]
;;     [else (...
;;            (node-fn (first lon))
;;            (lon-fn (rest lon)))]))

;; Examples:
(define list-initial (cons (make-node CANVAS-HALF-WIDTH 
                                      NODE-HALF-SIDE UNSELECTED EMPTY-LIST)
                           EMPTY-LIST))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;World
(define-struct world(root-node))

;; A World is a 
;;    (make-world ListOf<Node>)
;; Interp:
;; root-node is the sequence of nodes which will be the trees
;; displayed on the canvas

;; Template:
;; world-fn : World -> ??
;; (define (world-fn w)
;;   (... (lon-fn(world-root-node w))))

;; Examples:
(define empty-world (make-world EMPTY-LIST))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CONSTANTS FOR TESTING ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Root Nodes
(define SELECTED-ROOT-NODE 
  (make-node CANVAS-HALF-WIDTH NODE-HALF-SIDE SELECTED EMPTY-LIST))

(define UNSELECTED-ROOT-NODE 
  (make-node CANVAS-HALF-WIDTH NODE-HALF-SIDE UNSELECTED EMPTY-LIST))

;; Child Nodes
(define SELECTED-NODE 
  (make-node 250 250 SELECTED EMPTY-LIST))

(define UNSELECTED-NODE 
  (make-node 250 250 UNSELECTED EMPTY-LIST))

;; List of Root Nodes
(define LIST-OF-UNSELECTED-ROOT
  (list UNSELECTED-ROOT-NODE))

(define LIST-OF-SELECTED-ROOT
  (list SELECTED-ROOT-NODE))

;; List of Child Nodes
(define LIST-OF-UNSELECTED-NODE
  (list UNSELECTED-NODE))

(define LIST-OF-SELECTED-NODE
  (list SELECTED-NODE))

;; List of Root having one child
(define LIST-OF-NODES
  (list SELECTED-ROOT-NODE UNSELECTED-NODE))

(define WORLD-OF-UNSELECTED-ROOT
  (make-world LIST-OF-UNSELECTED-ROOT))

(define WORLD-OF-SELECTED-ROOT
  (make-world LIST-OF-SELECTED-ROOT))

(define WORLD-OF-UNSELECTED-NODE
  (make-world LIST-OF-UNSELECTED-NODE))

(define WORLD-OF-SELECTED-NODE
  (make-world LIST-OF-SELECTED-NODE))

(define WORLD-OF-NODES
  (make-world LIST-OF-NODES))

(define WORLD-OF-NODES-AT-BOUNDARY
  (make-world 
   (list (make-node NODE-HALF-SIDE CANVAS-HALF-WIDTH SELECTED 
                    (list(make-node NODE-HALF-SIDE
                               (+ CANVAS-HALF-WIDTH 
                                  (* 3 NODE-SIDE))
                               UNSELECTED EMPTY-LIST))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FUNCTIONS ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; initial-world : Any -> World
;; GIVEN: any value
;; RETURNS: an initial world.  The given value is ignored.
;; Examples:
;; (initial-world 100) -> (make-world EMPTY-LIST)
;; STRATEGY: Function Composition

(define (initial-world any)
  (make-world empty))

;; TESTS
(begin-for-test
  (check-equal? (initial-world 100) 
                (make-world EMPTY-LIST)
                "returns a world with no nodes"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-mouse-event : World Integer Integer MouseEvent -> World
;; GIVEN: a World, a location, and a MouseEvent
;; RETURNS: the state of the world as it should be 
;;          following the given mouse event at that location.
;; EXAMPLES:
;; (world-after-mouse-event WORLD-OF-SELECTED-ROOT 200 10 BUTTON-UP-EVENT)
;; -> WORLD-OF-UNSELECTED-ROOT
;; STRATEGY: Structural Decomposition on w : World

(define (world-after-mouse-event w mx my mev)
  (make-world (root-node-after-mouse-event (world-root-node w) mx my mev)))

;; TESTS: At the end of all helper functions.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; root-node-after-mouse-event : 
;; ListOf<Node> Integer Integer MouseEvent -> ListOf<Node>
;; GIVEN: a ListOf<Node>, location of the mouse pointer on the screen
;;        and a MouseEvent.
;; RETURNS: a list of nodes similar to the given one after the mouse event
;;          has taken place.
;; EXAMPLES: 
;; (root-node-after-mouse-event LIST-OF-UNSELECTED-ROOT 210 15 
;; BUTTON-DOWN-EVENT)
;; -> LIST-OF-SELECTED-ROOT
;; STRATEGY: Cases on mev : MouseEvent

(define (root-node-after-mouse-event lon mx my mev)
  (cond 
    [(string=? mev BUTTON-DOWN-EVENT) (nodes-after-button-down lon mx my)]
    [(string=? mev DRAG-EVENT) (nodes-after-drag lon mx my)]
    [(string=? mev BUTTON-UP-EVENT) (nodes-after-button-up lon mx my)]
    [else lon]))

;; TESTS: Covered under the main function world-after-mouse-event

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; nodes-after-button-down : ListOf<Node> Integer Integer -> ListOf<Node>
;; GIVEN: a ListOf<Node> and location of the mouse pointer on the screen
;; RETURNS: a list of nodes similar to the given one after the button down 
;;          event.
;; EXAMPLES:
;; (nodes-after-button-down LIST-OF-UNSELECTED-ROOT 201 11)
;; -> LIST-OF-SELECTED-ROOT
;; STRATEGY: Higher-order Function Composition

(define (nodes-after-button-down lon mx my)
  (map 
   ;; Node -> ListOf<Node>
   ;; GIVEN: a Node
   ;; RETURNS: a ListOf<Node> after the button down event
   (lambda(x)
     (single-node-after-button-down x mx my))
   lon))

;; TESTS: Covered under the main function world-after-mouse-event

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; single-node-after-button-down : Node Integer Integer -> Node
;; GIVEN: a Node and location of the mouse pointer on the screen
;; RETURNS: a node similar to the given one after the button down event.
;; EXAMPLES:
;; (single-node-after-button-down UNSELECTED-ROOT-NODE 210 13)
;; -> SELECTED-ROOT-NODE
;; STRATEGY: Function Composition

(define (single-node-after-button-down n mx my)
  (if (location-inside-node? n mx my)    
      (selected-node-after-button-down n mx my)
      (unselected-node-after-button-down n mx my)))

;; TESTS: Covered under the main function world-after-mouse-event

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; selected-node-after-button-down : Node Integer Integer -> Node
;; GIVEN: a Node and the location of the mouse position on the screen
;; RETURNS: a selected node after the button down event
;; EXAMPLES:
;; (selected-node-after-button-down UNSELECTED-ROOT-NODE 202 14)
;; ->SELECTED-ROOT-NODE
;; STRATEGY: Structural Decomposition on n : Node
(define (selected-node-after-button-down n mx my)
  (make-node (node-x-pos n)
             (node-y-pos n)
             SELECTED
             (nodes-after-button-down (node-sub-tree n)
                                      mx my)))

;; TESTS: Covered under the main function world-after-mouse-event

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; unselected-node-after-button-down : Node Integer Integer -> Node
;; GIVEN: a Node and the location of the mouse position on the screen
;; RETURNS: a unselected node after the button down event
;; EXAMPLES:
;; (unselected-node-after-button-down UNSELECTED-ROOT-NODE 205 10)
;; -> UNSELECTED-ROOT-NODE
;; STRATEGY: Structural Decomposition on n : Node
(define (unselected-node-after-button-down n mx my)
  (make-node (node-x-pos n)
             (node-y-pos n)
             (node-selected? n)
             (nodes-after-button-down (node-sub-tree n)
                                      mx my)))

;; TESTS: Covered under the main function world-after-mouse-event

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; location-inside-node? : Node Integer Integer -> Boolean
;; GIVEN: a Node and location of the mouse pointer on the screen.
;; RETURNS: true iff the mouse location is inside the given node.
;; EXAMPLES:
;; (location-inside-node? SELECTED-ROOT-NODE 206 16)
;; -> true
;; STRATEGY: Structural Decomposition on n : Node
(define (location-inside-node? n x y)
  (and 
   (<= (- (node-x-pos n) NODE-HALF-SIDE)
       x
       (+ (node-x-pos n) NODE-HALF-SIDE))
   (<= (- (node-y-pos n) NODE-HALF-SIDE)
       y
       (+ (node-y-pos n) NODE-HALF-SIDE))))

;; TESTS: Covered under the main function world-after-mouse-event

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; nodes-after-button-up : ListOf<Node> Integer Integer -> ListOf<Node>
;; GIVEN: a ListOf<Node> and location of the mouse pointer on the screen
;; RETURNS: a list of nodes similar to the given one after the button up 
;;          event.
;; EXAMPLES:
;; (nodes-after-button-up LIST-OF-SELECTED-ROOT 208 18)
;; LIST-OF-UNSELECTED-ROOT
;; STRATEGY: Higher-order Function Compostion

(define (nodes-after-button-up lon mx my)
  (map 
   ;; Node -> ListOf<Node>
   ;; GIVEN: a Node
   ;; RETURNS: a list of nodes after the button up event
   (lambda(x)
     (single-node-after-button-up x mx my))
   lon))

;; TESTS: Covered under the main function world-after-mouse-event

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; single-node-after-button-up : Node Integer Integer -> Node
;; GIVEN: a Node and location of the mouse pointer on the screen
;; RETURNS: a node similar to the given one after the button up event.
;; EXAMPLES:
;; (single-node-after-button-up SELECTED-ROOT-NODE 200 10)
;; -> UNSELECTED-ROOT-NODE
;; STRATEGY: Structural Decomposition on n : Node

(define (single-node-after-button-up n mx my)
  (make-node (node-x-pos n)
             (node-y-pos n)
             UNSELECTED
             (nodes-after-button-up (node-sub-tree n) mx my)))

;; TESTS: Covered under the main function world-after-mouse-event

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; nodes-after-drag: ListOf<Node> Integer Integer -> ListOf<Node>
;; GIVEN: a ListOf<Node> and location of the mouse pointer on the screen
;; RETURNS: a ListOf<Node> similar to the given one after the drag 
;;          event.
;; EXAMPLES:
;; (nodes-after-drag LIST-OF-SELECTED-ROOT 210 10)
;; -> (list (make-node 210 10 true empty))
;; STRATEGY: Higher-order Function Composition

(define (nodes-after-drag lon mx my)
  (map 
   ;; Node -> ListOf<Node>
   ;; GIVEN: a Node
   ;; RETURNS: a list of nodes after the drag event by shifting the 
   ;; given nodes to the mouse position.
   (lambda(x)
     (single-node-after-drag x mx my))
   lon))


;; TESTS: Covered under the main function world-after-mouse-event

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; single-node-after-drag : Node Integer Integer -> Node
;; GIVEN: a Node and location of the mouse pointer on the screen
;; RETURNS: a node similar to the given one after the button up event.
;; EXAMPLES:
;; (single-node-after-drag SELECTED-ROOT-NODE 205 10)
;; -> (make-node 205 10 true empty)
;; STRATEGY: Structural Decomposition on n : Node
(define (single-node-after-drag n mx my)
  (if (node-selected? n)
      (drag-selected-node n mx my)
      (return-unselected-node n mx my)))

;; TESTS: Covered under the main function world-after-mouse-event

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; drag-selected-node : Node Integer Integer -> Node
;; GIVEN: a Node and the position of the mouse pointer on screen
;; RETURNS: the selected node after the drag event
;; EXAMPLES:
;; (drag-selected-node SELECTED-ROOT-NODE 204 14)
;; -> (make-node 204 14 true empty)
;; STRATEGY: Structural Decomposition on n : Node
(define (drag-selected-node n mx my)
  (make-node mx
             my
             (node-selected? n)
             (drag-sub-tree-with-selected-node (node-sub-tree n) 
                                               (- mx (node-x-pos n))
                                               (- my (node-y-pos n)))))

;; TESTS: Covered under the main function world-after-mouse-event

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; return-unselected-node : Node Integer Integer -> Node
;; GIVEN: a Node and the location of the mouse pointer on the screen
;; RETURNS: the same Node after the drag event as it is unselected
;; EXAMPLES:
;; (return-unselected-node UNSELECTED-ROOT-NODE 211 11)
;; -> UNSELECTED-ROOT-NODE
;; STRATEGY: Structural Decomposition on n : Node
(define (return-unselected-node n mx my)
  (make-node (node-x-pos n)
             (node-y-pos n)
             (node-selected? n)
             (nodes-after-drag (node-sub-tree n) mx my)))

;; TESTS: Covered under the main function world-after-mouse-event

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; drag-sub-tree-with-selected-node : 
;;      ListOf<Node> Real Real -> ListOf<Node>
;; GIVEN: a ListOf<Node> and the offset between the mouse pointer and the
;;        selected node
;; RETURNS: a Listof<Node>, after the drag event has taken place
;; EXAMPLES:
;; (drag-sub-tree-with-selected-node LIST-OF-SELECTED-ROOT 201 3)
;; -> (list (make-node 401 13 true empty))
;; STRATEGY: Higher-order Function Compositon

(define (drag-sub-tree-with-selected-node lon dx dy)
  (map 
   ;; Node -> ListOf<Node>
   ;; GIVEN: a Node
   ;; RETURNS: a ListOf<Node> after the drag event
   ;; with all the nodes for a given parent dragged to the new position
   (lambda(n)
     (drag-single-node-with-selected-node n dx dy))
   lon))

;; TESTS: Covered under the main function world-after-mouse-event

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; drag-single-node-with-selected-node : Node Real Real -> Node
;; GIVEN: a Node and the offset between the mouse pointer and the
;;        selected node
;; RETURNS: the Node with the updated postion according the the parents 
;;          position
;; EXAMPLES:
;; (drag-single-node-with-selected-node SELECTED-ROOT-NODE 205 5)
;; -> (make-node 405 15 true empty)
;; STRATEGY: Structural Decomposition on n : Node

(define (drag-single-node-with-selected-node n dx dy)
  (make-node (+ (node-x-pos n) dx)
             (+ (node-y-pos n) dy)
             (node-selected? n)
             (drag-sub-tree-with-selected-node (node-sub-tree n) dx dy)))

;; TESTS: Covered under the main function world-after-mouse-event

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TESTS for world-after-mouse-event

(begin-for-test
  (check-equal? (world-after-mouse-event WORLD-OF-SELECTED-ROOT 200 10 
                                         BUTTON-UP-EVENT)
                WORLD-OF-UNSELECTED-ROOT
                "returns a world with an unselected ball at (200,10) after
the button up event")
  (check-equal? (world-after-mouse-event WORLD-OF-UNSELECTED-ROOT 210 12 
                                         BUTTON-DOWN-EVENT)
                WORLD-OF-SELECTED-ROOT
                "returns a world with a selected ball at (210,12) after
the button down event")
  (check-equal? (world-after-mouse-event WORLD-OF-UNSELECTED-ROOT 400 400 
                                         BUTTON-DOWN-EVENT)
                WORLD-OF-UNSELECTED-ROOT
                "returns the same world for the point (400,400) which is 
outside the node")
  (check-equal? (world-after-mouse-event WORLD-OF-SELECTED-ROOT 210 12 
                                         DRAG-EVENT)
                (make-world (list (make-node 210 12 SELECTED EMPTY-LIST)))
                "returns a world with a selected ball at (210,12) after
the drag event")
  (check-equal? (world-after-mouse-event WORLD-OF-UNSELECTED-ROOT 210 12 
                                         DRAG-EVENT)
                WORLD-OF-UNSELECTED-ROOT
                "returns the same world for unselected ball after the drag
event")
  (check-equal? (world-after-mouse-event WORLD-OF-SELECTED-ROOT 150 30 
                                         OTHER-MOUSE-EVENT)
                WORLD-OF-SELECTED-ROOT
                "returns the same world for any other mouse event")
  (check-equal? (world-after-mouse-event WORLD-OF-NODES-AT-BOUNDARY 150 30 
                                         DRAG-EVENT)
                (make-world (list 
                             (make-node 150 30 true 
                                        (list (make-node 150 90 false 
                                                         empty)))))
                "returns the same world for any other mouse event"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-key-event : World KeyEvent -> World
;; GIVEN: a World and a key event
;; RETURNS: the state of the world as it should
;;          be following the given key event
;; Examples:
;; (world-after-key-event (initial-world 19) "n") 
;;              -> (make-world tree-with-one-node)
;; STRATEGY: Structural Decomposition on w : World
(define (world-after-key-event w kev)
  (make-world (nodes-after-key-event (world-root-node w)
                                     kev)))

;; TESTS : After all helper functions

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; nodes-after-key-event : ListOf<Node> KeyEvent-> ListOf<Node>
;; GIVEN: a ListOf<Node>s and a KeyEvent
;; RETURNS: a ListOf<Node>s following the given KeyEvent
;; EXAMPLES:
;; (nodes-after-key-event LIST-OF-SELECTED-ROOT U-KEY-EVENT)
;; -> empty
;; STRATEGY: Cases on KeyEvent
(define (nodes-after-key-event lon kev)
  (cond 
    [(string=? kev T-KEY-EVENT) (add-new-root-node lon)]
    [(string=? kev N-KEY-EVENT) (add-tree-to-node lon)]
    [(string=? kev D-KEY-EVENT) (delete-tree-from-node lon)]
    [(string=? kev U-KEY-EVENT) (delete-nodes-from-upper-half lon)]
    [else lon]))

;; TESTS: Covered under the main function nodes-after-key-event

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; delete-nodes-from-upper-half : ListOf<Node> -> ListOf<Node>
;; GIVEN: a list of nodes
;; RETURNS: a list of nodes, after the u key event has taken place
;;          i.e. deletes all nodes in the upper half of the canvas
;; EXAMPLES:
;; (delete-nodes-from-upper-half LIST-OF-SELECTED-ROOT)
;; -> empty
;; STRATEGY: Structural Decomposition on n : Node

(define (delete-nodes-from-upper-half lon)
  (foldr 
   ;; Node ListOf<Node> -> ListOf<Node>
   ;; GIVEN: a Node and the result of subtree for a given root
   ;; RETURNS: a ListOf<Node> after the u key event depending on
   ;; the position of the given node.
   (lambda(x y)
     (if (< (node-y-pos x) CANVAS-HALF-HEIGHT)
         y
         (cons (new-single-node-lower-half x) y)))
   empty
   lon))

;; TESTS: Covered under the main function nodes-after-key-event

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; new-single-node-lower-half : Node -> Node
;; GIVEN: A node
;; RETURNS: a Node, similar to the given, while also calling 
;;          the function delete-tree-from-node on its sub-tree
;; EXAMPLES:
;; (new-single-node-lower-half UNSELECTED-ROOT-NODE)
;; -> UNSELECTED-ROOT-NODE
;; STRATEGY: Structural Decomposition on n : Node

(define (new-single-node-lower-half n)
  (make-node (node-x-pos n)
             (node-y-pos n)
             (node-selected? n)
             (delete-nodes-from-upper-half (node-sub-tree n))))

;; TESTS: Covered under the main function nodes-after-key-event

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; delete-tree-from-node : ListOf<Node> -> ListOf<Node>
;; GIVEN: a ListOf<Node>
;; RETURNS: a ListOf<Node> similar to the given, but after the 
;;          key event 'd' has taken place
;; EXAMPLES:
;; (delete-tree-from-node LIST-OF-NODES)
;; -> LIST-OF-UNSELECTED-NODE
;; STRATEGY: Structural Decomposition on n : Node

(define (delete-tree-from-node lon)
  (foldr 
   ;; Node ListOf<Node> -> ListOf<Node>
   ;; GIVEN: a Node and the result of the subtree for a given root 
   ;; RETURNS: a ListOf<Node> but after the d event has taken place
   (lambda(x y)
     (if (node-selected? x)
         y
         (cons (new-single-node x) y)))
   empty
   lon))

;; TESTS: Covered under the main function nodes-after-key-event

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; new-single-node: Node -> Node
;; GIVEN: A node
;; RETURNS: a Node, similar to the given, while also calling 
;;          the function delete-tree-from-node on its sub-tree
;; EXAMPLES: 
;; (new-single-node SELECTED-NODE)
;; -> SELECTED-NODE
;; STRATEGY: Structural Decomposition on n : Node

(define (new-single-node n)
  (make-node (node-x-pos n)
             (node-y-pos n)
             (node-selected? n)
             (delete-tree-from-node (node-sub-tree n))))

;; TESTS: Covered under the main function nodes-after-key-event

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; add-new-root-node : ListOf<Node> -> ListOf<Node>
;; GIVEN: a ListOf<Node>
;; RETURNS: a ListOf<Node> like the given one but a new Node added to the 
;; list.
;; EXAMPLES:
;; (add-new-root-node empty)
;; -> LIST-OF-UNSELECTED-ROOT
;; STRATEGY: Function Compostion

(define (add-new-root-node lon)
  (cons (make-node CANVAS-HALF-WIDTH
                   NODE-HALF-SIDE 
                   UNSELECTED
                   EMPTY-LIST) 
        lon)) 

;; TESTS: Covered under the main function nodes-after-key-event

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; add-new-node : ListOf<Node> -> ListOf<Node>
;; GIVEN: a ListOf<Node>
;; RETURNS: a ListOf<Node> after the event key 'n' is completed
;; EXAMPLES:
;; (add-tree-to-node LIST-OF-SELECTED-ROOT)
;; -> (list (make-node 200 10 true (list (make-node 200 70 false empty))))
;; STRATEGY: Higher Order Function Composition

(define (add-tree-to-node lon)
  (map 
   ;; Node -> ListOf<Node>
   ;; GIVEN: a Node
   ;; RETURNS: a ListOf<Node> after the n key event
   (lambda(x)
     (add-sub-tree-to-node x))
   lon))

;; TESTS: Covered under the main function nodes-after-key-event

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; add-sub-tree-to-node : Node -> Node
;; GIVEN: a Node
;; RETURNS: a Node similar to the given with a new sub-tree or 
;;          with no change
;; EXAMPLES:
;; (add-sub-tree-to-node UNSELECTED-NODE)
;; -> UNSELECTED-NODE
;; STRATEGY: Structural Decomposition on n : Node

(define (add-sub-tree-to-node n)
  (if (and (node-selected? n) (can-node-be-added? n))
      (add-new-child n)
      (return-same-node n)))

;; TESTS: Covered under the main function nodes-after-key-event

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; add-new-child : Node -> Node
;; GIVEN: a Node
;; RETURNS: a node with a new sub tree
;; EXAMPLES:
;; (add-new-child SELECTED-ROOT-NODE)
;; -> (make-node 200 10 true (list (make-node 200 70 false empty)))
;; STRATEGY: Structural Decomposition on n : Node

(define (add-new-child n)
  (make-node (node-x-pos n)
             (node-y-pos n)
             (node-selected? n)
             (cons (make-new-node-for-sub-tree (new-child-position n)
                                               (node-y-pos n)
                                               (node-sub-tree n))
                   (add-tree-to-node(node-sub-tree n)))))

;; TESTS: Covered under the main function nodes-after-key-event

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; return-same-node : Node -> Node
;; GIVEN: a Node
;; RETURNS: the same node if there is no place to add a new node
;; EXAMPLES:
;; (return-same-node (make-node 400 400 true empty))
;; -> (make-node 400 400 true empty)
;; STRATEGY: Structural Decomposition on n : Node

(define (return-same-node n)
  (make-node (node-x-pos n)
             (node-y-pos n)
             (node-selected? n)
             (add-tree-to-node(node-sub-tree n))))

;; TESTS: Covered under the main function nodes-after-key-event

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; make-new-node-for-sub-tree : Real Real ListOf<Node> -> Node
;; GIVEN: the x and y co-ordinates of the Node on the canvas 
;; and ListOf<Node>
;; RETURNS: a new Node which is 2 square length to the left of the center
;; of the current Node and 3 square length down from the center of the
;; parent.
;; EXAMPLES:
;; (make-new-node-for-sub-tree 210 15 empty)
;; -> (make-node 220 75 false empty)
;; STRATEGY: Function Composition

(define (make-new-node-for-sub-tree x y sub-tree)
  (make-node (+ x NODE-HALF-SIDE) 
             (+ y (* NODE-SIDE 3))
             UNSELECTED
             EMPTY-LIST))

;; TESTS: Covered under the main function nodes-after-key-event

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; can-node-be-added? : Node -> Boolean
;; GIVEN: a Node
;; RETURNS: true, iff there is room in the canvas for 
;;          the new node to be added
;; EXAMPLES:
;; (can-node-be-added? SELECTED-NODE)
;; -> true
;; STRATEGY: Function Composition

(define (can-node-be-added? n)
  (> (new-child-position n) CANVAS-BASE))

;; TESTS: Covered under the main function nodes-after-key-event

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; new-child-position : Node -> Real
;; GIVEN: a Node
;; RETURNS: the position of the next child for that node
;; EXAMPLES:
;; (new-child-position UNSELECTED-NODE)
;; -> 240
;; STRATEGY: Structural Decompostion on n : Node

(define (new-child-position n)
  (- (find-left-most-child-position (node-to-sons n) 
                                    (node-x-pos n))
     (if (equal? (length (node-sub-tree n)) 0)
         0
         (* 2 NODE-SIDE))
     NODE-HALF-SIDE))

;; TESTS: Covered under the main function nodes-after-key-event

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; find-left-most-child-position : ListOf<Node> Real-> Real
;; GIVEN: a list of nodes and x position
;; RETURNS: the position of the left most child
;; EXAMPLES: 
;; (find-left-most-child-position LIST-OF-UNSELECTED-NODE 200)
;; -> 250
;; STRATEGY: Higher-order Function Compostion

(define (find-left-most-child-position lon px)
  (foldr 
   ; Node Real -> Real
   ; GIVEN: a Node and value representing the result
   ; RETURNS: the min of the x co-ordinate of the node
   (lambda(x y)
     (min (node-x-pos x) y))
   (maximum-x-pos-of-sons lon px)
   lon))

;; TESTS: Covered under the main function nodes-after-key-event

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; maximum-x-pos-of-sons : ListOf<Node> Real -> Real
;; GIVEN: a ListOf<Node> and the x position
;; RETURNS: the maximum of the x position of the sons
;; EXAMPLES:
;;
;; STRATEGY: Higher Order Function Composition
(define (maximum-x-pos-of-sons lon px)
  (foldr
   ;; Node Real -> Real
   ;; GIVEN: a Node and the result of x position of sons
   ;; RETURNS: the maximum of the x position of the sons
   (lambda(x y)
     (max (node-x-pos x) y))
   px
   lon))

;; TESTS: Covered under the main function nodes-after-key-event

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TESTS FOR world-after-key-event

(begin-for-test
  (check-equal? (world-after-key-event (make-world empty) T-KEY-EVENT)
                WORLD-OF-UNSELECTED-ROOT
                "returns a new unselected root node")
  (check-equal? (world-after-key-event WORLD-OF-SELECTED-ROOT U-KEY-EVENT)
                (make-world empty)
                "removes the node as it lies in the upper half canvas")
  (check-equal? (world-after-key-event WORLD-OF-UNSELECTED-NODE 
                                       U-KEY-EVENT)
                WORLD-OF-UNSELECTED-NODE
                "returns the same world as the node is not in the upper 
left half of the canvas")
  (check-equal? (world-after-key-event WORLD-OF-UNSELECTED-NODE 
                                       OTHER-KEY-EVENT)
                WORLD-OF-UNSELECTED-NODE
                "returns the same world for any other key event")
  (check-equal? (world-after-key-event (make-world empty) N-KEY-EVENT)
                (make-world empty)
                "returns a new unselected root node")
  (check-equal? (world-after-key-event WORLD-OF-SELECTED-ROOT N-KEY-EVENT)
                (make-world 
                 (list 
                  (make-node 
                   CANVAS-HALF-WIDTH NODE-HALF-SIDE SELECTED 
                   (list (make-node CANVAS-HALF-WIDTH 
                                    (+ NODE-HALF-SIDE 
                                       (* 3 NODE-SIDE)) 
                                    UNSELECTED empty)))))
                "returns a new unselected node added to the root")
  (check-equal? (world-after-key-event WORLD-OF-NODES D-KEY-EVENT)
                WORLD-OF-UNSELECTED-NODE
                "removes the child nodes for the selected parent node")
  (check-equal? (world-after-key-event WORLD-OF-NODES-AT-BOUNDARY 
                                       N-KEY-EVENT)
                WORLD-OF-NODES-AT-BOUNDARY
                "returns the same world if a new node cannot be created"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-to-roots : World -> ListOf<Node>
;; GIVEN: a World
;; RETURNS: a list of all the root nodes in the given world.
;; STRATEGY: Strucutral Decomposition on w : World

(define (world-to-roots w)
  (world-root-node w))

;; TESTS:
(begin-for-test
  (check-equal? (world-to-roots WORLD-OF-NODES)
                LIST-OF-NODES))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; node-to-center : Node -> Posn
;; RETURNS: the center of the given Node as it is to be displayed on the 
;; scene.
;; STRATEGY: Structural Decomposition on n : Node

(define (node-to-center n)
  (make-posn (node-x-pos n)
             (node-y-pos n)))

;; TESTS:
(begin-for-test
  (check-equal? (node-to-center SELECTED-ROOT-NODE)
                (make-posn CANVAS-HALF-WIDTH NODE-HALF-SIDE)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; node-to-sons : Node -> ListOf<Node>
;; GIVEN: a Node
;; RETURNS: a list of nodes (sons) from the given node
;; STRATEGY: Structural Decomposition on n : Node

(define (node-to-sons n)
  (if (empty? (node-sub-tree n))
      empty
      (node-sub-tree n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; node-to-selected? : Node -> Boolean
;; GIVEN: a Node
;; RETURNS: true iff the given node has beeen selected
;; Examples:
;;   (node-to-selected? (make-node 150 150 true EMPTY-LIST)) -> true
;; STRATEGY: Structural Decomposition on n : Node

(define (node-to-selected? n)
  (node-selected? n))

(begin-for-test
  (check-equal?
         (node-to-selected? (make-node 150 150 true EMPTY-LIST))
         true
         "Given node is selected,should return true"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-to-scene : World -> Scene
;; RETURNS: a Scene that displays the given world.
;; EXAMPLES:
;;           (world-to-scene (initial-world 19) -> EMPTY-CANVAS
;; STRATEGY: Structural Decomposition on w : World

(define (world-to-scene w)
  (display-connecting-lines (world-root-node w) 
                            (nodes-to-scene (world-root-node w) 
                                            EMPTY-SCENE)))

;; TESTS: After all helper functions

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; display-connecting-lines : ListOf<Node> Scene -> Scene
;; GIVEN: A ListOf<Node> and a scene 
;; RETURNS: a Scene with the connecting lines between the parent and
;; the children nodes.
;; EXAMPLES:
;; STRATEGY: Higher Order Function Composition
(define (display-connecting-lines lon scene)
  (foldr
   ;; Node Scene -> Scene
   ;; GIVEN: a Node and a Scene which is the result of the computation 
   ;; so far
   ;; RETURNS: a Scene with the lines connecting the parent and child nodes
   (lambda (n image-so-far)
     (connect-parent-to-child n image-so-far))
   scene
   lon))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; connect-parent-to-child : Node Scene -> Scene
;; GIVEN: a Node and a Scene
;; RETURNS: a Scene by displaying the connecting lines between the parent
;; and the child nodes.
;; EXAMPLES:
;; STRATEGY: Structural Decomposition on n : Node
(define (connect-parent-to-child n image)
  (foldr
   ;; Node Scene -> Scene
   ;; GIVEN: a Node and a Scene which is the result of the computation
   ;; so far
   ;; RETURNS: a Scene with the lines connecting the parent and the child
   ;; nodes
   (lambda (cn image-so-far)
     (scene+line image-so-far
                 (node-x-pos cn) (node-y-pos cn) 
                 (node-x-pos n) (node-y-pos n)
                 LINE-COLOR))
   (display-connecting-lines (node-sub-tree n) image)
   (node-sub-tree n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; nodes-to-scene : ListOf<Node> Scene -> Scene
;; GIVEN: a ListOf<Node> lon and a Scene
;; RETURNS: a Scene with all the nodes created 
;; EXAMPLES:
;; STRATEGY: Higher Order Function Composition
(define (nodes-to-scene lon scene)
  (foldr
   ;; Node Scene -> Scene
   ;; GIVEN: a Node and a scene 
   ;; RETURNS: a Scene containing given node.
   (lambda (n s)
     (single-node-to-scene n s))
   scene
   lon))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; single-node-to-scene : Node Scene -> Scene
;; GIVEN: a Node and a Scene
;; RETURNS: a Scene with the nodes and the vertical line indicating the
;; position of the next child node.
;; EXAMPLES:
;; STRATEGY: Structural Decomposition on n : Node
(define (single-node-to-scene n scene)
  (if (node-selected? n)
      (scene+line (display-selected-square n scene)
                  (new-child-position n) CANVAS-BASE
                  (new-child-position n) CANVAS-HEIGHT
                  VERTICLE-LINE-COLOR)
      (place-image UNSELECTED-SQUARE
                   (node-x-pos n) (node-y-pos n)
                   (nodes-to-scene (node-sub-tree n) scene))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; display-selected-square : Node Scene -> Scene
;; GIVEN: a Node and a Scene 
;; RETURNS: a Scene with the selected node displayed 
;; EXAMPLES:
;; STRATEGY: Structural Decomposition on n : Node
(define (display-selected-square n scene)
  (place-image 
   (if (can-node-be-added? n)
       SELECTED-SQUARE
       SELECTED-SQUARE-NO-ROOM)
   (node-x-pos n) (node-y-pos n)
   (nodes-to-scene (node-sub-tree n) scene)))

;; TESTS ;;;

(begin-for-test
  (check-equal? (world-to-scene (initial-world 19)) 
                EMPTY-SCENE
                "Should return empty canvas"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;