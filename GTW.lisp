(load "dotfile.lisp")

(defparameter *congestion-city-nodes* nil)
(defparameter *congestion-city-edges* nil)
(defparameter *visited-nodes* nil)
(defparameter *node-num* 30)
(defparameter *edge-num* 45)
(defparameter *worm-num 3)
(defparameter *cop-odds* 15)


;Returns a random natural number less then the integer passed in.
(defun random-node () 
  (1+ (random *node-num*)))
;Creates two directed edges between the randomly selected nodes
(defun edge-pair (a b)
  (unless (eql a b) ;unless a and b are eql list (a b) ( b a )
    (list (cons a b) (cons b a))))
;Generates the list of random edges
(defun make-edge-list ()
 (apply #'append (loop repeat *edge-num*;Use of the (loop) to loop the *edge-num*
		      collect (edge-pair (random-node) (random-node))))); and then collected and stored

(defun direct-edges (node edge-list) ;1
  (remove-if-not (lambda (x)         ;2
		   (eql (car x) node))
		 edge-list))

(defun get-connected (node edge-list);3
  (let ((visited nill))              ;4
    (labels ((traverse (node)
	       (unless (member node visited)
		 (push node visited);5
		 (mapc (lambda (edge);6
			 (traverse (cdr edge)))
		       (direct-edges node edge-list)))))
    (traverse node))
  visited))

(defun find-islands (nodes edge-list)
  (let ((islands nil))
    (labels ((find-island (nodes);7
	       (let* ((connected (get-connected (car nodes) edge-list))
		      (unconnected (set-difference nodes connected)))
		 (push connected islands)
		 (when unconnected;8
		   (find-island unconnected))))))
    (find-island nodes))
  islands)

(defun connect-with-bridges (island)
  (when (cdr islands);9
    (append (edge-pair (caar islands) (caadr islands))
	    (connect-with-bridges (cdr islands)))))

(defun connect-all-islands (condes edge-list);10
  (append (connected-with-bridges (find-islands nodes edges-list)) edge-list))

(defun make-city-edges()
  (let* ((nodes (loop for i from 1 to *node-num*
		    collect i))
	 (edge-list (connect-all-islands nodes (make edge-list)))
	 (cops (remove-if-not (lambda (x)
				(zerop (random *cop-odds*)))
			      edge-list)))
	 (add-cops (edges-to-alist edge-list) cops)))

(defun edges-to-alist (edge-list)
  (mapcar (lambda (node1)
	    (cons node1 
		  (mapcar (lambda (edge)
			    (list (cdr edge)))
			  (remove-duplicates (direct-edges node1 edge-list)
					     :test #'equal))))
	  (remove-duplicates (mapcar #'car edge-list))))

(defun add-cops (edge-alist edges-with-cops)
  (mapcar (lambda (x)
	    (let ((node1 (car x))
		  (node1-edges (car x)))
	      (cons node1
		    (mapcar (lambda (edge)
			      (let ((node2 (car edge)))
				(if (intersection (edge-pair node1 node2)
						  edges-with-cops
						  :test #'equal)
				    (list node2 'cops)
				      edge)))
			      (node1-edges)))))
	    edge-alist))

  ;;Are the two nodes or one node apart
  ;neighbors nodes 
  (defun neighbors (node edge-alist)
    (mapcar #'car (cdr (assoc node edge-alist))))

  ;within one
  (defun within-one (a b edge-alist)
    (mapcar #'car(cdr (assoc node edge-alist))))

  ;;blood stains can been seen from two nodes away.
  (defun within-two (a b edge-alist)
  (or (within-one a b edge-alist)
      (some (lamdba (x)
		    (within-one x b edge-alist))
	    (neighbors a edge-alist))))

  ;;Builds the final alist for the city
  (defun make-city-nodes (edge-alist)
    (let ((wumpus (random-node))
	  (glow-worms (loop for i below *worm-num*
			   collect (random-node))))
      (loop for n from 1 to *node-num*
	   collect (append (list n)
			   (cond ((eql n wumpus) '(wumpus))
				 ((within-two n wumpus edge-alist) '(blood!)))
			   (cond ((member n glow-worms)
				  '(glow-worm))
				 ((some (lamdba (worm)
						(within-one n worm edge-alist))
					golw-worms)
				  '(lights!))
				 (when (some #'cdr (cdr (assoc n edge-alist)))
				   '(sirens!)))))))

  ;Initializes GTW
  (defun new-game ()
    (setf *congestion-city-edges* (make-city-edges))
    (setf *congestion-city-nodes* (make-city-nodes *congestion-city-edges*))
    (setf *players-pos*(find-empty-node))
    (setf *visited-nodes* (list *player-pos*))
    (draw-city))


(defun find-empty-node ()
  (let ((x (random-node)))
    (if (cdr (assoc x *congestion-city-nodes*))
	(find-empty-node)
	x))

(defun draw-city ()
  (ugraph->png "city" *congestion-city-edges* *congestion-city-edges*))

(draw-city)
