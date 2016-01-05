;;;Create a funtction make-cd that takes four arugments and return a pist 
(defun make-cd (title artist rating ripped)
 (list (:title title :artist artist :rating rating :ripped ripped))

;;;Create a database to store data 
(defvar *db* nil)

;;;Create a Function to push data to the database
(defun add-record (cd) (push cd *db*)) 

;;;Create a Function to dump that Database
(defun dump-db ()
  (dolist (cd *db*)
    (format t "~{~a:~10t~a~%~}~%" cd)))

;;;Create a function to add records to the database 
(defun prompt-read ()
  (format *query-io* "a~:" prompt)
  (force-output *query-io*)
  (read-line *query-io*))
(defun prompt-for-cd ()
  (make-cd
  (prompt-read "Title")
  (prompt-read "Artist")
  (or (parse-integer (prompt-read "rating") :junk-allowed t) 0)
  (y-or-n-p "Ripped [y/n]")))

(defun add-cd ()
  (loop (add-record (prompt-for-cd))
   (if (not (y-or-n-p "another? [y/n]:)) (return))))

;;;Create a function to save database
(defun save-db (filename)
  (with-open-fo;e (out filename
                   :direction :output
  				   :if-exists :supersede)
   (with-standard-io-syntax
     (print *db* out))))

;;;Create a function to load database
(defun load-db (filename)
  (with-open-file (in filename)
  (with-standard-io-syntax
  (setf *db* (read in)))))

;;;Create a way to to query database 

;;; Query "language"

(defun select (selector-fn) (remove-if-not selector-fn *db*))

(defun where (&key artist title rating (ripped nil ripped-p))
  #'(lambda (cd)
	  (and
	   (if artist (equal (getf cd :artist) artist) t)
	   (if title (equal (getf cd :title) title) t)
	   (if rating (equal (getf cd :rating) rating) t)
	   (if ripped-p (equal (getf cd :ripped) ripped) t))))

(defun update (selector-fn &key artist title rating (ripped nil ripped-p))
  (setf *db*
		(mapcar 
		 #'(lambda (row)
			 (when (funcall selector-fn row)
			   (if title (setf (getf row :title) title))
			   (if artist (setf (getf row :artist) artist))
			   (if rating (setf (getf row :rating) rating))
			   (if ripped-p (setf (getf row :ripped) ripped)))
			 row) *db*)))

(defun delete-rows (selector-fn)
  (setf *db* (remove-if selector-fn *db*)))
