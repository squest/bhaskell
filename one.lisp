(defun prime? (n)
  (loop for i from 3 to (isqrt n) by 2
     when (zerop (rem n i))
     return nil
     finally (return t)))

(defun sum-primes (lim)
  (loop for i from 3 to lim by 2
     when (prime? i)
     summing i into res
     finally (return (+ 2 res))))

(defun sum-sieve (lim)
  (declare (optimize (speed 3))
	   (fixnum lim))
  (let ((llim (isqrt lim))
	(primes (make-array (+ lim 1) :initial-element t)))
    (loop for i from 3 to lim by 2
       when (aref primes i)
       do (when (<= i llim)
	    (loop for j from (* i i) to lim by (* 2 i)
	       do (setf (aref primes j) nil)))
       and summing i into res
       finally (return (+ 2 res)))))

(defun nth-sieve (nthi)
  (declare (optimize (speed 3))
	   (fixnum nthi))
  (let* ((lim (* 12 nthi))
	 (llim (isqrt lim))
	 (primes (make-array (+ lim 1) :initial-element t))
	 (cur 1))
    (loop for i from 3 to lim by 2
       when (aref primes i)
       do (when (<= i llim)
	    (loop for j from (* i i) to lim by (* 2 i)
	       do (setf (aref primes j) nil)))
       and do (setf cur (+ cur 1))
       when (= cur nthi)
       return i)))

(defun div (a b)
  (declare (optimize (speed 3))
	   (fixnum a b))
  (labels ((loopi (n res)
	      (if (< n b)
		  res
		  (loopi (- n b) (1+ res)))))
    (loopi a 0)))

(defun abuns (lim)
  (declare (optimize (speed 3))
	   (fixnum lim))
  (let ((llim (isqrt lim))
	(faks (make-array (+ lim 1) :initial-element 1)))
    (loop for i from 2 to llim
       do (loop for j from (* i i) to lim by i
	     do (setf (aref faks j)
		      (+ i (aref faks j) (if (= (* i i) j) 0 (div j i)))))
       finally (return (loop for k from 1 to lim
			  for m = (aref faks k)
			  when (> m k)
			  collect k)))))

(defun sum-abuns (lim)
  (declare (optimize (speed 3))
	   (fixnum lim))
  (let* ((faks (abuns lim))
	 (nfak (length faks))
	 (refs (make-array nfak
			   :initial-contents faks))
	 (jojo (make-array (1+ lim) :initial-element nil)))
    (progn (loop for i from 0 to (div (1- nfak) 2)
	      do (loop for j from i to (1- nfak)
		    for vi = (aref refs i)
		    for vj = (aref refs j)
		    for m = (+ vi vj)
		    always (<= m lim)
		    do (setf (aref jojo m) t)))
	   (loop for i from 0 to lim
	      when (aref jojo i)
	      summing i into res
	      finally (return (- (div (* lim (1+ lim)) 2) res))))))



