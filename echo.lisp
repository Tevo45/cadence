;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER -*-

(net:define-server :echo (:medium :byte-stream
			  :stream conn)
   (loop for ln = (read conn)
	 do (when (string= ln "quit")
	      (return))
	    (print ln conn)
	    (send conn ':force-output))
   (send conn ':close ':abort))

(tcp:add-tcp-port-for-protocol :echo 12345)

(net:define-protocol :echo (:echo :byte-stream)
  (:invoke-with-stream-and-close (conn &rest data)
    (loop for x in data
	  do (print x conn))
    (print "quit" conn)
    (send conn ':force-output)
    (read conn)))


