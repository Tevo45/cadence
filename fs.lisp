;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: CADENCE -*-

(defvar +no-fid+ 0)
(defvar +max-fid+ 2147483647)

(defflavor 9p-vfs-client
	(client
	 root-fid
	 root-qid
	 (current-user "")
	 (auth-fid +no-fid+)
	 (active-fids (make-hash-table)))
	(fs:user-file-access-path)
  :readable-instance-variables)

(defmethod (new-fid 9p-vfs-client) ()
  (loop for n = (random +max-fid+)
	when (null (nth-value 1 (gethash n active-fids)))
	  return n))

(defmethod (9p-send 9p-vfs-client) (&rest args)
  (unless (member :tag args)
    (setf args
	  (append args (list :tag (9p:new-tag client)))))
  (let ((rmsg (9p:send client (apply #'clos:make-instance args))))
    (typecase rmsg
      (:Rerror (error "blah")) ; FIXME
      (t rmsg))))

(defmethod (attach 9p-vfs-client) (&key (rootspec ""))
  (let* ((fid (new-fid self))
	 (Rattach (9p-send self :Tattach
			   :fid fid
			   :afid auth-fid
			   :uname current-user
			   :aname rootspec)))
    (setf root-fid fid)
    (setf root-qid (9p::qid Rattach))))

(defmethod (clunk 9p-vfs-client) (fid)
  (9p-send self :Tclunk fid)
  (remhash fid active-fids))

(defmethod (:directory-list 9p-vfs-client) (path options)
  (print path)
  (print options)
  nil)

(defmethod (make-instance 9p-vfs-client) (&key host service-access-path (rootspec ""))
  (declare (ignore host))
  (setf client 
	(clos:make-instance '9p:9p-client 
			    :stream (net:get-connection-for-service service-access-path
								    :characters nil)))
  (setf root-fid (new-fid self))
  (attach self :rootspec rootspec))

(fs:define-file-protocol :9p (:tcp)
  (:desirability .75)
  (:access-path-flavor 9p-vfs-client))

(compile-flavor-methods 9p-vfs-client)

(tcp:add-tcp-port-for-protocol :9p 2121)









