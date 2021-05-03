;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: 9P -*-

(defvar +no-tag+ 0)
(defvar +maximum-message-size+ 65535)

(defvar *known-protocols* (make-hash-table :test 'string=))

(defun read-9p-uint (stream &key (size 1))
  (loop with n = 0
	for x from 1 to size
	do (setf (ldb (byte 8 (* 8 (1- x))) n) (read-byte stream))
	finally
	  (return n)))

(defun write-9p-uint (stream n &key (size 1))
  (loop for x from 1 to size
	do (write-byte (ldb (byte 8 (* 8 (1- x))) n) stream)))

;; FIXME don't use (linked) lists for variable length parameters

(defun read-9p-vlen-parameter (stream)
  (loop with n = (read-9p-uint stream :size 2)
	repeat n
	collect (read-byte stream)))

(defun write-9p-vlen-parameter (stream p)
  (write-9p-uint stream (length p) :size 2)
  (loop for b in p
	do (write-byte b stream)))

(defun read-9p-string (stream)
  (let* ((bytes (read-9p-vlen-parameter stream))
	 ; FIXME we should map between UTF-8 <-> Symbolics charset instead
	 (chars (mapcar #'code-char bytes)))
    (coerce chars 'string)))

(defun write-9p-string (stream str)
  (let* ((chars (coerce str 'list))
	 (bytes (mapcar #'char-code chars)))
    (write-9p-vlen-parameter stream bytes)))

(defgeneric message-type (message))
(defgeneric message-size (message))
(defgeneric serialize (message stream &key max-size))

(defmacro defproto (version &body messages)
  `(let ((message-types nil))
     ,@(loop for (message id . msgfields) in messages
	     for fields = (append '((tag 2)) msgfields) ; tag is implicit on every message
	     for slots = (mapcar #'car fields)
	     collect `(push (cons ,message ,id) message-types)
	     collect `(defclass ,message ()
			  ((tag :initarg :tag
				:initform +no-tag+
				:reader tag)
			   ,@(loop for (field) in msgfields
				   collect `(,field :initarg ,(intern (string field) :keyword)
					            :accessor ,field))))
	     collect `(defmethod initialize-instance :after ((message ,message) 
							     &key (from-stream nil))
			(when from-stream
			  (with-slots ,slots message
			    ,@(loop for (field type) in fields
				    collect `(setf ,field
						   ,(cond
						      ((eq type :string)
						       `(read-9p-string from-stream))
						      ((eq type :bytes)
						       `(read-9p-vlen-parameter from-stream))
						      ((or (numberp type) (symbolp type))
						       `(read-9p-uint from-stream 
								      :size ,type))
						      (t
						       (error "Don't know how to parse ~a" 
							      type))))))))
	     collect `(defmethod message-type ((message ,message)) ,id)
	     collect `(defmethod message-size ((message ,message))
			(with-slots ,slots message
			  (+ 4 1 ; size and message type
			     ,@(loop for (field type) in fields
				     collect (cond
					       ; FIXME this assumes characters are 1 byte
					       ((or (eq type :string) (eq type :bytes))
						`(+ 2 (length ,field)))
					       ((or (numberp type) (symbolp type))
						type)
					       (t
						(error "Don't know size for ~a" type)))))))
	     collect `(defmethod serialize ((message ,message) stream &key (max-size nil))
			(with-slots ,slots message
			  (let ((size (message-size message)))
			    (when (and max-size (< max-size size))
			      (error "Message is ~d bytes long, maximum size is ~d bytes."
				     size max-size))
			    (write-9p-uint stream size :size 4))
			  (write-9p-uint stream (message-type message))
			  ,@(loop for (field type) in fields
				  collect (cond
					    ((eq type :string)
					     `(write-9p-string stream ,field))
					    ((eq type :bytes)
					     `(write-9p-vlen-parameter stream ,field))
					    ((or (numberp type) (symbolp type))
					     `(write-9p-uint stream ,field
							     :size ,type))
					    (t
					     (error "Don't know how to serialize ~a"
						    type)))))))
     (setf (gethash ,version *known-protocols*) message-types)))

(defproto "9P2000"
  (:Tversion 100 (msize 4) (version :string))
  (:Rversion 101 (msize 4) (version :string))

  (:Tauth 102 (afid 4) (uname :string) (aname :string))
  (:Rauth 103 (aqid 13))

  (:Rerror 107 (ename :string))

  (:Tflush 108 (oldtag 2))
  (:Rflush 109)

  (:Tattach 104 (fid 4) (afid 4) (uname :string) (aname :string))
  (:Rattach 105 (qid 13))

  ;; what does nwname*(wname[s]) and nwqid*(wqid[13]) mean?
  (:Twalk 110 (fid 4) (newfid 4) (nwname 2))
  (:Rwalk 111 (nwqid 2))

  (:Topen 112 (fid 4) (mode 1))
  (:Ropen 113 (qid 13) (iounit 4))

  (:Tcreate 114 (fid 4) (name :string) (perm 4) (mode 1))
  (:Rcreate 115 (qid 13) (iounit 4))

  (:Tread 116 (fid 4) (name :string) (perm 4) (mode 1))
  (:Rread 117 (count 4) (data count))

  (:Twrite 118 (fid 4) (offset 8) (count 4) (data count))
  (:Rwrite 119 (count 4))

  (:Tclunk 120 (fid 4))
  (:Rclunk 121)

  (:Tremove 122 (fid 4))
  (:Rremove 123)

  (:Tstat 124 (fid 4))
  (:Rstat 125 (stat :bytes))

  (:Twstat 126 (fid 4) (stat :bytes))
  (:Rwstat 127))

(defclass 9p-client ()
    ((active-tags :initform (make-hash-table)
		  :reader active-tags)
     (stream :initarg :stream
	     :initform (error "You need to provide a stream.")
	     :reader stream)
     (protocol-version :initarg :version
		       :initform "9P2000"
		       :reader version)
     (maximum-message-size :initarg :max-size
			   :initform +maximum-message-size+
			   :reader maximum-message-size)
     (message-types :reader message-types)))

(defmethod new-tag ((client 9p-client))
  (with-slots (active-tags) client
    (loop for n = (1+ (random 65534))
	  when (null (nth-value 1 (gethash n active-tags)))
	    return n)))

(defmethod send ((client 9p-client) message)
  (with-slots (active-tags stream maximum-message-size throw-rerror) client
    (let ((rmsg nil))
      (setf (gethash (tag message) active-tags) 
	    (list message 
		  #'(lambda (msg)
		      (setf rmsg msg))
		  *current-process*))
      (serialize message stream :max-size maximum-message-size)
      (scl:send stream :force-output)
      ;; block here; we expect the listener function to call the
      ;; closure in the list and wake us up after receiving the
      ;; corresponding Rmessage
      (process:block-process *current-process*
			     #'(lambda ()
				 (not (null rmsg))))
      (if (and throw-rerror (typep rmsg :Rerror))
	  (error)
	  rmsg))))

(defmethod receive ((client 9p-client))
  (with-slots (stream message-types) client
    (read-9p-uint stream :size 4)
    (let* ((id (read-byte stream))
	   (type (car (rassoc id message-types))))
      (make-instance type :from-stream stream))))

(defmethod receive-and-dispatch ((client 9p-client))
  (ignore-errors
    (with-slots (active-tags stream message-types) client
      (loop for message = (receive client)
	    for listener = (gethash (tag message) active-tags)
	    when listener
	      do (funcall (second listener) message)
		 (process:wakeup (third listener))
		 (remhash (tag message) active-tags)))))

(defmethod negotiate-version ((client 9p-client) &key (compromise t))
  (restart-case
    (with-slots (protocol-version message-types maximum-message-size) client
      (flet ((refresh-messages ()
	       (multiple-value-bind (proto-messages exists)
		   (gethash protocol-version *known-protocols*)
		 (if exists
		     (setf message-types proto-messages)
		     (error "Unknown protocol version ~a" protocol-version)))))
	(refresh-messages)
	(let* ((tversion (make-instance :Tversion
					:msize +maximum-message-size+
					:version protocol-version))
	       (rversion (send client tversion)))
	  (when (and (not (string= protocol-version (version rversion)))
		     (not compromise))
	    (restart-case
	        (error "Tried asking for ~a, but server wants ~a"
		       protocol-version (version rversion))
	      (use-server-version ()
	        :report "Use server's 9P version, if supported."
	        (setf compromise t))))
	  (setf protocol-version (version rversion))
	  (setf maximum-message-size (msize rversion))
	  (refresh-messages)
	  protocol-version)))
    (try-version (version)
      :report "Try negotiating a different version."
      :interactive (lambda () (eval (read)))
      (setf (version client) version))))

(defmethod (setf version) ((client 9p-client) new-version)
  (with-slots (protocol-version) client
    (setf protocol-version new-version)
    (negotiate-version client)))

(defmethod initialize-instance :after ((client 9p-client) &key (skip-negotiation nil))
  (process:make-process "9P Client Message Listener"
			:initial-function #'(lambda ()
					      (receive-and-dispatch client)))
  (unless skip-negotiation
    (negotiate-version client)))

;(net:define-protocol :9p (:file :byte-stream)
;  (:invoke-with-stream ((conn :characters nil) &key (version "9P2000"))
;    (make-instance '9p-client :stream conn :version version)))

;(tcp:add-tcp-port-for-protocol :9p 2121)







