;;;; trurl.lisp

(in-package #:trurl)

(defparameter *update-frequency* 1)
(defparameter *grid* (lem:make-grid 80 40))
(defparameter *legend* (make-hash-table))
(defparameter *chars* (loop for i from 33 to 126 collect (code-char i)))

(lem:seed! *grid* 20 20 (lem:line))
(lem:seed! *grid* 30 30 (lem:ray))
(lem:seed! *grid* 60 20 (lem:box))

(bt:make-thread
 (lambda ()
   (loop do (lem:step-grid! *grid*)
      do (sleep *update-frequency*))))

(defun show! (unit)
  (let ((k (class-name (class-of unit))))
    (or (gethash k *legend*)
	(setf (gethash k *legend*)
	      (format nil "~a" (pop *chars*))))))

(define-handler (/) ()
  (with-html-output-to-string (s nil :prologue t :indent t)
    (:html (:head
	    (:title "Trurl")
	    (:link :rel "stylesheet" :href "/css/main.css")
	    (:script :type "text/javascript" :src "/js/base.js")
	    (:script :type "text/javascript" :src "/js/main.js"))
	   (:body
	    (:ul :class "grid"
		 (loop for y from 0 to (- (lem:grid-height *grid*) 1)
		    do (htm (:li :class "grid-row"
				 (loop for x from 0 to (- (lem:grid-width *grid*) 1)
				    for c = (lem:get-cell *grid* x y)
				    do (htm (:div
					     :class "cell"
					     (unless (lem:empty? c)
					       (htm (:div
						     :class (format
							     nil "unit ~(~a~)"
							     (class-name (class-of (lem:occupant c))))))))))))))))))

(define-handler (js/main.js :content-type "application/javascript") ()
  (ps (console.log "HELLO FROM JAVASCRIPTLAND!")))

(define-handler (js/base.js :content-type "application/javascript") ()
  (ps
    ;;;; base.js contains general utilities that might be useful in other JS
    ;;;; applications too. Nothing notebook-specific here.

    ;; basic functional stuff
    (defun identity (thing) thing)

    (defun constantly (thing) (lambda () thing))

    (defun rest (array) (chain array (slice 1)))

    (defun map (fn thing)
      (if (object? thing)
	  (let ((res (-array)))
	    (for-in (k thing) (chain res (push (fn (aref thing k) k))))
	    res)
	  (loop for elem in thing collect (fn elem))))

    (defun fold (fn memo thing)
      (let ((m memo))
	(if (object? thing)
	    (for-in (k thing) (setf m (fn (aref thing k) m)))
	    (loop for elem in thing do (setf m (fn elem m))))
	m))

    (defun filter (fn thing)
      (loop for elem in thing when (fn elem) collect elem))

    (defun extend (obj &rest other-objs)
      (flet ((ext! (dest src) (map (lambda (v k) (setf (aref dest k) v)) src)))
	(let ((res (create)))
	  (ext! res obj)
	  (map (lambda (obj) (ext! res obj)) other-objs)
	  res)))

    (defun append-new (list-a list-b)
      (let ((s (new (-set list-a)))
	    (lst (map #'identity list-a)))
	(loop for elem in list-b
	   unless (chain s (has elem))
	   do (chain lst (push elem)))
	lst))

    (defun lines (string) (chain string (split #\newline)))

    (defun join (strings &optional (separator "")) (chain strings (join separator)))

    ;; basic hash/array stuff
    (defun vals (obj) (map identity obj))
    (defun keys (obj) (map (lambda (v k) k) obj))
    (defun last (array) (aref array (- (length array) 1)))

    (defun member? (elem thing)
      (if (object? thing)
	  (in elem thing)
	  (chain thing (index-of elem))))

    (defun equal? (a b)
      (let ((type-a (typeof a))
	    (type-b (typeof b)))
	(and (equal type-a type-b)
	     (cond
	       ((member? type-a (list "number" "string" "function"))
		(equal a b))
	       ((array? a)
		(and
		 (= (length a) (length b))
		 (loop for elem-a in a for elem-b in b
		    unless (equal? elem-a elem-b) return f
		    finally (return t))))
	       ((object? a)
		;; object comparison here
		;; and
		;;   all keys of a are in b
		;;   all keys of b are in a
		;;   all keys of a and b have the same values
		nil
		)))))

    ;; basic regex stuff
    (defun regex-match (regex string)
      (chain (-reg-exp regex) (test string)))
    (defun regex-match-any (string &rest regexes)
      (loop for reg in regexes
	 when (regex-match reg string) return t
	 finally (return nil)))

    (defun matching? (regex)
      (lambda (string) (regex-match regex string)))

    ;; basic DOM/event stuff
    (defun sheet-text (sheet &optional (predicate identity))
      (if (number? sheet)
	  (sheet-text (aref (@ document style-sheets) sheet) predicate)
	  (join
	   (loop for rule in (@ sheet css-rules)
	      for text = (@ rule css-text)
	      when (predicate text) collect text)
	   #\newline)))

    (defun dom-ready (callback)
      (chain document (add-event-listener "DOMContentLoaded" callback)))

    (defun remove-all-event-handlers (elem)
      (let ((clone (chain elem (clone-node t))))
	(chain elem parent-node (replace-child clone elem))
	clone))

    (defun prevent (ev) (when ev (chain ev (prevent-default))))

    (defun debounce (fn delay immediate?)
      (let ((timeout))
	(lambda ()
	  (let ((context this)
		(args arguments))
	    (clear-timeout timeout)
	    (setf timeout (set-timeout
			   (lambda ()
			     (setf timeout nil)
			     (unless immediate?
			       (chain fn (apply context args))))
			   delay))
	    (when (and immediate? (not timeout))
	      (chain fn (apply context args)))))))

    (defun scroll-to-elem (elem)
      (let ((x (@ elem offset-left))
	    (y (@ elem offset-top)))
	(chain window (scroll-to x y))))

    (defun show! (elem)
      (setf (@ elem hidden) nil))
    (defun hide! (elem)
      (setf (@ elem hidden) t))

    (defun by-selector (selector)
      (chain document (query-selector selector)))
    (defun by-selector-all (selector)
      (chain document (query-selector-all selector)))

    (defun dom-escape (string)
      (when (string? string)
	(chain string
	       (replace "<" "&lt;")
	       (replace ">" "&gt;"))))

    (defun -make-elem (markup)
      (let ((new-content (chain document (create-element "span"))))
	(setf (@ new-content inner-h-t-m-l) markup)
	new-content))

    (defun dom-append (elem markup)
      (let ((new-content (-make-elem markup)))
	(loop while (@ new-content first-child)
	   do (chain elem (append-child (@ new-content first-child))))))

    (defun dom-prepend (elem markup)
      (let ((new-content (-make-elem markup)))
	(loop while (@ new-content last-child)
	   do (chain elem (prepend (@ new-content last-child))))))

    (defun dom-replace (elem markup)
      (let ((new-content (chain document (create-element "span")))
	    (parent (@ elem parent-node)))
	(setf (@ new-content inner-h-t-m-l) markup)
	(loop for child in (@ new-content child-nodes)
	   do (chain parent (insert-before new-content elem)))
	(chain elem (remove))))

    (defun dom-set (elem markup)
      (setf (@ elem inner-h-t-m-l) markup))

    ;; basic type stuff
    (defun number? (obj) (string= "number" (typeof obj)))
    (defun string? (obj) (string= "string" (typeof obj)))
    (defun function? (obj) (string= "function" (typeof obj)))

    (defun type? (obj type-string)
      (eql (chain -object prototype to-string (call obj)) type-string))
    (defun array? (arr) (type? arr "[object Array]"))
    (defun object? (obj) (type? obj "[object Object]"))

    ;; basic encoding/decoding stuff
    (defun encode (string)
      (encode-u-r-i-component string))

    (defun decode (string)
      (decode-u-r-i string))

    (defun string->obj (string)
      (chain -j-s-o-n (parse string)))

    (defun obj->string (object)
      (chain -j-s-o-n (stringify object)))

    (defun obj->params (object)
      (join
       (map (lambda (v k)
	      (+ (encode k) "="
		 (encode (if (object? v) (obj->string v) v))))
	    object)
       "&"))

    ;; basic AJAX stuff
    (defun save-file (filename contents &optional (type "application/json;charset=utf-8"))
      (let* ((content-string (if (string? contents) contents (obj->string contents)))
	     (blob (new (-blob (list content-string) (create :type type)))))
	(save-as blob filename)))

    (defun get-page-hash ()
      (let ((hash (@ window location hash))
	    (res (create)))
	(when hash
	  (loop for pair in (chain (rest hash) (split "&"))
	     for (k v) = (chain pair (split "="))
	     do (setf (aref res (decode k)) (decode v)))
	  res)))

    (defun set-page-hash (hash-object)
      (setf (@ window location hash) (obj->params hash-object)))

    (defun get (uri params callback)
      (let ((req (new (-x-m-l-http-request))))
	(setf (@ req onreadystatechange)
	      (lambda ()
		(when (and (equal (@ req ready-state) 4)
			   (equal (@ req status) 200))
		  (let ((result (@ req response-text)))
		    (callback result)))))
	(chain req (open :GET (if params (+ uri "?" (obj->params params)) uri) t))
	(chain req (send))))

    (defun post (uri params on-success on-fail)
      (let ((req (new (-x-m-l-http-request)))
	    (encoded-params (obj->params params)))
	(setf (@ req onreadystatechange)
	      (lambda ()
		(when (equal (@ req ready-state) 4)
		  (if (equal (@ req status) 200)
		      (when (function? on-success)
			(let ((result (@ req response-text)))
			  (on-success result)))
		      (when (function? on-fail)
			(on-fail req))))))
	(chain req (open :POST uri t))
	(chain req (set-request-header "Content-type" "application/x-www-form-urlencoded"))
	(chain req (set-request-header "Content-length" (length encoded-params)))
	(chain req (set-request-header "Connection" "close"))
	(chain req (send encoded-params))))

    (defun post/json (uri params on-success on-fail)
      (post uri params
	    (lambda (raw)
	      (when (function? on-success)
		(let ((res (string->obj raw)))
		  (on-success res))))
	    on-fail))

    (defun event-source (uri bindings)
      (let ((stream (new (-event-source uri))))
	(setf (@ stream onopen) (lambda (e) (console.log "Stream OPENED!"))
	      (@ stream onerror) (lambda (e) (console.log "Stream ERRORED!"))
	      (@ stream onmessage)
	      (lambda (e)
		(let* ((res (string->obj (@ e data)))
		       (callback (aref bindings (@ res action))))
		  (if callback
		      (funcall callback res)
		      (console.log "Unhandled message" res)))))
	stream))))

(define-handler (css/main.css :content-type "text/css") ()
  (css `((body :font-family sans-serif)

	 (".grid" :overflow auto :margin 0 :padding 0 :list-style-type none)
	 (".grid .grid-row" :display block :clear both)
	 (".cell" :float left :width 15px :height 15px
		  :background-color "#eee" :margin-right 1px :margin-bottom 1px)
	 (".cell .unit" :background-color "#f00" :width 100% :height 100%)
	 (".unit.box" :background-color "#0f0")
	 (".unit.line" :background-color "#00f" ))))
