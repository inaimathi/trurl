(in-package #:trurl)

(define-handler (/) ()
  (with-html-output-to-string (s nil :prologue t :indent t)
    (:html (:head
	    (:title "Trurl")
	    (:link :rel "stylesheet" :href "/css/main.css")
	    (:script :type "text/javascript" :src "/js/base.js")
	    (:script :type "text/javascript" :src "/js/main.js")
	    (:script :type "text/javascript"
		     (str (ps* `(progn (defvar +width+ ,(lem:width *grid*))
				       (defvar +height+ ,(lem:height *grid*)))))))
	   (:body
	    (:div :id "palette")
	    (:div :id "grid-container")))))

(define-handler (js/main.js :content-type "application/javascript") ()
  (ps
    (defun spawn-here (x y)
      (when +selected-palette-item+
	(post/json "/api/place" (create :x x :y y :unit +selected-palette-item+)
		   (lambda (dat) (console.log dat)))))

    (defun cell-template (cell x y)
      (who-ps-html
       (:div :class "cell"
	     :onclick (+ "spawnHere(" x "," y ")")
	     (when cell
	       (who-ps-html
		(:div :class (+ "unit " (@ cell type))))))))

    (defun grid-row-template (row y)
      (who-ps-html
       (:li :class "grid-row"
	    (join (loop for x from 0 for cell in row
		     collect (cell-template cell x y))))))

    (defun grid-template (grid)
      (who-ps-html (:ul :class "grid"
			(join (loop for y from 0 for rw in grid
				 collect (grid-row-template rw y))))))

    (defun update-grid! ()
      (get/json "/api/look-at" (create :from-x 0 :from-y 0 :to-x +width+ :to-y +height+)
		(lambda (dat)
		  (dom-set (by-selector "#grid-container")
			   (grid-template dat)))))

    (defun update-loop! ()
      (update-grid!)
      (set-timeout update-loop! 500))


    (defvar +selected-palette-item+ nil)

    (defun select-palette-item (elem name)
      (cond ((chain elem class-list (contains "selected"))
	     (chain elem class-list (remove "selected"))
	     (setf +selected-palette-item+ nil))
	    (t
	     (map
	      (lambda (el) (chain el class-list (remove "selected")))
	      (by-selector-all ".palette-item.selected"))
	     (chain elem class-list (add "selected"))
	     (setf +selected-palette-item+ name))))

    (defun palette-item-template (name)
      (who-ps-html (:li :class "palette-item" :onclick (+ "selectPaletteItem(this, '" name "')") name)))

    (defun palette-template (items)
      (who-ps-html
       (:ul :class "palette"
	    (join (map palette-item-template items)))))

    (defun setup-palette! ()
      (get/json "/api/inventory" (create)
		(lambda (dat)
		  (dom-set (by-selector "#palette") (palette-template dat)))))

    (dom-ready
     (lambda ()
       (setup-palette!)
       (update-loop!)))))

(define-handler (js/base.js :content-type "application/javascript") ()
  (ps
    ;;;; base.js contains general utilities that might be useful in other JS
    ;;;; applications too. Nothing trurl-specific here.
    ;;;; NOTE - this is the third time you're importing it. Consider just starting a house-utils project or something

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
	  (> (chain thing (index-of elem)) -1)))

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

    (defun get/json (uri params callback)
      (get uri params (lambda (raw) (callback (string->obj raw)))))

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
