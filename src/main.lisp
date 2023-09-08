(defpackage survelo
  (:use :cl))
(in-package :survelo)

(defvar *renderer* nil)
(defvar *window* nil)

(defvar ctrl-down? nil)
(defvar alt-down? nil)
(defvar shift-down? nil)

(defparameter *fps* 0)
(defparameter *fps-reset* (sdl2:get-ticks))

(defun idle (renderer draw-queue)
  (sdl2:set-render-draw-color *renderer* 255 0 0 255)
  (sdl2:render-clear renderer)

  (sdl2:set-render-draw-color *renderer* 0 255 0 255)

  (sdl2:render-fill-rect renderer (multiple-value-bind (x y)( sdl2:mouse-state)
				    (sdl2:make-rect (- x 25)
						    (- y 25)
						    50 50)))

  (sdl2:render-present renderer)
  (sleep 0.002)

  (incf *fps*)
  (when (> (- (sdl2:get-ticks) *fps-reset*) 1000)
    (setf *fps* 0)
    (setf *fps-reset* (sdl2:get-ticks))))


(defun handle-windowevent ()
  (let ((flags (sdl2:get-window-flags *window*)))
    ;; haaaack
    (if (or (position :mouse-focus flags)
	    (position :input-focus flags))
	(progn
	  ;;got focus
	  ;; (format t "We have a focus!~%")
	  )
	(progn
	  ;;lost focus
	  (setf ctrl-down? nil
		alt-down? nil
		shift-down? nil)
	  ))))

(defun event-loop (renderer)
  (sdl2:with-event-loop (:method :poll)
    (:keydown (:keysym keysym)
	      (format t "Keydown ~a~%" keysym))

    (:keyup (:keysym keysym)
	    (format t "Keydown ~a~%" keysym)
	    (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
	      (sdl2:push-event :quit)))

    (:mousebuttondown ()
		      (format t "mousebuttondown ~%"))
    (:mousebuttonup ()
		    (format t "mousebuttonup ~%"))
		    

    (:windowevent () 
		  (handle-windowevent))

    (:mousemotion (:x x :y y)
		  (format t "Mouse at ~a~%" (list x y)))
    (:idle ()
	   (idle renderer nil))

    (:quit () t)))

(defun main ()
  (sdl2:with-init (:everything)
    (format t "Using SDL Library Version: ~D.~D.~D~%"
            sdl2-ffi:+sdl-major-version+
            sdl2-ffi:+sdl-minor-version+
            sdl2-ffi:+sdl-patchlevel+)
    (sdl2:with-window (win :title "qmapper without the q" :flags '(:shown :resizable))
      (let* ((renderer (sdl2:create-renderer win)))
  	(setf *renderer* renderer)
  	(setf *window* win)
	
  	(event-loop renderer)))))

;;(main)
