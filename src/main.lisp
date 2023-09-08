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



(defparameter *vertex-shader-source*
"#version 330 core
layout (location = 0) in vec3 position;
void main()
{
gl_Position = vec4(position.x, position.y, position.z, 1.0);
}")

(defparameter *fragment-shader-source*
"#version 330 core
out vec4 color;
void main()
{
color = vec4(1.0f, 0.5f, 0.2f, 1.0f);
}")


(defun assert-no-shader-errors (shader-id)
  (let ((success (cffi:foreign-alloc :int :initial-element 0)))
    (unwind-protect
         (progn
           (%gl:get-shader-iv shader-id :compile-status success)
           (when (/= 1 (cffi:mem-aref success :int))
             (error "OpenGl error:~%~A" (gl:get-shader-info-log shader-id))))
      (cffi:foreign-free success))))

(defun assert-no-program-errors (program-id)
  (let ((success (cffi:foreign-alloc :int :initial-element 0)))
    (unwind-protect
         (progn
           (%gl:get-program-iv program-id :link-status success)
           (when (/= 1 (cffi:mem-aref success :int))
             (error "OpenGl error:~%~A" (gl:get-program-info-log program-id))))
      (cffi:foreign-free success))))


(defun get-shader ()
  (let ((vertex-shader (gl:create-shader :vertex-shader))
        (fragment-shader (gl:create-shader :fragment-shader)))
    (gl:shader-source vertex-shader *vertex-shader-source*)
    (gl:compile-shader vertex-shader)
    (assert-no-shader-errors vertex-shader)

    (gl:shader-source fragment-shader *fragment-shader-source*)
    (gl:compile-shader fragment-shader)
    (assert-no-shader-errors fragment-shader)

    (let ((program (gl:create-program)))
      (gl:attach-shader program vertex-shader)
      (gl:attach-shader program fragment-shader)
      (gl:link-program program)
      (assert-no-program-errors program)

      (gl:delete-shader vertex-shader)
      (gl:delete-shader fragment-shader)
      program)))

(defun make-model ()  
  (let* ((vec #(-0.5 -0.5 0.0
		0.5 -0.5 0.0
		0.0  0.5 0.0))
	 (triangle 
           (loop :with gl-array = (gl:alloc-gl-array :float (length vec))
                 :for i :from 0 :below (length vec) :do
                   (setf (gl:glaref gl-array i)
                         (elt vec i))
                 :finally (return gl-array)))
	 (vao (gl:gen-vertex-array))
	 
	 (gl-buffer-address (gl:gen-buffer)))

    (gl:bind-vertex-array vao)

    (gl:bind-buffer :array-buffer gl-buffer-address)
    (gl:buffer-data :array-buffer
                    :static-draw
                    triangle)
    (gl:vertex-attrib-pointer 0 3 :float 0 (* 3 (cffi:foreign-type-size :float)) 0)
    (gl:enable-vertex-attrib-array 0)

    (gl:bind-buffer :array-buffer 0)
    (gl:bind-vertex-array 0)
    vao))      
      
		       

(defun idle (win shader vao)

  (gl:clear :depth-buffer-bit :color-buffer-bit)
  (gl:use-program shader)
  (gl:bind-vertex-array vao)
  (gl:draw-arrays :triangles 0 3)
  (gl:bind-vertex-array 0)
  (sdl2:gl-swap-window win)
  (sleep 0.100)

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

(defun event-loop (win shader vao)
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
	   (idle win shader vao))
    (:quit () t)))

(defun main ()
  
  (sdl2:with-init (:everything)
    (format t "Using SDL Library Version: ~D.~D.~D~%"
            sdl2-ffi:+sdl-major-version+
            sdl2-ffi:+sdl-minor-version+
            sdl2-ffi:+sdl-patchlevel+)
    
    (sdl2:gl-set-attr :context-major-version 3)
    (sdl2:gl-set-attr :context-minor-version 3)
    (sdl2:gl-set-attr :context-profile-mask
                      sdl2-ffi:+sdl-gl-context-profile-core+)
    (sdl2:gl-set-attr :doublebuffer 1)
    #+darwin
    (sdl2:gl-set-attr :context-forward-compatible-flag
                      sdl2-ffi:+sdl-gl-context-forward-compatible-flag+)
    (sdl2:with-window (win :w 800 :h 600 :title "qmapper without the q" :flags '(:shown :resizable :opengl))
      (gl:viewport 0 0 800 600)
      
      (let* ((renderer (sdl2:create-renderer win))
	     (shader (get-shader))
	     (vao (make-model)))
	(setf *renderer* renderer)
	(setf *window* win)
	
	(event-loop  win shader vao)))))


;;(main)
