;;;; fft-anim-3d.lisp
;;;;
;;;; Copyright (c) 2016 Jeremiah LaRocco <jeremiah.larocco@gmail.com>

(in-package #:fft-anim-3d)

(named-readtables:in-readtable :qtools)

(declaim (optimize (speed 3) (safety 0) (size 0) (debug 0)))

(defparameter *fps* 60)
(defparameter *fft-window-size* 512)

(defun random-list-of-int (length max-int)
  (loop for i below length 
     collect (random max-int)))


(defun random-between (min-val max-val)
  (+ min-val (random (- max-val min-val))))

(defstruct cube
  (width      0.145 :type double-float)
  (x-location (make-animated-var :val (random-between -20.0 20.0)
                                 :buckets (random-list-of-int 4 (/ *fft-window-size* 2)))
              :type animated-var)
  (y-location (make-animated-var :val (random-between -20.0 20.0)
                                 :buckets (random-list-of-int 4 (/ *fft-window-size* 2)))
              :type animated-var)
  (z-location (make-animated-var :val (random-between -20.0 20.0)
                                 :buckets (random-list-of-int 4 (/ *fft-window-size* 2)))
              :type animated-var)
  (x-rotation (make-animated-var :val 0.0d0
                                 :buckets (random-list-of-int 4 (/ *fft-window-size* 2)*))
              :type animated-var)
  (y-rotation (make-animated-var :val 0.0d0
                                 :buckets (random-list-of-int 4 (/ *fft-window-size* 2)*))
              :type animated-var)
  (z-rotation (make-animated-var :val 0.0d0
                                 :buckets (random-list-of-int 4 (/ *fft-window-size* 2)*))
              :type animated-var))

(defstruct scene
  "The parameters used for drawing a spirograph image."
  (cubes (loop for i below 800 collect (make-cube))))

(defun reset-cube (cube)
  (with-slots (x-location y-location z-location x-rotation y-rotation z-rotation) cube
    (reset-var x-location)
    (reset-var y-location)
    (reset-var z-location)
    (reset-var x-rotation)
    (reset-var y-rotation)
    (reset-var z-rotation)))

(defun reset-scene (scene)
  (with-slots (cubes) scene
    (dolist (cube cubes)
      (reset-cube cube))))

(defun compute-colors (cube)
  (with-slots (x-rotation y-rotation z-rotation) cube
    
    (values (with-slots (buckets) x-rotation
              (/ (car buckets) 1.0 *fft-window-size*))
            (with-slots (buckets) y-rotation
              (/ (car buckets) 1.0 *fft-window-size*))
            (with-slots (buckets) z-rotation
              (/ (car buckets) 1.0 *fft-window-size*)))))


(define-widget cube-animator (QGLWidget)
  ((the-mp3 :initform nil)
   (start-time :initform 0)
   (song-duration :initform 0)
   (window-buffer :initform (make-array *fft-window-size* 
                                        :element-type '(complex double-float)
                                        :adjustable nil
                                        :fill-pointer nil))
   (left-fft-data :initform (make-array *fft-window-size* 
                                        :element-type '(complex double-float)
                                        :adjustable nil
                                        :fill-pointer nil))
   (right-fft-data :initform (make-array *fft-window-size* 
                                         :element-type '(complex double-float)
                                         :adjustable nil
                                         :fill-pointer nil))
   
   (scene :initform (make-scene)))
  (:documentation "The scene-drawer widget draws a cube using the parameters specified in scene."))

(define-subwidget (cube-animator timer) (q+:make-qtimer cube-animator)
  (setf (q+:single-shot timer) nil))

(define-initializer (cube-animator setup)
  (q+:start timer (round (/ 1000 *fps*)))
  (setf (q+:auto-fill-background cube-animator) nil)
  (setf (q+:auto-buffer-swap cube-animator) nil))

(define-slot (cube-animator tick) ()
  (declare (connected timer (timeout)))
  (q+:repaint cube-animator))

(define-override (cube-animator initialize-G-L) ()
  (gl:enable :line-smooth :polygon-smooth
             :depth-test :depth-clamp :alpha-test))

(define-override (cube-animator resize-g-l) (width height)
  )

(defun max-radius (scene)
  (sqrt (* 30 1.5 1.5)))

(defun render-cube (cube)
  (let ((cube-verts #(
                      ;; Top corners
                      #(-0.5 -0.5 0.5)
                      #(-0.5 0.5 0.5)
                      #(0.5 0.5 0.5)
                      #(0.5 -0.5 0.5)

                      ;; Bottom corners
                      #(-0.5 -0.5 -0.5)
                      #(-0.5 0.5 -0.5)
                      #(0.5 0.5 -0.5)
                      #(0.5 -0.5 -0.5)))

        (cube-normals #(#(0.0 0.0 1.0)
                        #(0.0 0.0 -1.0)
                        #(0.0 -1.0 0.0)
                        #(0.0 1.0 0.0)
                        #(-1.0 0.0 0.0)
                        #(1.0 0.0 0.0)
                        
                        
                        ))
        (indices #(
                   #(0 1 2 3 0 0 0 0)
                   #(4 5 6 7 1 1 1 1)
                   #(0 1 5 4 2 2 2 2)
                   #(2 3 7 6 3 3 3 3)
                   #(1 2 6 5 5 5 5 5)
                   #(3 0 4 7 4 4 4 4))))

    (gl:push-matrix)
    (with-slots (width x-location y-location z-location x-rotation y-rotation z-rotation) cube
      (multiple-value-call #'gl:color (compute-colors cube))
      (gl:rotate (var-val x-rotation) 1.0 0.0 0.0)
      (gl:rotate (var-val y-rotation) 0.0 1.0 0.0)
      (gl:rotate (var-val z-rotation) 0.0 0.0 1.0)
      (gl:translate (var-val x-location) (var-val y-location) (var-val z-location))
      (gl:scale width width width))
          

    (gl:with-primitives :quads
      (loop for index across indices
         do
           (dotimes (i 4)
             (gl:normal (aref (aref cube-normals (aref index (+ 4 i))) 0)
                        (aref (aref cube-normals (aref index (+ 4 i))) 1)
                        (aref (aref cube-normals (aref index (+ 4 i))) 2))
             (gl:vertex (aref (aref cube-verts (aref index i)) 0)
                        (aref (aref cube-verts (aref index i)) 1)
                        (aref (aref cube-verts (aref index i)) 2)))))
    (gl:pop-matrix)))

(define-override (cube-animator paint-g-l paint) ()
  "Handle paint events."
  (with-slots (cubes) scene
    (let* ((max-radius (max-radius scene))
           (width (q+:width cube-animator))
           (height (q+:height cube-animator))
           (x-aspect-ratio (if (< height width)
                               (/ height width 1.0d0)
                               1.0d0))
           (y-aspect-ratio (if (< height width)
                               1.0d0
                               (/ width height 1.0d0)))
           (song-location (/ (+ 1 (- (get-internal-real-time) start-time)) 1.0 internal-time-units-per-second))
           (win-center (ceiling (max 0 
                                     (- (* 44100 song-location)
                                        (round (/ *fft-window-size* 2)))))))

      (with-finalizing 
          ;; Create a painter object to draw on
          ((painter (q+:make-qpainter cube-animator)))

        (q+:begin-native-painting painter)
        (gl:viewport 0 0 width height)
        (gl:matrix-mode :projection)
        (gl:load-identity)

        (gl:viewport 0 0 width height)
        (gl:matrix-mode :projection)
        (gl:load-identity)
        (glu:perspective 50 (/ height width) 1.0 5000.0)
        (glu:look-at 32 32 32
                     0 0 0
                     0 1 0)

        (gl:clear-color 0 0 0 1)
        (gl:enable :line-smooth :polygon-smooth
                   :depth-test :depth-clamp :alpha-test)
        (gl:polygon-mode :front-and-back :line)

        (gl:matrix-mode :modelview)
        (gl:load-identity)

        (gl:clear :color-buffer :depth-buffer)

        ;; Clear the background
        (when (and the-mp3 (< (/ (- (get-internal-real-time) start-time) 1.0 internal-time-units-per-second) song-duration))

          (bordeaux-fft:windowed-fft! (mp3-file-left-channel the-mp3)
                                      window-buffer left-fft-data
                                      win-center *fft-window-size* 'bordeaux-fft:triangle)
          (bordeaux-fft:windowed-fft! (mp3-file-right-channel the-mp3) 
                                      window-buffer right-fft-data
                                      win-center *fft-window-size* 'bordeaux-fft:triangle)
          
          ;; Actual drawing goes here.  In this case, just a line.
          (gl:push-matrix)

          
          (dolist (cube cubes)
            (render-cube cube)
            (with-slots (width x-location y-location z-location x-rotation y-rotation z-rotation) cube
              (step-var x-rotation 0.0125d0 left-fft-data right-fft-data)
              (step-var y-rotation 0.0125d0 left-fft-data right-fft-data)
              (step-var z-rotation 0.0125d0 left-fft-data right-fft-data)))

          (gl:pop-matrix)

          ;; Update offsets based on fft-data
          )

        (q+:swap-buffers cube-animator)
        (q+:end-native-painting painter)
        (trivial-garbage:gc)))))

(define-widget controller-widget (QWidget)
               ()
               (:documentation "A Spirograph animator and its controls."))

(define-subwidget (controller-widget animation-widget) (make-instance 'cube-animator)
  "The spirograph-drawer itself.")

(define-subwidget (controller-widget mp3-file-edit) (q+:make-qlineedit controller-widget)
  "The currently open file."
  (setf (q+:read-only mp3-file-edit) t))

(defun format-list (lst)
  (format nil "~{~d~^ ~}" lst))

(define-subwidget (controller-widget reset-button) (q+:make-qpushbutton "Reset" controller-widget)
  "Reset all-offsets to 0")

(define-slot (controller-widget reset-pressed) ()
  "Handle the reset button."
  (declare (connected reset-button (released)))
  (reset-scene (slot-value animation-widget 'scene)))

(define-subwidget (controller-widget control-layout) (q+:make-qvboxlayout controller-widget)
  "Layout all of the control widgets in a vertical box layout."

  ;; Create horizontal layouts to hold the labels and spinboxes
  (let ((file-layout (q+:make-qhboxlayout))
        (other-layout (q+:make-qhboxlayout)))
    
    ;; Populate the horizontal layouts and add them to the top level vertical layout

    (q+:add-widget file-layout (q+:make-qlabel "Filename: " controller-widget))
    (q+:add-widget file-layout mp3-file-edit)

    (q+:add-widget other-layout reset-button)

    (q+:add-layout control-layout file-layout)
    (q+:add-layout control-layout other-layout)

    ;; Finally add the spirograph viewer directly to the vertical layout
    (q+:add-widget control-layout animation-widget)))


(define-signal (controller-widget open-mp3) (string))

(define-slot (controller-widget open-mp3) ((file-name string))
  (declare (connected controller-widget (open-mp3 string)))
  (let* ((new-mp3-file (read-mp3-file file-name))
         (sduration (mp3-file-duration-in-seconds new-mp3-file))
         (tframes (ceiling (* sduration *fps*))))

    (setf (q+:text mp3-file-edit) file-name)
    (reset-scene (slot-value animation-widget 'scene))
    (setf (slot-value animation-widget 'the-mp3) new-mp3-file)
    (setf (slot-value animation-widget 'song-duration) sduration)
    (setf (slot-value animation-widget 'start-time) (get-internal-real-time))))


(define-widget main-window (QMainWindow)
  ((mixer :initform (mixalot:create-mixer))
   (current-stream :initform nil)))

(define-override (main-window close-event) (ev)
  (mixalot:mixer-remove-all-streamers mixer)
  (mixalot:destroy-mixer mixer)
  (q+:accept ev))


(define-menu (main-window File)
  (:item ("Open MP3" (ctrl o))
         (open-mp3 main-window))
  (:separator)
  (:item ("Quit" (ctrl alt q))
         (q+:close main-window)))

(define-menu (main-window Help)
  (:item "About"
         (q+:qmessagebox-information
          main-window "About"
          "Interactively view and manipulate FFT data.")))

(define-subwidget (main-window scene-controller) (make-instance 'controller-widget)
  "The central controller-widget.")


(define-slot (main-window open open-mp3) ()
  (let ((filename (q+:qfiledialog-get-open-file-name main-window "Select File"
                                                     (q+:qdesktopservices-storage-location 
                                                      (q+:qdesktopservices.music-location))
                                                     "*.mp3")))
    (when (and filename (> (length filename) 0))
      (signal! scene-controller (open-mp3 string) filename)
      (when current-stream (mixalot:mixer-remove-streamer mixer current-stream))
      (setf current-stream (mixalot-mp3:make-mp3-streamer filename))
      (mixalot:mixer-add-streamer mixer current-stream))))


(define-initializer (main-window setup)
  "Set the window title and set the fft-controls to be the central widget."
  (setf (q+:window-title main-window) "Interactive FFT Explorer")
  (setf (q+:central-widget main-window) scene-controller))

(defun main ()
  "Create the main window."
  (trivial-main-thread:call-in-main-thread #'mixalot:main-thread-init)
  (with-main-window (window (make-instance 'main-window))))
