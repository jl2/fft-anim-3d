;;;; fft-anim-3d.asd
;;;;
;;;; Copyright (c) 2016 Jeremiah LaRocco <jeremiah.larocco@gmail.com>

(asdf:defsystem #:fft-anim-3d
  :description "Describe fft-anim-3d here"
  :author "Jeremiah LaRocco <jeremiah.larocco@gmail.com>"
  :license "ISC (BSD-like)"
  :depends-on (#:qt
               #:qtools
               #:qtgui
               #:qtcore
               #:anim-utils
               #:mixalot-mp3
               #:qtopengl
               #:cl-opengl
               #:cl-glu
               #:trivial-garbage
               #:trivial-main-thread)
  :serial t
  :components ((:file "package")
               (:file "fft-anim-3d")))

