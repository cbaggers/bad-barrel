(in-package #:chip-play)

(defvar *initd* nil)
(defvar *running* nil)
(defvar *ball-body* nil)

(defun init ()
  (unless *initd*
    (cepl:repl)
    (setf *initd* t)))

(cepl:defun-g basic-vs ((vert :vec2))
  (values (v! vert 0 1)
          (+ (* vert 0.5) 0.5)))

(cepl:defun-g basic-fs ((uv :vec2)
                        &uniform
                        (pos :vec2)
                        (line0 :vec4)
                        (line1 :vec4))
  (let* ((xy (s~ gl-frag-coord :xy))
         (r 15)
         (d (nineveh.sdf.2d:merge-simple
             (circle (translate xy pos) r)
             (nineveh.sdf.2d:merge-simple
              (line xy (s~ line0 :xy) (s~ line0 :zw) 5)
              (line xy (s~ line1 :xy) (s~ line1 :zw) 5))))
         (a (mask-fill d)))
    (mix (v! 0.03 0.03 0.05 1.0)
         (v! 1 0.03 0.05 1.0)
         a)))

(cepl:defpipeline-g basic-pline ()
  (basic-vs :vec2)
  (basic-fs :vec2))

(defun step-demo (ball-body space time-step line0-v4 line1-v4)
  (c-with ((pos %cp:vect)
           (vel %cp:vect))
    (setf (cepl:viewport-resolution (cepl:current-viewport))
          (cepl:surface-resolution (cepl:current-surface)))
    (nineveh:as-frame
      (%cp:body-get-position pos ball-body)
      (%cp:body-get-velocity vel ball-body)
      (let ((pos (v! (pos :x) (pos :y))))
        ;;(format t "~%pos: ~a vel: ~a,~a" pos (vel :x) (vel :y))
        (cepl:map-g #'basic-pline (nineveh:get-quad-stream-v2)
                    :pos pos
                    :line0 line0-v4
                    :line1 line1-v4))
      (%cp:space-step space time-step))))

(defun reset-ball ()
  (let ((ball-body *ball-body*))
    (with-many-free ((new-pos (cp:v 300d0 600d0))
                     (new-vel (cp:v 0d0 0d0)))
      (%cp:body-set-position ball-body new-pos)
      (%cp:body-set-velocity ball-body new-vel))))

(defun make-line (line-v4 space)
  (with-many-free ((a (cp:v (float (x line-v4) 0d0)
                            (float (y line-v4) 0d0)))
                   (b (cp:v (float (z line-v4) 0d0)
                            (float (w line-v4) 0d0))))
    (let ((ground (%cp:segment-shape-new (%cp:space-get-static-body space) a b 0d0)))
      (%cp:shape-set-friction ground 1d0)
      (%cp:space-add-shape space ground)
      ground)))

(defun main ()
  (init)
  ;; cpVect is a 2D vector and cpv() is a shortcut for initializing them.
  (with-free (gravity (cp:v 0d0 -100d0))
    ;; Create an empty space.
    (let ((space (%cp:space-new)))
      (%cp:space-set-gravity space gravity)
      ;; Add a static line segment shape for the ground.
      ;; We'll make it slightly tilted so the ball will roll off.
      ;; We attach it to a static body to tell Chipmunk it shouldn't be movable.
      (let* ((line0-v4 (v! -400d0 300d0 400d0 0d0))
             (line1-v4 (v! 150d0 200d0 400d0 400d0))
             (line0 (make-line line0-v4 space))
             (line1 (make-line line1-v4 space)))
        ;; Now let's make a ball that falls onto the line and rolls off.
        ;; First we need to make a cpBody to hold the physical properties of the object.
        ;; These include the mass, position, velocity, angle, etc. of the object.
        ;; Then we attach collision shapes to the cpBody to give it a size and shape.
        (let* ((radius 15d0)
               (mass 0.1d0)
               ;; The moment of inertia is like mass for rotation
               ;; Use the cpMomentFor*() functions to help you approximate it.
               (moment (%cp:moment-for-circle mass 0d0 radius cp:+vzero+))
               ;; The cpSpaceAdd*() functions return the thing that you are adding.
               ;; It's convenient to create and add an object in one line.
               (ball-body (%cp:space-add-body space (%cp:body-new mass moment))))
          (setf *ball-body* ball-body)
          (with-free (position (cp:v 0d0 300d0))
            (%cp:body-set-position ball-body position))
          ;; Now we create the collision shape for the ball.
          ;; You can create multiple collision shapes that point to the same body.
          ;; They will all be attached to the body and move around to follow it.
          (let ((ball-shape (%cp:space-add-shape space
                                                 (%cp:circle-shape-new ball-body radius cp:+vzero+))))
            (%cp:shape-set-friction ball-shape 0.7d0)
            ;; Now that it's all set up, we simulate all the objects in the space by
            ;; stepping forward through time in small increments called steps.
            ;; It is *highly* recommended to use a fixed size time step.
            (let ((time-step (float 1/60 0d0)))
              (setf *running* t)
              (unwind-protect
                   (loop
                      :while *running*
                      :do
                      (livesupport:update-repl-link)
                      (livesupport:continuable (cepl:step-host))
                      (livesupport:continuable (step-demo ball-body space time-step line0-v4 line1-v4)))
                (setf *running* nil)))
            ;; Clean up our objects and exit!
            (setf *ball-body* nil)
            (%cp:shape-free ball-shape)
            (%cp:body-free ball-body)
            (%cp:shape-free line0)
            (%cp:shape-free line1)
            (%cp:space-free space)))))))

(defun stop-demo ()
  (if *running*
      (setf *running* nil)
      (print "- wasnt running -")))
