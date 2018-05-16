;;;; chip-play.asd

(asdf:defsystem #:chip-play
  :description "Describe chip-play here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :depends-on (:claw
               :bodge-chipmunk
               :chipmunk-blob
               :cepl.sdl2
               :nineveh)
  :serial t
  :components ((:file "package")
               (:file "base")))
