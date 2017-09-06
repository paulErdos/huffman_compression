#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk/bin/gst -f

Smalltalk at: #x put: 0 !

x := Array new: 20 !

(x at: 1) printNl !

x at: 1 put: 99 !

(x at: 1) printNl !
