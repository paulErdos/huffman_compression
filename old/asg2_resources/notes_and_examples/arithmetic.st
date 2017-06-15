#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk/bin/gst -f
"This is intended to be a hand-copied version of the same file in 
 Mackey's directory of example programs. It is my intention to copy and 
 understand all of the programs in this directory in order to make more 
 substantial my time spent learning languages.
"

nl := Character nl.

""
"There are only three precedences in Smaltalk"
"1. Unary"
"2. Binary, which evaluates strictly left to right"
"3. Keyword"
""

"Printing is done like C++"
stdout << 'Beginning' << nl.
stdout << 'x := 3 + 4 * 5.' << nl.

"Assignment and arithmetic"
x := 3 + 4 * 5. "this evaluates to 35. wtf. (3 + 4) * 5 :((("
stdout << 'Inspecting a variable:' << nl.
x inspect.

"Printing a variable"
stdout << x << nl.

"Smalltalk allegedly has extended precision"
stdout << 'y1 := 64 raisedTo: 20.' << nl.
y1 := 64 raisedTo: 20.
y1 inspect.
stdout << y1 << nl.

stdout << 'y2 := 6 * 6 raisedTo: 3 * 4.' << nl.
y2 := 6 * 6 raisedTo: 3 * 4.
y2 inspect.
stdout << y2 << nl.

"Rational numbers"
stdout << 'z := 22/7' << nl.
z := 22/7.
z inspect.
stdout << z << nl.
