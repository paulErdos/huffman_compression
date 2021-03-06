#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk/bin/gst -f

" Parse command line arguments "

"execname := '$RCSfile: cmdline.st,v $' =~ '.*: (.*),v.*' at: 1."

nl := Character nl.

"stdout << 'execname: ' << execname << nl.
stdout << 'version: ' << Smalltalk version << nl.
stdout << 'hostSystem: ' << Smalltalk hostSystem << nl."

"stdout << 'arguments: ' << Smalltalk arguments << nl."
"
(1 to: Smalltalk arguments size)
do: [:index |
    'argument[' display.
    index display.
    '] = ""' display.
    (Smalltalk arguments at: index) display.
    '""' displayNl.
].

(1 to: Smalltalk getArgc)
do: [:argi |
   'argv[' display.
   argi display.
   '] = ""' display.
   (Smalltalk getArgv: argi) display.
   '""' displayNl.
].
"

" ARGUMENTS "

"Here is what's going on in the following block."
" https://www.gnu.org/software/smalltalk/manual-base/html_node/SystemDictionary_002dcommand_002dline.html"

""
options := Dictionary new.
Smalltalk arguments: '-d -c -t -u -a -b --foo --bar'
do: [:opt :arg |
   (opt = d) displayNl.
   opt displayNl.
   opt = 'd' ifFalse: ['debug opt not set' displayNl].
   opt = '-d' ifFalse: ['debug opt not set' displayNl].
   'arguments:do: ' display.
   opt display.
   ' ' display.
   arg displayNl.
]
ifError: [
   'Error: invalid option' displayNl.
"   '--foo'""this line appears to do nothing."
]

'$RCSfile: cmdline.st,v $' display.
'finished.' displayNl.

"TEST: cmdline.st foo bar baz"


" FILE INPUT "
execname := 'filein.st'.
stdout << 'Starting ' << execname << '.'; nl.
FileStream fileIn: 'tree.st'.
stdout << 'Finished ' << execname << '.'; nl.

"TEST: filein.st"

