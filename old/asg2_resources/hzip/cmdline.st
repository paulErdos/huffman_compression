#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk/bin/gst -f
""
"Parse command line arguments"
""

execname := '$RCSfile: cmdline.st,v $' =~ '.*: (.*),v.*' at: 1.
nl := Character nl.

stdout << 'execname: ' << execname << nl.
stdout << 'version: ' << Smalltalk version << nl.
stdout << 'hostSystem: ' << Smalltalk hostSystem << nl.
stdout << 'arguments: ' << Smalltalk arguments << nl.


stdout << nl << 'block 1 start' << nl.
(1 to: Smalltalk arguments size)
do: [:index |
    'argument[' display.
    index display.
    '] = "' display.
    (Smalltalk arguments at: index) display.
    '"' displayNl.
].
stdout << 'block 1 stop' << nl << nl.

stdout << nl << 'block 2 start' << nl.
(1 to: Smalltalk getArgc)
do: [:argi |
   'argv[' display.
   argi display.
   '] = "' display.
   (Smalltalk getArgv: argi) display.
   '"' displayNl.
].
stdout << 'block 2 stop' << nl << nl.

stdout << nl << 'block 3 start' << nl.
Smalltalk arguments: '-a -b --foo --bar'
do: [:opt :arg |
   'arguments:do: ' display.
   opt display.
   ' ' display.
   arg displayNl.
]
ifError: [
   'Error: invalid option' displayNl.
   '--foo'
].
stdout << 'block 3 stop' << nl << nl.

'$RCSfile: cmdline.st,v $' display.
'finished.' displayNl.

"TEST: cmdline.st foo bar baz"

