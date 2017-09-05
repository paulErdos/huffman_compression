#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk/bin/gst -f

nl := Character nl.

"
Object editing
"

Character extend [
   isPrint [
      ^ (Character space <= self) & (self <= $~)
   ]
   visible [
      self isPrint ifTrue: [^ self asString]
                   ifFalse: [^ self asInteger printStringRadix: 8]
   ]
]

Object subclass: ZeroArray [
   |theArray|

   ZeroArray class >> new [
      self shouldNotImplement.
   ]

   ZeroArray class >> new: size [
      |result|
      result := super new.
      result init: size.
      ^ result
   ]

   init: size [
      theArray := Array new: size.
   ]

   size [
      ^ theArray size.
   ]

   at: index [
      ^ theArray at: index + 1.
   ]

   at: index put: value [
      ^ theArray at: index + 1 put: value.
   ]

   incr: index [
      (self at: index) isNil
         ifTrue: [ self at: index put: 0].
      self at: index put: (self at: index) + 1.
   ]

   keysAndValuesDo: aBlock [
      (0 to: (self size) - 1) do: [:index |
         aBlock value: index value: (self at: index).
      ]
   ]
]

" Verify program called correctly. "

"check for proper number of arguments"
argsize := Smalltalk arguments size.
argsize < 2 ifTrue: [
    stdout << 'Usage: hzip.st -dctu inputfile [outputfile]' << nl.
    ObjectMemory quit: 1.
].
argsize > 3 ifTrue: [
    stdout << 'Usage: hzip.st -dctu inputfile [outputfile]' << nl.
    ObjectMemory quit: 1.
].

" Parse command line arguments "

opt := Smalltalk arguments at: 1.
valid_option := (opt sameAs: '-c') | (opt sameAs: '-u') |
                (opt sameAs: '-t') | (opt sameAs: '-d').
valid_option ifFalse: [
    stdout << 'Usage: hzip.st -dctu inputfile [outputfile]' << nl.
    ObjectMemory quit: 1.
].

(opt sameAs: '-c') ifTrue: [ compress := 1. ].
(opt sameAs: '-u') ifTrue: [ uncompress := 1. ].
(opt sameAs: '-t') ifTrue: [ print_table := 1. ].
(opt sameAs: '-d') ifTrue: [ debug := 1. ].

" FILE INPUT "

"open file"
filename := Smalltalk arguments at: 2.
infile := FileStream open: filename mode: FileStream read.

"build the frequency table"
freqtable := ZeroArray new: 256.
[infile atEnd not] whileTrue: [
   |ordChar|
   ordChar := infile next asInteger.
   freqtable incr: ordChar.
].

"add entry 256 with a count of 1 to indicate EoF.
 because it's zero-indexed, entry 256 is indexed by 255"
freqtable at: 255 put: 1.

"debug: print the freqtable"
(debug = 1) ifTrue: [
   freqtable keysAndValuesDo: [:key :value |
      (value notNil and: [value > 0]) ifTrue: [
         stdout << 'freq[' << key asCharacter visible << '] = ' << value         << nl.
      ]
   ].
   freqtable inspect.
].
