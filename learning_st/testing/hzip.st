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
    stderr << 'Usage: hzip.st -dctu inputfile [outputfile]' << nl.
    ObjectMemory quit: 1.
].
argsize > 3 ifTrue: [
    stderr << 'Usage: hzip.st -dctu inputfile [outputfile]' << nl.
    ObjectMemory quit: 1.
].

" Parse command line arguments "

opt := Smalltalk arguments at: 1.
valid_option := (opt sameAs: '-c') | (opt sameAs: '-u') |
                (opt sameAs: '-t') | (opt sameAs: '-d').
valid_option ifFalse: [
    stderr << 'Usage: hzip.st -dctu inputfile [outputfile]' << nl.
    ObjectMemory quit: 1.
].

|outfile|
outfile := stdout.

(opt sameAs: '-c') ifTrue: [
    "The input file is compressed... "
    compress := 1.

    "... and written to the output file, if specified ... "
    (Smalltalk arguments size) = 3 ifTrue: [
        |outfilename|
        outfilename := Smalltalk arguments at: 3.
        [
            outfile := FileStream open: outfilename mode: FileStream write.
        ] on: SystemExceptions.FileError do: [:signal |
            |errno|
            errno := File errno.
            stdout flush.
            stderr << 'Error: ' << signal messageText << nl.
            stderr flush.
        ]
    ]
].

(opt sameAs: '-u') ifTrue: [
    uncompress := 1.

    "... and written to the output file, if specified ... "
    Smalltalk arguments size = 3 ifTrue: [
        |outfilename|
        outfilename := Smalltalk arguments at: 3.
        [
            outfile := FileStream open: outfilename mode: FileStream write.
        ] on: SystemExceptions.FileError do: [:signal |
            |errno|
            errno := File errno.
            stdout flush.
            stderr << 'Error: ' << signal messageText << nl.
            stderr flush.
        ]
    ]
].

(opt sameAs: '-t') ifTrue: [
    (Smalltalk arguments size) = 2 ifTrue: [
        print_table := 1.
    ] ifFalse: [
        stderr << 'Error: Output file may not be specified with -t option.'
               << nl.
        ObjectMemory quit: 1.
    ].
].
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
         outfile << 'freq[' << key asCharacter visible << '] = ' << value << nl.
      ]
   ].
"   freqtable inspect."
].

"3b."

Object subclass: Leaf [
   |char count|
   char [ ^ char ]
   count [ ^ count ]

   Leaf class >> new [
      self shouldNotImplement
   ]

   Leaf class >> new: aChar count: aCount [
      |result|
      result := super new.
      result setChar: aChar andCount: aCount.
      ^result
   ]

   setChar: aChar andCount: aCount [
      char := aChar.
      count := aCount.
   ]

   <= other [
      ^ (count < other count)
      | ((count = other count) & (char < other char))
   ]

   printBase: aStream [
      ^ aStream << self class << '(' << char << ',' << count
   ]

   printOn: aStream [
      (self printBase: aStream) << ')'.
   ]

   inorder: visitor prefix: string [
      visitor value: char value: string.
   ]

    postorder: visitor prefix: string [
        " For each leaf, write out a 0 bit... "
        | leafcontents nextBit |
        leafcontents := '0'.

        " ... followed by the eight bits of the corresponding byte. "
        8 to: 1 by: -1 do: [:x |
            x = 0 ifTrue: [
                nextBit := '0'.
            ] ifFalse: [
                x = 256 ifTrue: [
                    nextBit := '1'.
                ] ifFalse: [
                    nextBit := (((char asInteger) bitAt: x) asString).
                ].
            ].
            leafcontents := leafcontents, nextBit.
        ].

        visitor value: leafcontents.
    ]

]

Leaf subclass: Tree [
   |left right|

   Tree class >> new: aChar count: aCount [
      self shouldNotImplement
   ]

   Tree class >> new: aChar count: aCount left: aLeft right: aRight [
      |result|
      result := super new: aChar count: aCount.
      result setLeft: aLeft andRight: aRight.
      ^ result
   ]

   <= other [
      ^ (count < other count)
      | ((count = other count) & (char < other char))
   ]

   setLeft: aLeft andRight: aRight [
      left := aLeft.
      right := aRight.
   ]

   printOn: aStream [
      (self printBase: aStream) << ',' << left << ',' << right << ')'.
   ]

   inorder: visitor prefix: string [
      left inorder: visitor prefix: string, '0'.
      right inorder: visitor prefix: string, '1'.
   ]

    postorder: visitor prefix: string [
        left postorder: visitor prefix: string.
        right postorder: visitor prefix: string.
        visitor value: '1'.

    ]

]

"Iterate over the freqtable"
sortcol := SortedCollection new.
sortcol_copy := SortedCollection new.


" and for each non-zero element, create a leaf
  node with a character and a count, and insert that leaf node into
  a priority queue.
"
freqtable keysAndValuesDo: [:key :value |
   (value notNil and: [value > 0]) ifTrue: [
      l := Leaf new: key asCharacter count: value.
      sortcol add: l.

      sortcol_copy add: l.
   ]
].

(debug = 1) ifTrue: [
    sortcol do: [:item |
        outfile << item << nl.
    ].
].

"3c."

sortcol_ := SortedCollection new.

" Repeatedly remove the two smallest elements from the priority queue... "
[sortcol notEmpty] whileTrue: [
   |first second left right character count|
   first := sortcol removeFirst.

   (sortcol isEmpty) ifFalse: [
      second := sortcol removeFirst.

      " ... creating a new tree ... "
      " The smaller tree or leaf becomes the left child... "
      (first <= second) ifTrue: [
           left := first.
           " ... and the larger the right child. "
           right := second.
      ]
      ifFalse: [
           left := second.
           right := first.
      ].

      " The character in the new tree is the left child's character. "
      character := left char.

      " And I assume this means that the count becomes the left child's char's
        count.
      "
      count := left count + right count.

      t := Tree new: character count: count left: left right: right.

      sortcol add: t.
   ]
   ifTrue: [
      sortcol_ add: first.
   ].
].

"sortcol_ inspect."
hufmann_t := sortcol_ removeFirst.
"hufmann_t inspect."
"
(sortcol_ removeFirst) inspect.
"
encoding_table := SortedCollection new.
encoding_table_ := Array new: 256.

Object subclass: Encoding [
    |char string|
    char [^ char]
    string [^ string]
    Encoding class >> new [self shouldNotImplement]

    Encoding class >> new: character string: string [
        |result|
        result := super new.
        result setChar: character setString: string.
        ^ result.
    ]

    setChar: myChar setString: myString [
        char := myChar.
        string := myString.
    ]

    <= other [
       ^ (char < other char)
       | ((char = other char) & (string <= other string))
    ]
"
    <= other [
        char = other char
            ifTrue: [^ string <= other string]
            ifFalse: [^ char <= other char ].
    ]
"

    printOn: aStream [
        ^ aStream << '[' << char << '] = ' << string << Character nl.
    ]
]

" 3d. "

hufmann_t inorder: [:char :string |
   encoding_table add: (Encoding new: char string: string).
    encoding_table_ at: (char asInteger) put: string.
] prefix: ''.

" 3e. "

Object subclass: OutBits [
   |bitIndex currentByte myStream|
   OutBits class >> new [
      self shouldNotImplement.
   ]
   OutBits class >> new: fileStream [
      |result|
      result := super new.
      result init: fileStream.
      ^result
   ]
   clearByte [
      bitIndex := 8.
      currentByte := 0.
   ]
   init: fileStream [
      myStream := fileStream.
      self clearByte.
   ]
   flushByte [
      bitIndex = 8 ifFalse: [
         myStream nextPutByte: currentByte.
         self clearByte.
      ]
   ]
   writeBit: bit [
      currentByte := currentByte bitAt: bitIndex put: (bit asInteger).
      bitIndex := bitIndex - 1.
      bitIndex = 0 ifTrue: [self flushByte].
   ]
]

writeBitArray := [:outfile :outstring|
   |outBytes|
   outBytes := OutBits new: outfile.
   outstring do: [:bit| outBytes writeBit: bit].
   outBytes flushByte.
].

" If the -t option is specified ... "
(print_table = 1) ifTrue: [
    " ... write out the encoding table sorted by character. "
    1 to: (encoding_table size) do: [ :i |
        | thisChar |
        thisChar := (encoding_table at: i) char.

        ((32 < (thisChar asInteger)) and: [(thisChar asInteger) < 127]) ifTrue: [
            outfile << '  ' << thisChar.
            "thisChar display."
        ] ifFalse: [
            " Print formatting "
            (thisChar asInteger < 10) ifTrue: [ outfile << ' '. ].
            (thisChar asInteger < 100) ifTrue: [ outfile << ' '. ].
            outfile << thisChar asInteger.
        ].


        (freqtable at: (thisChar asInteger)) < 10000 ifTrue: [ outfile << ' '. ].
        (freqtable at: (thisChar asInteger)) < 1000 ifTrue: [ outfile << ' '. ].
        (freqtable at: (thisChar asInteger)) < 100 ifTrue: [ outfile << ' '. ].
        (freqtable at: (thisChar asInteger)) < 10 ifTrue: [ outfile << ' '. ].
        (freqtable at: (thisChar asInteger)) display.

        outfile << ' '.

        (encoding_table at: i) string displayNl.

    ].

    " Then stop. "
    print_table = 1 ifTrue: [ ObjectMemory quit. ].

].

(compress = 1) ifTrue: [
    " 3f. If the -t option is not specified, write out the encoding "
    " table as follows. "

    " Perform a postorder traversal of the encoding tree... "
    | outputstring |
    outputstring := ''.
    hufmann_t postorder: [:nodeType  |
        outputstring := outputstring, nodeType.
    ] prefix: ''.

    writeBitArray value: outfile value: outputstring.
    outfile close.
].

(compress = 1) ifTrue: [
    | n_writtenbits n_remainingbits outputstring |

    outputstring := ''.

    " Reopen the file... "
    filename := Smalltalk arguments at: 2.
    infile := FileStream open: filename mode: FileStream read.

    n_writtenbits := 0.
    [infile atEnd not] whileTrue: [
        |ordChar|
        ordChar := infile next asInteger.

        outputstring := outputstring, (encoding_table_ at: ordChar).
"        outfile << (encoding_table_ at: ordChar)."

        n_writtenbits := n_writtenbits + 1.
    ].

    outputstring := outputstring, (encoding_table_ at: 256).
    n_writtenbits := n_writtenbits + 1.

    n_remainingbits := n_writtenbits \\ 8.

    n_remainingbits to: 1 by: -1 do: [ :i |
        outputstring := outputstring, '0'.
    ].

    writeBitArray value: outfile value: outputstring.
].
