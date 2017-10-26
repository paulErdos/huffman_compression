#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk/bin/gst -f

nl := Character nl.

"============================================================================"
" Object editing                                                             "
"============================================================================"

Object subclass: Leaf [
   |char count|
   char [ ^ char ]
   count [ ^ count ]
   isTree [ ^ false ]
   isLeaf [ ^ true ]

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
            nextBit := (((char asInteger) bitAt: x) asString).
            leafcontents := leafcontents, nextBit.
        ].

        " As a special case, if the byte is 0, write out bit 8, which will be
          a 0 for a byte value of 0, and a 1 for a byte value of 256 (the
          EOF marker).
        "
        (char asInteger) = 256 ifTrue: [
            leafcontents := leafcontents, '1'.
        ].

        (char asInteger) = 0 ifTrue: [
            leafcontents := leafcontents, '0'.
        ].

        visitor value: leafcontents.
    ]

]

Leaf subclass: Tree [
   |left right|

   left [ ^ left ]
   right [ ^ right ]
   isTree [ ^ true ]
   isLeaf [ ^ false ]

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

"=== 3g. ============================================================"
        " For each interior node, write out a 1 bit. "
        visitor value: '1'.

    ]

]

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

Object subclass: BitStream [
   |bitIndex byte myStream|
   BitStream class >> new: fileStream [
      |result|
      result := super new.
      result init: fileStream.
      ^result
   ]
   init: fileStream [
      myStream := fileStream.
      bitIndex := 1.
   ]
   nextBit [
      bitIndex = 1 ifTrue: [
         byte := myStream next.
         bitIndex := 9.
      ].
      bitIndex := bitIndex - 1.
      ^byte value bitAt: bitIndex
   ]
   atEnd [
      ^bitIndex = 1 and: [myStream atEnd]
   ]
   currByte [
      ^byte
   ]
]

Object subclass: Stack [
    | theArray theIndex |

    Stack class >> new [
        self shouldNotImplement.
    ]

    Stack class >> new: size [
        | result |
        result := super new.
        result init: size.
        ^ result
    ]

    init: size [
        theArray := Array new: size.
        theIndex := 0.
    ]

    push: value [
        theIndex := theIndex + 1.
        theArray at: theIndex put: value.
    ]

    pop [
        theIndex := theIndex - 1.
        ^ theArray at: (theIndex + 1).
    ]

    isEmpty [
        ^ (theIndex = 0).
    ]
]


"============================================================================"
" Verify program called correctly.                                           "
"============================================================================"

" Variable declarations "

|outfile|
outfile := stdout.


" Check for proper number of arguments "

argsize := Smalltalk arguments size.
argsize < 2 ifTrue: [
    stderr << 'Usage: hzip.st -dctu inputfile [outputfile]' << nl.
    ObjectMemory quit: 1.
].
argsize > 3 ifTrue: [
    stderr << 'Usage: hzip.st -dctu inputfile [outputfile]' << nl.
    ObjectMemory quit: 1.
].

"============================================================================"
" Initialize program state.                                                  "
"============================================================================"

" Parse command line arguments "
opt := Smalltalk arguments at: 1.
valid_option := (opt sameAs: '-c') | (opt sameAs: '-u') |
                (opt sameAs: '-t') | (opt sameAs: '-d').
valid_option ifFalse: [
    stderr << 'Usage: hzip.st -dctu inputfile [outputfile]' << nl.
    ObjectMemory quit: 1.
].


"============================================================================"
" Set up configuration based on arguments.                                   "
"============================================================================"

"The input file is compressed... "
(opt sameAs: '-c') ifTrue: [
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


" The input file is assumed to be compressed... "
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

" The compression algorith is activated "
(opt sameAs: '-t') ifTrue: [
    (Smalltalk arguments size) = 2 ifTrue: [
        " And the decoding tree is printed to the stanadard output. "
        print_table := 1.
    ] ifFalse: [
        " The output filename may not be specified. "
        stderr << 'Error: Output file may not be specified with -t option.'
               << nl.
        ObjectMemory quit: 1.
    ].
].


" Debug information is printed for the benefit of the application author. "
(opt sameAs: '-d') ifTrue: [ debug := 1. ].

writeBitArray := [:outfile :outstring|
   |outBytes|
   outBytes := OutBits new: outfile.
   outstring do: [:bit| outBytes writeBit: bit].
   outBytes flushByte.
].


"============================================================================"
" 3. Compression                                                             "
"============================================================================"

" The compression algorithm reads the file twice. Once to construct the
  encoding tree, and if the -c rather than the -t option is specified, a
  second time to perform the compression. It proceeds as follows: "

"=== 3a ====================================================================="

" Read in the input file and create a frequency table... "
freqtable := ZeroArray new: 257.
filename := Smalltalk arguments at: 2.
infile := FileStream open: filename mode: FileStream read.

[infile atEnd not] whileTrue: [
    |ordChar|
    ordChar := infile next asInteger.

    " ... counting the number of times each character appears on input. "
    freqtable incr: ordChar.
].

" Possible error? "
" Add entry 256 with a count of 1 to indicate EoF. "
freqtable at: 256 put: 1.

" Debug: print the frequency table"
(debug = 1) ifTrue: [
   freqtable keysAndValuesDo: [:key :value |
      (value notNil and: [value > 0]) ifTrue: [
         outfile << 'freq[' << key asCharacter visible << '] = ' << value << nl.
      ]
   ].
].


"=== 3b ====================================================================="

sortcol := SortedCollection new.
sortcol_copy := SortedCollection new.

" Iterate over the frequency table... "
freqtable keysAndValuesDo: [:key :value |
    " ... and for each nonzero element ... "
    (value notNil and: [value > 0]) ifTrue: [
        " ... create a leaf node ... "
        l := Leaf new: key asCharacter count: value.
        " ... and insert that node into a priority queue. """
        sortcol add: l.

        sortcol_copy add: l.
    ]
].

(debug = 1) ifTrue: [
    outfile << 'Debug 3b' << nl.
    sortcol do: [:item |
        outfile << item << nl.
    ].
    outfile << 'Debug 3b complete.'.
    outfile << ''.
].

"=== 3c ====================================================================="

sortcol_ := SortedCollection new.

" Repeatedly ... "
[sortcol notEmpty] whileTrue: [
   |first second left right character count|

    " ... remove the two ... "
    first := sortcol removeFirst.

    (sortcol isEmpty) ifFalse: [
        " ... two smallest element in the priority queue ... "
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
encoding_table_ := ZeroArray new: 257.


"=== 3d. ====================================================================="

hufmann_t inorder: [:char :string |
    encoding_table add: (Encoding new: char string: string).
    encoding_table_ at: (char asInteger) put: string.
] prefix: ''.

"=== 3e. ===CHECKED==========================================================="

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


"=== 3f ====================================================================="

(compress = 1) ifTrue: [
    " If the -t option is not specified, write out the encoding "
    " table as follows. "

    " Perform a postorder traversal of the encoding tree... "
    | n_writtenbits n_remainingbits outputstring |
    outputstring := ''.

    hufmann_t postorder: [:nodeType  |
        outputstring := outputstring, nodeType.
    ] prefix: ''.

    " From Piazza: write an extra 1 after you finish writing your
      tree to the file to indicate during reconstruction of the
      tree to indicate the border between tree code and ciphertext"
    outputstring := outputstring, '1'.

    outputstring displayNl.

"=== 3h ====================================================================="

    " Reopen the file... "
    filename := Smalltalk arguments at: 2.
    infile := FileStream open: filename mode: FileStream read.

    [infile atEnd not] whileTrue: [
        |ordChar|
        ordChar := infile next asInteger.

        outputstring := outputstring, (encoding_table_ at: ordChar).
    ].

    " Add EOF marker "
    outputstring := outputstring, (encoding_table_ at: 256).

    " Pad with zeroes out to a byte boundary, if needed. "
    n_remainingbits := (8 - (outputstring size) \\ 8).
    n_remainingbits to: 1 by: -1 do: [ :i |
        outputstring := outputstring, '0'.
    ].


    writeBitArray value: outfile value: outputstring.

    outfile close.
].


"============================================================================"
" 4. Decompression                                                           "
"============================================================================"

(uncompress = 1) ifTrue: [
    | filename infile stdinBits theNextBit theNextByte theStack oneSubtree zeroSubtree newTree doneReadingTree treePointer theDecodingTree outputstring |

    theNextBit := 0.
    theStack := Stack new: 1000.
    doneReadingTree := 0.
    outputstring := ''.
    " Open the file "

    filename := Smalltalk arguments at: 2.
    infile := FileStream open: filename mode: FileStream read.
    stdinBits := BitStream new: infile.

    [stdinBits atEnd] whileFalse: [
        (doneReadingTree = 0) ifTrue: [
            " Reset the byte "
            theNextByte := 0.

            " Read one bit. "
            theNextBit := stdinBits nextBit.

            " If it is a 0... "
            theNextBit = 0 ifTrue: [
                " ... read the next 8 bits and reconstruct the byte,
                  giving a value from 0 to 255."
                8 to: 1 by: -1 do: [ :i |
                    theNextBit := (stdinBits nextBit).
                    theNextByte := theNextByte bitAt: i put: theNextBit.
                ].

                " If the value is 0... "
                (theNextByte = 0) ifTrue: [
                    " ... read one more bit... "
                    theNextBit := (stdinBits nextBit).

                    " ... and if it is a 1... "
                    (theNextBit = 1) ifTrue: [
                        " ...add 256 to the byte, giving the encoding
                          for EOF"
                        theNextByte := theNextByte + 256.
                    ]
                ].

                " Then push this byte onto a stack. "
                theStack push: (Leaf new: (theNextByte asCharacter) count: 0).

            " If it is a 1, pop the 1-subtree from the stack, then pop the
              0-subtree from the stack, and create a new tree with both of
              those children and push the new tree back on the stack. "
            ] ifFalse: [
                " When the stack is empty, there is only one tree left,
                  the decoding tree. "
                oneSubtree := theStack pop.

                (theStack isEmpty) ifTrue: [
                    doneReadingTree := 1.
                    " Now loop over the rest of the input file to
                      reconstruct the original file: initialize a pointer to
                      the root of the decoding tree. "
                    theDecodingTree := oneSubtree.
                    treePointer := theDecodingTree.
                ].

                (doneReadingTree = 0) ifTrue: [
                    zeroSubtree := theStack pop.

                    newTree := Tree new: '0' count: 0 left: zeroSubtree right: oneSubtree.

                    theStack push: newTree.
                ].
            ].
        ] ifFalse: [
            " Read a single bit... "
            theNextBit := (stdinBits nextBit).

            " ...and use it to move down the 0-link or the 1-link to
              the child of the current node. "
            (theNextBit = 0) ifTrue: [
                treePointer := (treePointer left). 
            ] ifFalse: [
                treePointer := (treePointer right).
            ].

            " If this node is a leaf... "
            (treePointer isLeaf) ifTrue: [
                " Write out the corresponding byte... "
                ((treePointer char) asInteger) = 256 ifTrue: [
                    " Then we're done. "
                    writeBitArray value: outfile value: outputstring.
                    outfile close.
                    ObjectMemory quit: 0.

                ].

                outputstring := outputstring, ((treePointer char) asString).

                "... and reset the pointer back to the root of the tree. "
                treePointer := theDecodingTree.
            ].

        ].
    ].

].

