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
         stdout << 'freq[' << key asCharacter visible << '] = ' << value << nl.
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
        stdout << item << nl.
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

sortcol_ inspect.
hufmann_t := sortcol_ removeFirst.
hufmann_t inspect.
"
(sortcol_ removeFirst) inspect.
"
encoding_table := SortedCollection new.

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


stdout << nl << 'Before vising the Hufmann tree' << nl.
hufmann_t inorder: [:char :string |
   encoding_table add: (Encoding new: char string: string).
] prefix: ''.

(print_table = 1) ifTrue: [
    "This is probably not the way to do it in the final version."
    1 to: (encoding_table size) do: [ :i |
        (encoding_table at: i) display.
        (sortcol_copy at: i) display.
    ].

"    encoding_table inspect."
].

" sortcol_ should be empty"

