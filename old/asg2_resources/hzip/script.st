Character extend [
   isPrint [
      ^ (Character space <= self) & (self <= $~)
   ]
   visible [
      self isPrint ifTrue: [^ '$', self asString]
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
      | ((count = other count) & (char <= other char))
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

nl := Character nl.

"open the file and read it in"
filename := 'Terence_Mckenna_Shamans_among_the_Machines.txt'.
infile := FileStream open: filename mode: FileStream read.

"make a frequency table and fill it"
freqtable := ZeroArray new: 256.

[infile atEnd not] whileTrue: [
   |ordChar|
   ordChar := infile next asInteger.
   freqtable incr: ordChar.
].
freqtable at: 255 put: 1.

"make a sortedcollection and fill it"
sortcol := SortedCollection new.
freqtable keysAndValuesDo: [:key :value |
   (value notNil and: [value > 0]) ifTrue: [
        sortcol add: (Leaf new: key count: value).
   ]
].

"iterate over the sortcol and create the tree"
[sortcol size > 1] whileTrue: [
        first := sortcol removeFirst.
        second := sortcol removeFirst.

        "smaller count becomes left child"
        sum := (first count) + (second count).
        first_char := first char.
        second_char := second char.
        (first count) <= (second count) ifTrue: [ sortcol add: (Tree new: first_char count: sum left: first right: second) ].
        (first count) > (second count) ifTrue: [ sortcol add: (Tree new: second_char count: sum left: second right: first) ].

].

"build the string table"
t := sortcol removeFirst.
encoding_strings := SortedCollection new.
t inorder: [:char :string |
        "this order used to sort by char"
        leaf := Leaf new: string count: char.
        encoding_strings add: leaf.
] prefix: ''.

