#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk/bin/gst -f

"General to-do
 1, Close the files
 2. Verify the postorder traversal is done correctly by giving this program a small sample of text and constructing on paper what should be done.
"

"=====================================================================
  Classes, extensions, variable declarations. Skip to line 187 to 
  bypass this section.
  
  To do: 
  1. Clean these up. I edited some of this and I'm not using the 
     edits. They need to go. 
 ===================================================================="

compress := 0.
uncompress := 0.
print_table := 0.
debug := 0.
nl := Character nl.
tab := Character tab.

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
]

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

   postorder: visitor [
      	| i bit bits charval | 
	i := 8.
	charval := char asInteger.
	bits := '0'.
	[i > 0] whileTrue: [
		bit := charval bitAt: i.
		bits := bits, (bit asString).
		i := i - 1.
	].
	visitor value: bits.
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
      | ((count = other count) & (left char <= other char))
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

   postorder: visitor [ 
	left postorder: visitor.
	right postorder: visitor.
	visitor value: '1'.
   ]

]

"the following added to restore color code sanity"
dummy_variable := ('n' sameAs: '-c') | ('k' sameAs: '-u').

"=====================================================================
 Parse arguments and die if necessary. Read everything in as 
 necessary.
 
 Handle more pathological input later:
 1. Multiple options
 2. Invalid option
 3. first argument doesn't start with a '-'
 4. 
 ====================================================================="

"check for proper number of arguments"
argsize := Smalltalk arguments size.
(debug = 1) ifTrue: [
	Smalltalk arguments inspect.
	stdout << argsize << nl.
].
argsize < 2 ifTrue: [
	stdout << 'Program called improperly. Quitting!.' << nl.
	ObjectMemory quit: 1.
].
argsize > 3 ifTrue: [
        stdout << 'Program called improperly. Quitting!.' << nl.
        ObjectMemory quit: 1.
].

"set flags to indicate what the user wants to do"
"Ideas for improvement:
1. put all of the options in a table so that they're not so messily dealt with
2.
"
opt := Smalltalk arguments at: 1.
valid_option := (opt sameAs: '-c') | (opt sameAs: '-u') | 
                (opt sameAs: '-t')|(opt sameAs: '-d').
valid_option ifFalse: [ ObjectMemory quit: 1 ].

"debug: are the options valid?"
(debug = 1) ifTrue: [ 
	valid_option ifTrue: [ stdout << 'Option is valid' << nl. ].
	valid_option ifFalse: [ 
        stdout << 'fucking evil nonsense options. Quitting!' << nl. ].
].

(opt sameAs: '-c') ifTrue: [ compress := 1. ].
(opt sameAs: '-u') ifTrue: [ uncompress := 1. ].
(opt sameAs: '-t') ifTrue: [ print_table := 1. ].
(opt sameAs: '-d') ifTrue: [ debug := 1. ].

"debug: what are we doing?"
(debug = 1) ifTrue: [
	(compress == 1) 	ifTrue: [ stdout << 'we are compressing' << nl. ]
				ifFalse: [ stdout << 'we are not compressing' << nl. ].
	(uncompress == 1)	ifTrue: [ stdout << 'we are uncompressing' << nl. ]
        	        	ifFalse: [ stdout << 'we are not uncompressing' << nl. ].
	(debug == 1) 		ifTrue: [ stdout << 'we are debugging' << nl. ]
        	        	ifFalse: [ stdout << 'we are not debugging' << nl. ].
	(print_table == 1) 	ifTrue: [ stdout << 'we are printing the tree' << nl. ]
        	        	ifFalse: [ stdout << 'we are not printing the tree' << nl. ].
].

"Open the file, read it in, and create the frequency table"
"Unmet requirements:
1. Catch case in which the file won't open (e.g., a bogus file)
   Check out catfile.st
"
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
	      stdout << 'freq[' << key asCharacter visible << ']=' << value         << nl.
	   ]
	].
	freqtable inspect.
].


"=====================================================================
 Create the Huffman tree and encoding table
 ====================================================================="

"Iterate over the frequency table. For each nonzero element create a 
leaf node and insert that node into a SortedCollection."
"make a new sortedcollection and fill it with leaves"
"the key is the character and the count is the frequency"
sortcol := SortedCollection new.

freqtable keysAndValuesDo: [:key :value |
   (value notNil and: [value > 0]) ifTrue: [
	sortcol add: (Leaf new: key count: value).    
   ]
].
"debug: show the sortcol"
(debug = 1) ifTrue: [
        stdout << 'The freshly-built sortcol:' << nl.
        sortcol inspect
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

"Traverse the tree to build the encoding string table"
t := sortcol removeFirst.
encoding_strings := SortedCollection new.

t inorder: [:char :string |
	"this order used to sort by char"
	leaf := Leaf new: string count: char.
	encoding_strings add: leaf.
"	stdout << (char asCharacter) << tab << string << nl.
"] prefix: ''.

"debug: show the Huffman string table"
(debug = 1) ifTrue: [ encoding_strings inspect ].

"print the encoding table"
"col1: character, printable if possible, int if not
 col2: frequency
 col3: encoding string
 Format should be identical to 
  '%3d, %5d, %s' in C. 
   %nk means: 'print k using at least n characters. Pad the front with whitespace to make the quota'
"
"Unmet reqs: 
1. make the format nice
"
(print_table = 1) ifTrue: [ 
	tab := Character tab. 
	encoding_strings do: [:item| 
		character := (item count) asCharacter. 
                stdout << 'character: ' << character.
		stdout << tab.
		
		frequency := (freqtable at: item count). 
		stdout << 'frequency: ' << frequency.
		stdout << tab.
		
		huffcode := item char.
		stdout << 'huffcode: ' << huffcode << nl.
	].
].

"if the -t option is not specified, write out the encoding table as follows:
 1. Perform a postorder traversal of the decoding tree writing out one bit at a time in big-endian format (write the high bit first)
 2. For each leaf, write out a zero bit followed by the eight bits of the corresponding byte.
 3. If the byte is 0, write out bit 8 (the high bit), which will be 0 for a byte of 0 and 1 for a byte of 256 (the EOF marker)
 4. For each interior node write out a 1 bit.
"
"
1. Edit tree to allow postorder traversal.
"
"stdout << nl << 'testing' << nl.
"Object subclass: BitStream [
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
]

(print_table = 1) ifFalse: [
        stdout << nl << 'postorder traversal' << nl. 

	bitstring := ''.
	t postorder: [:char |
		bitstring := bitstring, char.
	].

	stdout << bitstring << nl.
"	
        t postorder: [:char :string |
		stdout << 'char: ' << char << ' string: ' << nl.
		index := 8.
		[index > 0] whileTrue: [
        		current_bit := char bitAt: index.
		        stdout << current_bit. 
        		index := index - 1.
		].
		stdout << nl << 'string: ' << string << nl.
	] prefix: ''.
"].



"=====================================================================
 Extra stuff included for my convenience. This should not exist in 
 final version.
 ====================================================================="

"stuff to copy/paste"
"
stdout << 'print_table: ' 
(print_table = 1) ifTrue: [ stdout << 'True' << nl ]
		 ifFalse: [ stdout << 'False' << nl ].
stdout << 'debug: '
(debug = 1) ifTrue: [ stdout << 'True' << nl ]
            ifFalse: [ stdout << 'False' << nl ].
stdout << 'uncompress: '
(uncompress = 1) ifTrue: [ stdout << 'True' << nl ]
            ifFalse: [ stdout << 'False' << nl ].
stdout << 'compress: '
(compress = 1) ifTrue: [ stdout << 'True' << nl ]
            ifFalse: [ stdout << 'False' << nl ].
"
