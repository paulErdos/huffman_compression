#!/usr/bin/bash

clear

cp ../hzip.st .;

./mk.tests; 

for i in 0 1 3 4 5 6 7; do
    echo $i
    diff test$i*.in test$i*.unzipped
    echo
done

echo "Test5: frequency table."
diff test5.frequencies Sample-test5-output

/afs/cats.ucsc.edu/courses/cmps104a-wm/bin/checksource hzip.st 
