ghc -O2 --make MyTest

rm *.hi
rm *.o

time ./MyTest