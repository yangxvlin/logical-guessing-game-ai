ghc -O2 --make Proj1test

# rm *.hi
# rm *.o

time ./Proj1test "AS" "2C" "AH" "8D"
time ./Proj1test "2C" "AS" "AH"
time ./Proj1test "2C" "AS"
time ./Proj1test "6D" "QS"

rm Proj1test