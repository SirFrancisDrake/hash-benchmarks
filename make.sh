echo "Removing various trash "
rm makelog.txt
rm *.hi
rm *.o
rm HashmapTest 
rm HashtablesBTest
rm HashtablesCTest
rm HashtablesLTest
rm MapTest
rm RedisTest
rm TestRunner
echo "Making HashmapTest.hs"
ghc --make -O2 -rtsopts HashmapTest >> makelog.txt
echo "Making HashtablesBTest.hs"
ghc --make -O2 -rtsopts HashtablesBTest >> makelog.txt
echo "Making HashtablesCTest.hs"
ghc --make -O2 -rtsopts HashtablesCTest >> makelog.txt
echo "Making HashtablesLTest.hs"
ghc --make -O2 -rtsopts HashtablesLTest >> makelog.txt
echo "Making MapTest.hs"
ghc --make -O2 -rtsopts MapTest >> makelog.txt
echo "Making RedisTest.hs"
ghc --make -O2 -rtsopts RedisTest >> makelog.txt
echo "Making TestRunner.hs"
ghc --make -O2 -rtsopts TestRunner >> makelog.txt
echo "Removing *.hi and *.o files"
rm *.hi
rm *.o
echo "All done."
