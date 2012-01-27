echo "Removing various trash "
rm makelog.txt > /dev/null
rm *.hi > /dev/null
rm *.o > /dev/null
rm HashtablesBTest > /dev/null
rm HashtablesCTest > /dev/null
rm HashtablesLTest > /dev/null
rm MapTest > /dev/null
rm RedisTest > /dev/null
rm TestRunner > /dev/null
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
rm *.hi > /dev/null
rm *.o > /dev/null
echo "All done."
