NOVIS:
	gcc -std=c89 -Wall -pedantic -O2 -o bin/Huffman src/bitWriter.c src/huff.c
	strip -s bin/Huffman
VIS:
	gcc -std=c89 -Wall -pedantic -O2 -DVIS -o bin/Huffman src/bitWriter.c src/huff.c
	strip -s bin/Huffman

