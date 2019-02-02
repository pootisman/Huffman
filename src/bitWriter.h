#ifndef BITWRITER_H
#define BITWRITER_H

#define CURUNIT 8

char writeBitToFile(FILE *out, unsigned char bit);
char writeBitArrayToFile(FILE *out, unsigned char length, int array);

char readBitFromFile(FILE *fin);
char readBitsFromFile(FILE *fin, char length);

void dumpBuffer(FILE *out);
int reverseBits(unsigned char buffLen, int array);

#endif
