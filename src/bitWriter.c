#include <stdlib.h>
#include <stdio.h>
#include "bitWriter.h"

unsigned char curLen = 0, iCurLen = 33, buff = 0, iBuff = 0;
const long int msk = 1;

char writeBitToFile(FILE *out, unsigned char bit){
  if(out == NULL){
    (void)puts("Error, can't write bit to NULL file!\n");
    return -1;
  }
  
  if(curLen >= CURUNIT){
    (void)fwrite(&buff, sizeof(char), 1, out);
    buff = 0;
    curLen = 0;
  }
  
  buff = buff + (bit<<curLen);
  curLen++;
  
  return 1;
}

char writeBitArrayToFile( FILE *out, unsigned char length, int array)
{
  unsigned char i = 0;
  
  if(out == NULL){
    (void)puts("Error, can't write bits to NULL file!\n");
    return -1;
  }
  
  while(i < length){
    if(curLen >= CURUNIT)
    {
      (void)fwrite(&buff, sizeof(char), 1, out);
      buff = 0;
      curLen = 0;
    }
    
    buff = buff + (((array>>i)&1) <<curLen);
    curLen++;
    i++;
  }
  
  return 1;
}

char readBitFromFile(FILE *fin){
  unsigned char res = 0;
  if(fin == NULL){
    (void)puts("Error, can't read bit from NULL file!\n");
    return -1;
  }
  
  if(iCurLen >= CURUNIT && !feof(fin)){
    (void)fread( &iBuff, sizeof(char), 1, fin);
    iCurLen = 0;
  }
  
  res = (iBuff & (msk<<iCurLen)) >> iCurLen;
  iCurLen++;
  
  return res;
}

char readBitsFromFile(FILE *fin, char length){
  unsigned char res = 0;
  
  if(fin == NULL){
    (void)puts("Error, can't read bits from NULL file!\n");
    exit(-2);
  }
  
  length = length - 1;
  
  while(length >= 0){
    if(iCurLen >= CURUNIT && !feof(fin)){
      (void)fread(&iBuff, sizeof(char), 1, fin);
      iCurLen = 0;
    }
    res = res + (((iBuff>>iCurLen)&1)<<length);
    iCurLen++;
    length--;
  }
  
  return res;
}

void dumpBuffer(FILE *out)
{
  if(out == NULL){
    (void)puts("Error, can't dump buffer to NULL file!\n");
  }
  
  if(curLen < CURUNIT && curLen != 0){
    (void)fwrite(&buff, sizeof(char), 1, out);
  }
}

int reverseBits(unsigned char buffLen, int array)
{
  int bbuff = 0, j = 0;
  
  if(buffLen == 1)
  {
    return array;
  }
  
  buffLen = buffLen - 1;
  
  for(j = buffLen; j >= 0; j--)
  {
    bbuff = bbuff + ((( array & (msk << j)) >> j) << (buffLen-j));
  }
  
  return bbuff;
}
