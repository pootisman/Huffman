#include <stdlib.h>
#include <stdio.h>
#include <memory.h>
#include <malloc.h>
#include <getopt.h>
#include "bitWriter.h"

#define ALLOWED "ASHE:D:h"
#define T 255
#define F 0
#define BLK_SIZE 8 /* Bytes */

typedef struct symbol {
  void* pOwner;
  unsigned long count;
  unsigned char symbol;
  unsigned int code;
  unsigned char codelength;
} symbol;

typedef struct TREE_NODE_ENCODE {
  struct TREE_NODE_ENCODE* up;
  struct TREE_NODE_ENCODE *left, *larray;
  unsigned long weight;
  symbol* ptr;
  struct TREE_NODE_ENCODE *right, *rarray;
} TREE_NODE_ENCODE;

typedef struct TREE_NODE_DECODE {
  struct TREE_NODE_DECODE *left, *right;
  unsigned char symbol;
} TREE_NODE_DECODE;

symbol symbols[257];
symbol* psymbols[257] = { NULL };

TREE_NODE_ENCODE* root = NULL;
TREE_NODE_DECODE* rootDe = NULL;

unsigned char leafs = 0;
unsigned short nodes = 0;

char getCodes(TREE_NODE_ENCODE* top, unsigned char depth, int codeSoFar);

void initAll(void)
{
  short i = 0;
  root = malloc(sizeof(TREE_NODE_ENCODE));
  (void)memset(root, 0, sizeof(TREE_NODE_DECODE));
  for(i = 0; i <= 256; i++) {
    symbols[i].pOwner = NULL;
    symbols[i].count = 0;
    symbols[i].code = 0;
    symbols[i].codelength = 0;
    symbols[i].symbol = i;
  }
  root->ptr = &symbols[256];
  symbols[256].pOwner = (void*)root;
}

TREE_NODE_ENCODE* findBro(TREE_NODE_ENCODE* src, unsigned char mode)
{
  if(!src) {
    (void)puts("Error: Null provided in findBro function!");
    exit(-10);
  }
  
  if(src == root) {
    return NULL;
  }
  
  if(src == src->up) {
    (void)puts("Error: tree is corrupted!");
    return NULL;
  }
  
  if(src->up->left == src) {
    return (mode == 'T') ? (src->up->right) : (src->up->left);
  } else {
    return (mode == 'R') ? (src->up->right) : (src->up->left);
  }
  
  (void)puts("Error: Unable to find bro in the tree, exiting!");
  exit(-9);
}

void rebuildWeights(TREE_NODE_ENCODE* tmp)
{
  if(tmp != root) {
    do {
      tmp = tmp->up;
      tmp->weight++;
    } while(tmp != root);
  }
}

void rebuildTree(TREE_NODE_ENCODE* src)
{
  TREE_NODE_ENCODE *cPos = NULL, *sto1 = NULL, *sto2 = NULL, *tmp = NULL;
  unsigned char found = 0;
  unsigned int i = 0;
  
  if(src == NULL) {
    (void)puts("Error:NULL provided insted of pointer in rebuildTree!");
    exit(-11);
  }
  
  if(src == root || src->up == root) {
    return;
  }
  
  cPos = src->up;
  tmp = findBro(cPos, 'T');
  
  while(src != root && cPos != root) {
    while(src != root && cPos != root && tmp->weight <= src->weight) {
      if(tmp->weight != src->weight) {
        sto1 = src;
        sto2 = tmp;
        found = 255;
      }
      
      cPos = cPos->up;
      
      if(cPos != NULL && cPos != root) {
        tmp = findBro(cPos, 'T');
      }
    }
    
    src = src->up;
    cPos = src->up;
    if(cPos != NULL && cPos != root) {
      tmp = findBro(cPos, 'T');
    }
  }
  
  if(found == 255 && sto1->weight != sto2->weight && sto1 != root && sto2 != root) {
    if(sto1->up->left == sto1) {
      found++;
    }
    
    if(sto2->up->left == sto2) {
      found = found + 2;
    }
    
    switch(found) {
      case 255: {
        sto1->up->right = sto2;
        sto2->up->right = sto1;
        break;
      }
      case 0: {
        sto1->up->left = sto2;
        sto2->up->right = sto1;
        break;
      }
      case 1: {
        sto1->up->right = sto2;
        sto2->up->left = sto1;
        break;
      }
      case 2: {
        sto1->up->left = sto2;
        sto2->up->left = sto1;
        break;
      }
    }
    
    cPos = sto1->up;
    sto1->up = sto2->up;
    sto2->up = cPos;
    
    i = abs(sto1->weight - sto2->weight);
    
    tmp = sto1;
    while(tmp != root) {
      tmp = tmp->up;
      tmp->weight += i;
    }
    
    tmp = sto2;
    while(tmp != root) {
      tmp = tmp->up;
      tmp->weight -= i;
    }
    
    (void)getCodes(root, 0, 0);
  }
}

void adaptCode(char* iName, char* oName){
  FILE *I, *O;
  unsigned int i = 0, k = 0, l = 0;
  #ifdef VIS
  unsigned int fLen, cPos, prevPos, onePercent;
  unsigned char j;
  #endif
  unsigned char tmp;
  TREE_NODE_ENCODE *cEsc = root, *temp;
  unsigned char rotations[BLK_SIZE][BLK_SIZE];
  
  if(!iName && !oName) {
    (void)puts("Error: NULL in filenames!\a");
    exit(-7);
  }
  
  I = fopen(iName, "rb");
  
  if(!I) {
    (void)puts("Error: Can't open file!\a");
    exit(-8);
  }
  
  O = fopen(oName, "wb");
  
  (void)puts("Number of leafs is variable, no summary will be displayed.");
  
  #ifdef VIS
  (void)fseek(I, 0, SEEK_END);
  fLen = ftell(I);
  (void)fseek(I, 0, SEEK_SET);
  fLen -= ftell(I);
  onePercent = fLen / 100;
  #else
  (void)fseek(I, 0, SEEK_SET);
  #endif
  
  while(!feof(I)) {
    /* Read block of bytes */
    while(fread(&(rotations[k][0]), sizeof(char), 1, I) != 0 && k < BLK_SIZE) {
      i++;
      k++;
    }
    
    /* Apply BWT */
    for(k = 1; k < BLK_SIZE; k++){
      for(l = 0; l < BLK_SIZE; l++){
        rotations[(l+k)%BLK_SIZE][k] = rotations[l][0];
      }
    }
    
    for(k = 0; k < BLK_SIZE; k++){
      
      #ifdef VIS
      if(i - cPos >= onePercent) {
        (void)putchar('/');
        for(j = 1; j < 101; j += 2) {
          (j <= ((double)i / (double)fLen * 100)) ? ((void)putchar('#')) : ((void)putchar('_'));
        }
        (void)printf("/ /%d%/", (unsigned int)((double)i / (double)fLen * 100));
        (i < fLen - onePercent) ? (void)putchar('\r') : (void)puts("");
        cPos = i;
        (void)fflush(stdout);
      }
      #endif
      
      if((TREE_NODE_ENCODE*)symbols[rotations[BLK_SIZE - 1][k]].pOwner == NULL) {
        (void)writeBitArrayToFile(O, symbols[256].codelength, reverseBits(symbols[256].codelength, symbols[256].code));
        (void)writeBitArrayToFile(O, 8, rotations[BLK_SIZE - 1][k]);
        cEsc->right = malloc(sizeof(TREE_NODE_ENCODE));
        cEsc->left = malloc(sizeof(TREE_NODE_ENCODE));
        (void)memset(cEsc->right, 0, sizeof(TREE_NODE_ENCODE));
        (void)memset(cEsc->left, 0, sizeof(TREE_NODE_ENCODE));
        cEsc->right->up = cEsc->left->up = cEsc;
        cEsc->left->ptr = &symbols[rotations[BLK_SIZE - 1][k]];
        cEsc->left->weight = symbols[rotations[BLK_SIZE - 1][k]].count = 1;
        cEsc->right->ptr = cEsc->ptr;
        cEsc->ptr = NULL;
        rebuildWeights(cEsc->left);
        rebuildTree(cEsc->left);
        (void)getCodes(root, 0, 0);
        symbols[tmp].pOwner = (void*)cEsc->left;
        cEsc = cEsc->right;
        symbols[256].pOwner = (void*)cEsc;
      } else {
        (void)writeBitArrayToFile(O, symbols[rotations[BLK_SIZE - 1][k]].codelength, reverseBits(symbols[rotations[BLK_SIZE - 1][k]].codelength, symbols[rotations[BLK_SIZE - 1][k]].code));
        temp = (TREE_NODE_ENCODE*)symbols[rotations[BLK_SIZE - 1][k]].pOwner;
        temp->weight++;
        symbols[rotations[BLK_SIZE - 1][k]].count++;
        rebuildWeights(temp);
        rebuildTree(temp);
        /*(void)getCodes(root,0,0);*/
      }
    }
  }
  dumpBuffer(O);
  (void)fwrite(&i, sizeof(int), 1, O);
  (void)fclose(I);
  (void)fclose(O);
}

unsigned long int memsum(unsigned char *ptr){
  unsigned long int sum = 0;
  unsigned char i = 0;
  
  for(i = 0; i < BLK_SIZE; i++){
    sum += (1>>i)*ptr[i];
  }
  
  return sum;
}

unsigned char uncomp_BWT_sort(unsigned char *compressed){
  unsigned char rotations[BLK_SIZE][BLK_SIZE], buffer[BLK_SIZE];
  unsigned char i = 0, j = 0;
  unsigned long int sumvec[BLK_SIZE] = {0};
  
  for(i = 0; i < BLK_SIZE; i++){
    rotations[BLK_SIZE - 1][i] = compressed[i];
  }
  
  /* Cheat and simplify comparison */
  for(i = 0; i < BLK_SIZE; i++){
    sumvec[i] = memsum(rotations[BLK_SIZE - 1][i]);
  }
}

int compare(const void *blk1, const void *blk2){
  unsigned char i = 0;
  for(i = 0; i < 8; i++){
    if(*((char *)blk1 + i) < *((char *)blk2 + i)){
      return -1;
    }else if(*((char *)blk1 + i) > *((char *)blk2 + i)){
      return 1;
    }
  }
  return 0;
}

void adaptDecode(char* iName, char* oName)
{
  FILE *I, *O;
  unsigned int oSize, i = 2, j = 0, k = 0;
  unsigned char tmp, rotations[BLK_SIZE][BLK_SIZE];
  TREE_NODE_ENCODE *temp = root, *cEsc;
  #ifdef VIS
  unsigned int fLen, cPos, prevPos, onePercent;
  unsigned char j;
  #endif
  
  if(iName == NULL || oName == NULL) {
    (void)puts("ERROR: NULL provided in adaptDecode!");
    exit(-12);
  }
  
  if((I = fopen(iName, "rb")) == NULL) {
    (void)puts("ERROR: Can't open input file.");
    exit(-13);
  }
  
  (void)fseek(I, -4, SEEK_END);
  (void)fread(&oSize, sizeof(int), 1, I);
  (void)fseek(I, 0, SEEK_SET);
  
  #ifdef VIS
  onePercent = oSize / 100;
  #endif
  
  O = fopen(oName, "wb");
  tmp = reverseBits(8, readBitsFromFile(I, 8));
  cEsc = root;
  cEsc->left = malloc(sizeof(TREE_NODE_ENCODE));
  cEsc->right = malloc(sizeof(TREE_NODE_ENCODE));
  (void)memset(cEsc->left, 0, sizeof(TREE_NODE_ENCODE));
  (void)memset(cEsc->right, 0, sizeof(TREE_NODE_ENCODE));
  cEsc->right->up = cEsc->left->up = cEsc;
  cEsc->left->ptr = &symbols[tmp];
  cEsc->left->weight = cEsc->weight = symbols[tmp].count = 1;
  cEsc->right->ptr = cEsc->ptr;
  cEsc->ptr = NULL;
  symbols[tmp].pOwner = (void*)cEsc->left;
  symbols[256].pOwner = (void*)cEsc->right;
  (void)fwrite(&tmp, sizeof(char), 1, O);
  cEsc = cEsc->right;
  
  (void)puts("Number of leafs is variable, no summary will be displayed.");
  
  while(!feof(I) && i < oSize) {
    #ifdef VIS
    if(i - cPos >= onePercent) {
      (void)putchar('[');
      for(j = 1; j < 101; j += 2) {
        (j <= ((double)i / (double)oSize * 100)) ? ((void)putchar('|')) : ((void)putchar(' '));
      }
      (void)printf("] <%d%>", (unsigned int)((double)i / (double)oSize * 100));
      (i < oSize - onePercent) ? (void)putchar('\r') : (void)puts("");
      cPos = i;
      (void)fflush(stdout);
    }
    #endif
    
    tmp = readBitFromFile(I);
    (tmp == 1) ? (temp = temp->right) : (temp = temp->left);
    if(temp->ptr != NULL) {
      if(temp->ptr == &symbols[256]) {
        tmp = reverseBits(8, readBitsFromFile(I, 8));
        cEsc->left = malloc(sizeof(TREE_NODE_ENCODE));
        cEsc->right = malloc(sizeof(TREE_NODE_ENCODE));
        (void)memset(cEsc->left, 0, sizeof(TREE_NODE_ENCODE));
        (void)memset(cEsc->right, 0, sizeof(TREE_NODE_ENCODE));
        cEsc->right->up = cEsc->left->up = cEsc;
        cEsc->left->ptr = &symbols[tmp];
        cEsc->left->weight = symbols[tmp].count = 1;
        cEsc->right->ptr = cEsc->ptr;
        cEsc->ptr = NULL;
        symbols[tmp].pOwner = (void*)cEsc->left;
        (void)fwrite(&tmp, sizeof(char), 1, O);
        cEsc = cEsc->right;
        symbols[256].pOwner = (void*)cEsc;
      } else {
        rotations[BLK_SIZE - 1][j] = temp->ptr->symbol;
        /*(void)fwrite(&temp->ptr->symbol, sizeof(char), 1, O);*/
        j++;
        tmp = temp->ptr->symbol;
        temp->weight++;
        symbols[tmp].count++;
      }
      
      if(j == BLK_SIZE){
        /* Un-BWT */
        qsort(rotations[BLK_SIZE - 1], BLK_SIZE, sizeof(unsigned char), compare);
        j = 0;
      }
      
      rebuildWeights((TREE_NODE_ENCODE*)symbols[tmp].pOwner);
      rebuildTree((TREE_NODE_ENCODE*)symbols[tmp].pOwner);
      temp = root;
      i++;
    }
  }
  dumpBuffer(O);
  (void)fclose(I);
  (void)fclose(O);
}

void genFreq(char* fname)
{
  FILE* input;
  unsigned char buffer;
  unsigned short i;
  input = fopen(fname, "rb");
  
  if(input == NULL) {
    (void)puts("Error, can't open file!\a");
    exit(-1);
  }
  
  (void)fseek(input, 0, SEEK_SET);
  
  for(i = 0; i < 256; i++) {
    symbols[i].symbol = i;
  }
  
  (void)puts("Gathering statistics...");
  
  while(!feof(input)) {
    (void)memset(&buffer, 0, sizeof(char));
    (void)fread(&buffer, sizeof(char), 1, input);
    symbols[buffer].count++;
  }
  
  for(i = 0; i < 256; i++) {
    (symbols[i].count != 0) ? (psymbols[i] = &symbols[i]) : (psymbols[i] = NULL);
  }
  
  (void)fclose(input);
  
  (void)puts("done.");
}

void srtFreq(void)
{
  unsigned short i, j, mpos;
  for(i = 0; i < 256; i++) {
    mpos = i;
    for(j = i; j < 256; j++) {
      if(symbols[j].count > symbols[mpos].count) {
        mpos = j;
      }
    }
    if(mpos != i) {
      symbols[i].symbol = symbols[i].symbol ^ symbols[mpos].symbol;
      symbols[mpos].symbol = symbols[mpos].symbol ^ symbols[i].symbol;
      symbols[i].symbol = symbols[mpos].symbol ^ symbols[i].symbol;
      
      symbols[i].count = symbols[i].count ^ symbols[mpos].count;
      symbols[mpos].count = symbols[mpos].count ^ symbols[i].count;
      symbols[i].count = symbols[mpos].count ^ symbols[i].count;
      
      psymbols[symbols[i].symbol] = &symbols[i];
      psymbols[symbols[mpos].symbol] = &symbols[mpos];
    }
  }
  
  for(i = 0; i < 256; i++) {
    if(psymbols[i] != NULL)
      if(psymbols[i]->count == 0)
        psymbols[i] = NULL;
  }
}

void printDbgFreq(void)
{
  unsigned short i;
  
  for(i = 0; i < 256; i++) {
    if(psymbols[i] != NULL)
      (void)printf("Symbol %d: %ld  -- %d  -- %d.\n\a", psymbols[i]->symbol, psymbols[i]->count, psymbols[i]->code,
                   psymbols[i]->codelength);
  }
  (void)puts("End.");
}

void buildTreeBase(void)
{
  short i;
  
  TREE_NODE_ENCODE *temp, *temp2 = NULL;
  
  for(i = 0; symbols[i].count > 0; i++) {
  }
  i = i - 1;
  leafs = i;
  
  if(i == 1 && symbols[i].count == 0) {
    (void)puts("Can't create tree base, maybe the file is empty?\n\a");
    exit(-2);
  }
  
  do {
    temp = malloc(sizeof(TREE_NODE_ENCODE));
    (void)memset(temp, 0, sizeof(TREE_NODE_ENCODE));
    temp->ptr = &symbols[i];
    temp->weight = symbols[i].count;
    temp->rarray = temp2;
    if(temp2 != NULL) {
      temp2->larray = temp;
    }
    temp2 = temp;
    i--;
  } while(i >= 0);
  
  root = temp;
}

TREE_NODE_ENCODE* getRightmost(void)
{
  TREE_NODE_ENCODE* temp = root;
  
  while(temp->rarray != NULL) {
    temp = temp->rarray;
  }
  
  return temp;
}

TREE_NODE_ENCODE* getLeftmost(void)
{
  TREE_NODE_ENCODE* temp = root;
  
  while(temp->larray != NULL) {
    temp = temp->larray;
  }
  
  return temp;
}

unsigned char isLeft(TREE_NODE_ENCODE* strt, TREE_NODE_ENCODE* tst)
{
  while(tst != NULL) {
    if(tst == strt) {
      return F;
    }
    tst = tst->larray;
  }
  return T;
}

void interchange(TREE_NODE_ENCODE* p1, TREE_NODE_ENCODE* p2)
{
  TREE_NODE_ENCODE *temp1, *temp2;
  unsigned long weight;
  symbol* symb;
  
  if(!p1 || !p2) {
    return;
  }
  
  weight = p1->weight;
  symb = p1->ptr;
  
  p1->weight = p2->weight;
  p1->ptr = p2->ptr;
  
  p2->ptr = symb;
  p2->weight = weight;
  
  temp1 = p1->left;
  temp2 = p1->right;
  
  p1->left = p2->left;
  p1->right = p2->right;
  
  p2->left = temp1;
  p2->right = temp2;
}

void srtPntrs(void)
{
  TREE_NODE_ENCODE *curMax, *cur, *temp;
  
  curMax = getLeftmost();
  
  if(!curMax->larray && !curMax->rarray) {
    return;
  }
  
  while(curMax != NULL) {
    temp = cur = curMax;
    while(cur != NULL) {
      if(cur->weight > temp->weight) {
        temp = cur;
      }
      cur = cur->rarray;
    }
    if(temp != curMax) {
      interchange(temp, curMax);
    }
    curMax = curMax->rarray;
  }
}

void buildTree(void)
{
  TREE_NODE_ENCODE *newNode, *min1, *min2, *temp;
  
  (void)puts("Building the Huffman tree...");
  
  while(root->larray != NULL || root->rarray != NULL) {
    srtPntrs();
    min1 = temp = getRightmost();
    
    while(temp != NULL) {
      if(temp->weight < min1->weight) {
        min1 = temp;
      }
      temp = temp->larray;
    }
    
    min2 = getRightmost();
    if(min2 == min1) {
      if(min2->larray != NULL) {
        min2 = min2->larray;
        goto foundLeft;
      }
      if(min2->rarray != NULL) {
        min2 = min2->rarray;
      } else {
        return;
      }
    }
    
    foundLeft:
    
    temp = getRightmost();
    
    while(temp != NULL) {
      if(temp->weight < min2->weight && temp != min1) {
        min2 = temp;
      }
      temp = temp->larray;
    }
    
    newNode = malloc(sizeof(TREE_NODE_ENCODE));
    (void)memset(newNode, 0, sizeof(TREE_NODE_ENCODE));
    
    nodes++;
    
    if(isLeft(min2, min1) == T) {
      min1 = (TREE_NODE_ENCODE*)((unsigned long)min1 ^ (unsigned long)min2);
      min2 = (TREE_NODE_ENCODE*)((unsigned long)min2 ^ (unsigned long)min1);
      min1 = (TREE_NODE_ENCODE*)((unsigned long)min2 ^ (unsigned long)min1);
    }
    
    newNode->right = min1;
    newNode->left = min2;
    newNode->weight = newNode->right->weight + newNode->left->weight;
    
    if(min1->rarray != NULL) {
      min1->rarray->larray = newNode;
      newNode->rarray = min1->rarray;
    }
    
    if(min2->larray != NULL) {
      min2->larray->rarray = newNode;
      newNode->larray = min2->larray;
    }
    
    root = newNode;
    temp = getLeftmost();
  }
  
  (void)puts("done.");
}

char getCodes(TREE_NODE_ENCODE* top, unsigned char depth, int codeSoFar)
{
  unsigned char leftDepth = 0, rightDepth = 0;
  if(top->ptr == NULL) {
    if(top->left != NULL) {
      leftDepth = getCodes(top->left, depth + 1, (codeSoFar << 1));
    }
    if(top->right != NULL) {
      rightDepth = getCodes(top->right, depth + 1, (codeSoFar << 1) + 1);
    }
    return ((leftDepth < rightDepth) ? rightDepth : leftDepth);
  } else {
    top->ptr->code = codeSoFar;
    top->ptr->codelength = depth;
    return depth;
  }
}

void writeTree(FILE* fout, TREE_NODE_ENCODE* cNode)
{
  if(!fout || !cNode) {
    (void)puts("Can't write tree to file, NULL provided.\a");
    return;
  }
  
  if(cNode == root) {
    (void)puts("Aquiring codes...");
  }
  
  if(cNode->ptr != NULL) {
    (void)writeBitToFile(fout, 0);
    (void)writeBitArrayToFile(fout, 8, cNode->ptr->symbol);
    return;
  } else {
    (void)writeBitToFile(fout, 1);
    writeTree(fout, cNode->left);
    writeTree(fout, cNode->right);
  }
  return;
  
  if(cNode == root) {
    (void)puts("done.");
  }
}

void writeToFile(char* inputName, char* outName)
{
  FILE *I, *O;
  unsigned char Ibuffer = 0;
  unsigned short i = 0;
  
  if(!inputName || !outName) {
    (void)puts("Error: NULL provided instead of string pointer.\n\a");
    exit(-3);
  }
  
  I = fopen(inputName, "rb");
  
  if(!I) {
    (void)puts("Error: Can't open input file!\n\a");
  }
  
  (void)puts("Writing compressed data to file...");
  
  O = fopen(outName, "wb");
  
  (void)fseek(I, 0, SEEK_SET);
  (void)fseek(O, 0, SEEK_SET);
  
  (void)printf("Total size:%ld.\nNumber of leafs:%d.\nNumber of nodes:%d.\n", root->weight, leafs, nodes);
  
  for(i = 0; i < 256; i++) {
    if(psymbols[i] != NULL)
      psymbols[i]->code = reverseBits(psymbols[i]->codelength, psymbols[i]->code);
  }
  
  (void)fwrite(&(root->weight), sizeof(unsigned long int), 1, O);
  
  writeTree(O, root);
  
  while(!feof(I)) {
    (void)fread(&Ibuffer, sizeof(char), 1, I);
    (void)writeBitArrayToFile(O, psymbols[Ibuffer]->codelength, psymbols[Ibuffer]->code);
  }
  
  dumpBuffer(O);
  
  (void)fclose(O);
  (void)fclose(I);
  
  (void)puts("done.");
}

void readTree(TREE_NODE_DECODE* ptr, FILE* I)
{
  unsigned char isEnd;
  
  if(!ptr || !I) {
    (void)puts("Error: Can't decode, NULL provided.\n\a");
    return;
  }
  
  isEnd = readBitFromFile(I);
  
  if(isEnd == 0) {
    ptr->symbol = reverseBits(8, readBitsFromFile(I, 8));
    return;
  } else {
    ptr->left = malloc(sizeof(TREE_NODE_DECODE));
    (void)memset(ptr->left, 0, sizeof(TREE_NODE_DECODE));
    ptr->right = malloc(sizeof(TREE_NODE_DECODE));
    (void)memset(ptr->right, 0, sizeof(TREE_NODE_DECODE));
    ptr->symbol = 0;
    readTree(ptr->left, I);
    readTree(ptr->right, I);
    return;
  }
  (void)puts("Huh?\n\a");
}

TREE_NODE_DECODE* readFromFile(char* inName)
{
  FILE *I = NULL, *O = NULL;
  TREE_NODE_DECODE* temp1 = NULL;
  unsigned char i = 0;
  unsigned long Olen = 0;
  
  I = fopen(inName, "rb");
  
  if(I == NULL) {
    return NULL;
  }
  
  O = fopen("Output", "wb");
  
  temp1 = malloc(sizeof(TREE_NODE_DECODE));
  (void)memset(temp1, 0, sizeof(TREE_NODE_DECODE));
  
  (void)fread(&Olen, sizeof(unsigned long), 1, I);
  
  (void)puts("Reading Huffman tree from file...");
  readTree(temp1, I);
  (void)puts("done.");
  
  rootDe = temp1;
  
  (void)puts("Writing data to Output file...");
  while(!feof(I) && 0 < Olen - 1) /*Throw out unneeded byte. !!!DOG_NAIL!!!*/
  {
    while(temp1->left != NULL && temp1->right != NULL) {
      i = readBitFromFile(I);
      if(i == 1) {
        temp1 = temp1->right;
      }
      if(i == 0) {
        temp1 = temp1->left;
      }
      if(feof(I)) {
        return temp1;
      }
    }
    (void)fwrite(&(temp1->symbol), sizeof(unsigned char), 1, O);
    --Olen;
    temp1 = rootDe;
  }
  (void)puts("done.");
  
  (void)fclose(I);
  (void)fclose(O);
  
  return temp1;
}

void end(TREE_NODE_ENCODE* top)
{
  if(top != NULL) {
    end(top->left);
    end(top->right);
    (void)free(top);
  }
}

void ende(TREE_NODE_DECODE* top)
{
  if(top != NULL) {
    ende(top->left);
    ende(top->right);
    (void)free(top);
  }
}

int main(int argc, char* argv[])
{
  char *name = NULL, *oname = malloc(sizeof(char) * 36), j = 0;
  unsigned char isAdaptive = 0, decode = 0, encode = 0, i = 0;
  (void)memset(oname, 0, sizeof(char) * 36);
  
  if(argc < 3) {
    (void)free(oname);
    (void)puts("Error: I require arguments.");
    exit(-8);
  }
  
  while((j = getopt(argc, argv, ALLOWED)) != -1) {
    ++i;
    switch(j) {
      case 'A':
        (void)puts("Using adaptive Huffman coding.");
        isAdaptive = 255;
        break;
      case 'S':
        (void)puts("Using non-adaptive Huffman coding.");
        isAdaptive = 0;
        break;
      case 'E':
        encode = 255;
        decode = 0;
        ++i;
        name = argv[i];
        break;
      case 'D':
        decode = 255;
        encode = 0;
        ++i;
        name = argv[i];
        break;
      case 'h':
        encode = decode = isAdaptive = 128;
        break;
    }
  }
  
  if(isAdaptive == 0 && encode == 255 && decode == 0) {
    genFreq(name);
    srtFreq();
    buildTreeBase();
    buildTree();
    (void)getCodes(root, 0, 0);
    (void)memcpy(oname, name, sizeof(char) * 32);
    oname = strcat(oname, ".hcf");
    writeToFile(name, oname);
    end(root);
  } else if(isAdaptive == 0 && decode == 255 && encode == 0) {
    rootDe = readFromFile(name);
    ende(rootDe);
  } else if(isAdaptive == 255 && encode == 255 && decode == 0) {
    (void)memcpy(oname, name, sizeof(char) * 32);
    oname = strcat(oname, ".acf");
    initAll();
    adaptCode(name, oname);
    end(root);
  } else if(isAdaptive == 255 && encode == 0 && decode == 255) {
    (void)puts("Please type in output filename(MAX 35 characters):");
    oname = fgets(oname, 36, stdin);
    *(oname + strlen(oname) - 1) = 0;
    initAll();
    adaptDecode(name, oname);
    end(root);
  } else if(isAdaptive == 128 && decode == 128 && encode == 128) {
    (void)puts("Help for Huffman compressor.\nA...Use adaptive Huffman algorithm.\nH...Use Huffman non-adaptive "
    "algorithm.\nE...Encode specified file.\nD...Decode specified file.\nh...Recall help for program.\n");
  }
  
  (void)free(oname);
  return 0;
}
