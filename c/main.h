typedef unsigned char byte;

void byteXor(byte a[], const byte b[], int length);
byte multi(byte a, byte b);
void addRoundKey(byte state[], byte w[], int round);
void subBytes(byte state[]);
void switchColRows(byte state[]);
void shiftRows(byte state[]);
void mixColumns(byte state[]);
void subWord(byte state[4]);
void rotWord(byte state[4]);
void rcon(int i, byte out[4]);
void splitArr(const byte in[], byte out[], int start, int end);
void mergeArr(const byte in[], byte out[], int start, int end);
void keyExpansion(const byte key[], byte w[], int nk, int nr);
void cipher(byte in[], byte out[], byte w[]);
void printByte(byte in[], int length);