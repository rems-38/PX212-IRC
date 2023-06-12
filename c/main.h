typedef unsigned char byte;

void fillTemp(byte in[], byte temp[], int length);
byte multi(byte a, byte b);
void addRoundKey(byte state[], byte w[], int round);
void subBytes(byte state[]);
void switchColRows(byte state[]);
void shiftRows(byte state[]);
void mixColumns(byte state[]);
void subWord(byte state[4], byte out[]);
void rotWord(byte state[4], byte out[]);
void rcon(int i, byte out[4]);
void keyExpansion(byte key[], byte w[]);
void cipher(byte in[], byte out[], byte w[]);
void printByte(byte in[], int length);