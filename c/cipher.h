typedef unsigned char byte;

void addRoundKey(byte state[], byte w[], int round);
void subBytes(byte state[], const byte box[256]);
void shiftRows(byte state[]);
void invShiftRows(byte state[]);
void mixColumns(byte state[], const byte polyMix[16]);
void subWord(byte state[4]);
void rotWord(byte state[4]);
void rcon(int i, byte out[4]);
void keyExpansion(byte key[], byte w[], int nk, int nr);
void cipher(byte in[], byte w[], int nr);
void invCipher(byte in[], byte w[], int nr);