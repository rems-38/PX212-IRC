typedef unsigned char byte;

byte *keyprocess(char *key, int keysize, int *nr);
char* hextoascii(const char* in);
char* asciitohex(const char* in);
int aes_encrypt (char *data, int size, char *key, int keysize);
int aes_decrypt (char *data, int size, char *key, int keysize);