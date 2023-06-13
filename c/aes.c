#include "cipher.h"
#include "tools.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>


byte *keyprocess(char *key, int keysize, int *nr) {
    int nk = keysize / 4;
    *nr = nk + 6;
    byte *w = malloc(16*(*nr+1));

    keyExpansion((byte*)key, w, nk, *nr);

    return w;
}

char* hextoascii(const char* in) {
    if (strlen(in) % 2 != 0) { return NULL; }

    char *out = malloc(strlen(in) / 2);

    for (size_t i = 0; i < strlen(in) / 2; i++) {
        sscanf(&in[i * 2], "%2hhx", &out[i]);
    }

    return out;
}


int aes_encrypt (char *data, int size, char *key, int keysize) {
    if (size % 16 != 0) { return 1; }
    if (keysize != 16 && keysize != 24 && keysize != 32) { return 1; }
    
    byte *encrypted_data = malloc(size);
    if (encrypted_data == NULL) { return 1; }
    
    int nr = 0;
    byte *w = keyprocess(key, keysize, &nr);

    for (int i = 0; i < size / 16; i++) {
        byte temp[16];
        splitArr((byte*)data, temp, i*16, i*16+16);
        cipher(temp, w, nr);
        mergeArr(temp, encrypted_data, i*16, i*16+16);
    }

    memcpy(data, encrypted_data, size);

    free(encrypted_data);
    free(w);
    return 0;
}


int aes_decrypt (char *data, int size, char *key, int keysize) {
    if (size % 16 != 0) { return 1; }
    if (keysize != 16 && keysize != 24 && keysize != 32) { return 1; }
    
    byte *encrypted_data = malloc(size * sizeof(byte));
    if (encrypted_data == NULL) { return 1; }
    
    int nr = 0;
    byte *w = keyprocess(key, keysize, &nr);

    for (int i = 0; i < size / 16; i++) {
        byte temp[16];
        splitArr((byte*)data, temp, i*16, i*16+16);
        invCipher(temp, w, nr);
        mergeArr(temp, encrypted_data, i*16, i*16+16);
    }

    memcpy(data, encrypted_data, size);

    free(encrypted_data);
    free(w);
    return 0;
}


int main (void) {
    char input[16] = "coucouaezrtgfrst";
    printf("%s\n", input);

    aes_encrypt(input, 16, "masuperclechiffredelamor", 24);
    printf("%s\n", input);

    aes_decrypt(input, 16, "masuperclechiffredelamor", 24);
    printf("%s\n", input);

    return 1;
}