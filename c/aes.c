/** @file aes.c
 *  @brief AES encryption and decryption protocol
 *
 *  Contient les fonctions de chiffrement et de déchiffrement AES
 *  pour des données de taille multiple de 16 octets.
 *
 *  @author Mazzone Rémi (rems-38)
 *  @author Moussu Guillemot (guillemotmoussu)
 *  @bug No known bugs.
 */


/* -- Includes -- */
#include "cipher.h"
#include "tools.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>


/* -- Functions -- */
byte *keyprocess(char *key, int keysize, int *nr) {
    int nk = keysize / 4;
    *nr = nk + 6;
    byte *w = malloc(16*(*nr+1));

    keyExpansion((byte*)key, w, nk, *nr);

    return w;
}

char* hextoascii(const char* in) {
    if (strlen(in) % 2 != 0) { return NULL; }

    char *out = malloc(strlen(in) / 2 + 1);

    for (size_t i = 0; i < strlen(in) / 2; i++) {
        sscanf(&in[i * 2], "%2hhx", &out[i]);
    }
    out[strlen(in) / 2] = '\0';
    return out;
}

char* asciitohex(const char* in) {
    char *out = malloc(strlen(in) * 2 + 1);

    for (size_t i = 0; i < strlen(in); i++) {
        sprintf(&out[i * 2], "%02x", (unsigned char)in[i]);
    }
    out[strlen(in) * 2] = '\0';
    return out;
}


int aes_encrypt (char *data, int size, char *key, int keysize, int cbc) {
    if (size % 16 != 0) { return 1; }
    if (keysize != 16 && keysize != 24 && keysize != 32) { return 1; }
    byte initVect[16] = {0};
    
    byte *encrypted_data = malloc(size);
    if (encrypted_data == NULL) { return 1; }
    
    int nr = 0;
    byte *w = keyprocess(key, keysize, &nr);

    for (int i = 0; i < size / 16; i++) {
        byte temp[16];
        splitArr((byte*)data, temp, i*16, i*16+16);

        if (cbc) { byteXor(temp, initVect, 16); }

        cipher(temp, w, nr);
        mergeArr(temp, encrypted_data, i*16, i*16+16);
        mergeArr(temp, initVect, 0, 16);
    }

    memcpy(data, encrypted_data, size);

    free(encrypted_data);
    free(w);
    return 0;
}


int aes_decrypt (char *data, int size, char *key, int keysize, int cbc) {
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
    char test_ebc[] = "JLTLxsIDTsZYmcbd-qbqsJnEZUpJxyRLryKYzbKLUwWHbFHe";
    char result_ebc[] = "f414520e82bfa2071369fa74bebf308bcb098883f020bcf93b6648b152b2ee508a869c26f368d7d53ebf05cf06ab13a9"; 
    char test_cbc[] = "JLTLxsIDTsZYmcbd-qbqsJnEZUpJxyRLryKYzbKLUwWHbFHe";
    char result_cbc[] = "f414520e82bfa2071369fa74bebf308bf8519fa9747f7fa5a40493d19d389d73d52d518ebc38a20aaa3cfd9e8a527ad6";
    char key[] = "xnlonrauzwvfqzbpiiewzlblonalhyxf";
    
    printf("test ebc : %s\n", test_ebc);
    aes_encrypt(test_ebc, strlen(test_ebc), key, strlen(key), 0);
    char *output_ecb = asciitohex(test_ebc);
    printf("ebc-ed : %s\n", output_ecb);
    printf("result ok : %d\n", strcmp(output_ecb, result_ebc) == 0);
    free(output_ecb);

    printf("test cbc : %s\n", test_cbc);
    aes_encrypt(test_cbc, strlen(test_cbc), key, strlen(key), 1);
    char *output_cbc = asciitohex(test_cbc);
    printf("cbc-ed : %s\n", output_cbc);
    printf("result ok : %d\n", strcmp(output_cbc, result_cbc) == 0);
    free(output_cbc); 


    return 1;
}
