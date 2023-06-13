#include "cipher.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>


int aes_encrypt (char *data, int size, char *key, int keysize) {
    if (size % 16 != 0) { return 1; }
    if (keysize != 16 || keysize != 24 || keysize != 32) { return 1; }

    

    for (int i = 0; i < size % 16; i++) {


    }

}


int aes_decrypt (char *data, int size, char *key, int keysize) {

}


int main (void) {
    printf("%d\n", aes_encrypt("coucou", 17, "abc", 3));



    return 1;
}