#include "cipher.h"
#include "tools.h"
#include "aes.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void ecrireBMP(char* filename, unsigned char* info, unsigned char* data, int size) {
    FILE *f = fopen(filename, "wb");
    if (f == NULL) {
        printf("Error opening file\n");
        return;
    }

    fwrite(info, sizeof(unsigned char), 54, f);
    fwrite(data, sizeof(unsigned char), size, f);

    fclose(f);
}

void crypterBMP(char* filename) {
    FILE *f = fopen(filename, "rb");
    if (f == NULL) {
        printf("Error opening file\n");
        return;
    }

    unsigned char info[54];
    fread(info, sizeof(unsigned char), 54, f);

    int size = *(int*)&info[34];

    unsigned char* data = malloc(size);
    
    fread(data, sizeof(unsigned char), size, f);
    
    aes_encrypt(data, size, "0123456789abcdef", 16);

    ecrireBMP("bitmap_files/bitmap_file_encrypted.bmp", info, data, size);
   
    fclose(f);
    free(data);
}

int main(void)
{
    crypterBMP("bitmap_files/bitmap_original.bmp");
    return 0;
}
