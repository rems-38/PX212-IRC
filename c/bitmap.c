#include "cipher.h"
#include "tools.h"
#include "aes.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void ecrireBMP(char* filename, unsigned char* info, unsigned char* data, int size) {
    FILE *f = fopen(filename, "w");
    if (f == NULL) {
        printf("Error opening file\n");
        return;
    }

    fwrite(info, sizeof(unsigned char), 54, f);
    fwrite(data, sizeof(unsigned char), size, f);

    fclose(f);
}

void crypterBMP(char* filename) {
    FILE *f = fopen(filename, "r");
    if (f == NULL) {
        printf("Error opening file\n");
        return;
    }

    unsigned char info[54];
    fread(info, sizeof(unsigned char), 54, f);

    int width = *(int*)&info[18];
    int height = *(int*)&info[22];
    int size = 3 * width * height;

    unsigned char* data = malloc(size);
    
    fread(data, sizeof(unsigned char), size, f);
    printf("Size: %d & %d\n", size, size % 16);
    
    ecrireBMP("bitmap_files/bitmap_file_encrypted.bmp", info, data, size);
    
    aes_encrypt((char*)data, size, "00112233445566778899aabbccddeeff", 32);
    

    fclose(f);
    free(data);
}

int main(void)
{
    crypterBMP("bitmap_files/bitmap_original.bmp");

    return 0;
}
