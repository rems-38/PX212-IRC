/** @file bitmap.c
 *  @brief BMP encryption and decryption
 *
 *  Contient les fonctions de chiffrement et de déchiffrement pour des fichiers BMP.
 *
 *  @author Mazzone Rémi (rems-38)
 *  @author Moussu Guillemot (guillemotmoussu)
 *  @bug Fichiers illisibles sous Windows uniquement.
 */

#include "cipher.h"
#include "tools.h"
#include "aes.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

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

void chiffrerBMP(char* filename, char *output_name, int cbc) {
    FILE *f = fopen(filename, "rb");
    if (f == NULL) {
        printf("Error opening file\n");
        return;
    }

    unsigned char info[54];
    fread(info, sizeof(unsigned char), 54, f);

    int size = *(int*)&info[34];

    unsigned char* data = malloc(size);
    if (data == NULL) {
        printf("Error allocating memory\n");
        return;
    }
    
    fread(data, sizeof(unsigned char), size, f);
    
    clock_t start = clock();
    aes_encrypt((char*)data, size, "00112233445566778899aabbccddeeff", 32, cbc);
    double elapsed_time = (double)(clock() - start) / CLOCKS_PER_SEC;
    printf("Temps de chiffrement : %fs\nVitesse : %fMo/s\n", elapsed_time, (size * 0.0000009537) / elapsed_time);

    ecrireBMP(output_name, info, data, size);
   
    fclose(f);
    free(data);
}

void dechiffrerBMP(char* filename, char *output_name, int cbc) {
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
    
    clock_t start = clock();
    aes_decrypt((char*)data, size, "00112233445566778899aabbccddeeff", 32, cbc);
    double elapsed_time = (double)(clock() - start) / CLOCKS_PER_SEC;
    printf("Temps de déchiffrement : %fs\nVitesse : %fMo/s\n", elapsed_time, (size * 0.0000009537) / elapsed_time);

    ecrireBMP(output_name, info, data, size);
   
    fclose(f);
    free(data);
}

int main(void)
{
    printf("ECB :\n");
    chiffrerBMP("bitmap_files/bitmap_file_2.bmp", "bitmap_files/encrypt_ecb.bmp", 0);
    dechiffrerBMP("bitmap_files/encrypt_ecb.bmp", "bitmap_files/decrypt_ecb.bmp", 0);
    
    printf("\n\nCBC :\n");
    chiffrerBMP("bitmap_files/bitmap_file_2.bmp", "bitmap_files/encrypt_cbc.bmp", 1);
    dechiffrerBMP("bitmap_files/encrypt_cbc.bmp", "bitmap_files/decrypt_cbc.bmp", 1);
    
    return 0;
}
