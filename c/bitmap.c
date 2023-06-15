/** @file bitmap.c
 *  @brief BMP encryption and decryption
 *
 *  Contient les fonctions de chiffrement et de déchiffrement pour des fichiers BMP.
 *
 *  @author Mazzone Rémi (rems-38)
 *  @author Moussu Guillemot (guillemotmoussu)
 *  @bug Fichiers illisibles sous Windows uniquement.
 */


/* -- Includes -- */
#include "cipher.h"
#include "tools.h"
#include "aes.h"
#include "entropie.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>


/* -- Functions -- */
/** @brief Create a BMP file
 *  @param filename Filename of the output file
 *  @param info The header of the BMP file
 *  @param data The data of the BMP file
 *  @param size The size of the data
 *  @return Void
 */
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

/** @brief Encrypt a BMP file
 *  @param filename Filename of the input file
 *  @param output_name Filename of the output file
 *  @param cbc Enable the CBC mode (1 enabled and 0 for ECB mode)
 *  @return Void
 */
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

/** @brief Decrypt a BMP file
 *  @param filename Filename of the input file
 *  @param output_name Filename of the output file
 *  @param cbc Enable the CBC mode (1 enabled and 0 for ECB mode)
 *  @return Void
 */
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

int main(int argc, char** argv)
{
    printf("ECB :\n");
    chiffrerBMP(argv[1], "bitmap_files/encrypt_ecb.bmp", 0);
    dechiffrerBMP("bitmap_files/encrypt_ecb.bmp", "bitmap_files/decrypt_ecb.bmp", 0);
    entropie(argv[1]);
    entropie("bitmap_files/encrypt_ecb.bmp");

    printf("\n\nCBC :\n");
    chiffrerBMP(argv[1], "bitmap_files/encrypt_cbc.bmp", 1);
    dechiffrerBMP("bitmap_files/encrypt_cbc.bmp", "bitmap_files/decrypt_cbc.bmp", 1);
    entropie("bitmap_files/encrypt_cbc.bmp");

    return 0;
}
