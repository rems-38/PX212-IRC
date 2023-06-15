/** @file entropie.c
 *  @brief Entropie algorithm 
 * 
 *  Calcule à quel point c\'est le "chaos" dans le fichier (grande répartition des octets)
 *
 *  @author Mazzone Rémi (rems-38)
 *  @author Moussu Guillemot (guillemotmoussu)
 *  @bug No known bugs.
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

typedef unsigned char byte;


void entropie(char *filename) {
    double proba[256] = {0};

    FILE *f = fopen(filename, "r");
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


    for (int j = 0; j < size; j++) {
        proba[data[j]]++;   
    }

    for (int i = 0; i < 256; i++) {
        proba[i] = (double)proba[i]/size * log2(1/((double)proba[i]/size));
    }

    double sum = 0;
    for (int i = 0; i < 256; i++) {
        sum += proba[i];
    }

    printf("Entropie (%s): %f\n", filename, sum);

    fclose(f);
    free(data);
}
