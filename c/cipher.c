/** @file cipher.c
 *  @brief Cipher method 
 * 
 *  Contient tous les fonctions nécessaires au chiffrement (et au déchiffrement)
 *  d'un bloc de 16 octets avec une clé.
 *
 *  @author Mazzone Rémi (rems-38)
 *  @author Moussu Guillemot (guillemotmoussu)
 *  @bug No known bugs.
 */


/* -- Includes -- */
#include "tools.h"
#include "const.h"
#include <string.h>


/* -- Functions -- */
void addRoundKey (byte state[], byte w[], int round) {
	for (int i = 0; i < 16; i++) {
		state[i] ^= w[round + i];
	}
}

void subBytes (byte state[], const byte box[256]) {
	for (int i = 0; i < 16; i++) {
		state[i] = box[state[i]];
	}
}

void shiftOneRow(byte state[], int row, int direction, int shift) {
    byte temp;
    if (direction > 0) {  // décalage à droite
        for (int i = 3; i >= shift; i--) {
            temp = state[i*4+row];
            state[i*4+row] = state[(i-shift)*4+row];
            state[(i-shift)*4+row] = temp;
        }
    } else {  // décalage à gauche
        for (int i = 0; i < 4 - shift; i++) {
            temp = state[i*4+row];
            state[i*4+row] = state[(i+shift)*4+row];
            state[(i+shift)*4+row] = temp;
        }
    }
}

void shiftRows(byte state[]) {
    // On ne change pas la ligne 0
    // On décale la ligne 1 à gauche de 1
    shiftOneRow(state, 1, -1, 1);
    // On décale la ligne 2 à gauche de 2
    shiftOneRow(state, 2, -1, 2);
    // On décale la ligne 3 à droite de 1
    shiftOneRow(state, 3, 1, 1);
}

void invShiftRows(byte state[]) {
    // On ne change pas la ligne 0
    // On décale la ligne 1 à droite de 1
    shiftOneRow(state, 1, 1, 1);
    // On décale la ligne 2 à droite de 2
    shiftOneRow(state, 2, 1, 2);
    // On décale la ligne 3 à gauche de 1
    shiftOneRow(state, 3, -1, 1);
}

void mixColumns (byte state[], const byte polyMix[16]) {
	byte temp[16];

	for (int i = 0; i < 16; i++) {
		temp[i] = 0;
		for (int j = 0; j < 4; j++) {
			temp[i] ^= multi(polyMix[(i * 4 + j) % 16], state[j + ((i / 4) * 4)]);
		}
	}

	memcpy(state, temp, 16);
}

void subWord (byte state[4]) { 
	for (int i = 0; i < 4; i++) {
		state[i] = sbox[state[i]];
	}
}

void rotWord (byte state[4]) {
	byte temp[4];
	
	for (int i = 1; i < 4; i++) { temp[i - 1] = state[i]; }
	temp[3] = state[0];

	memcpy(state, temp, 4);
}

void rcon(int i, byte out[4]) {
    byte rcon[] = {0x01, 0x00, 0x00, 0x00};

	if (i != 1) {
		for (int j = 1; j < i; j++) {
			byte b = (rcon[0] & 0x80) ? 0x1b : 0x00;
			rcon[0] = (rcon[0] << 1) ^ b;
		}
	}

	memcpy(out, rcon, 4);
}

void keyExpansion (byte key[], byte w[], int nk, int nr) {
	byte ending[4] = {0};
	byte starting[4] = {0};
	byte pRcon[4] = {0};
	memcpy(w, key, 4*nk);

	for (int i = nk; i < 4*(nr+1); i++) {
		splitArr(w, ending, i*4-4, i*4);

		if (i % nk == 0) {
			rotWord(ending);
			subWord(ending);
			rcon(i/nk, pRcon);
			byteXor(ending, pRcon, 4);
		}
		else if (nk > 6 && i % nk == 4) { subWord(ending); }

		splitArr(w, starting, (i-nk)*4, (i-nk)*4+4);
		byteXor(starting, ending, 4);

		mergeArr(starting, w, i*4, i*4+4);
	}
}


void cipher (byte in[], byte w[], int nr) {
	addRoundKey(in, w, 0);
	for (int round = 1; round < nr; round++) {
		subBytes(in, sbox);
		shiftRows(in);
		mixColumns(in, a_x_mixColumns);
		addRoundKey(in, w, round*16);
	}

	subBytes(in, sbox);
	shiftRows(in);
	addRoundKey(in, w, nr*16);
}

void invCipher (byte in[], byte w[], int nr) {
	addRoundKey(in, w, nr*16);
	for (int round = nr-1; round > 0; round--) {
		invShiftRows(in);
		subBytes(in, invSbox);
		addRoundKey(in, w, round*16);
		mixColumns(in, a_x_invMixColumns);
	}

	invShiftRows(in);
	subBytes(in, invSbox);
	addRoundKey(in, w, 0);
}