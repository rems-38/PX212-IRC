#include "tools.h"
#include "const.h"
#include <string.h>


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

// V1 à modifier !!
void shiftRows (byte state[]) {
    byte temp;

	// Row 0 don't change 
    // Shift row 1
    temp = state[1];
    state[1] = state[5];
    state[5] = state[9];
    state[9] = state[13];
    state[13] = temp;

    // Shift row 2
    temp = state[2];
    state[2] = state[10];
    state[10] = temp;
    temp = state[6];
    state[6] = state[14];
    state[14] = temp;

    // Shift row 3
    temp = state[15];
    state[15] = state[11];
    state[11] = state[7];
    state[7] = state[3];
    state[3] = temp;
}

void invShiftRows (byte state[]) {
	byte temp;

	// Row 0 don't change 
	// Shift row 1
	temp = state[13];
	state[13] = state[9];
	state[9] = state[5];
	state[5] = state[1];
	state[1] = temp;

	// Shift row 2
	temp = state[2];
	state[2] = state[10];
	state[10] = temp;
	temp = state[6];
	state[6] = state[14];
	state[14] = temp;

	// Shift row 3
	temp = state[3];
	state[3] = state[7];
	state[7] = state[11];
	state[11] = state[15];
	state[15] = temp;
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

void keyExpansion (const byte key[], byte w[], int nk, int nr) {
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