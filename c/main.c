#include "main.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

const byte sbox[256] = {0x63, 0x7c, 0x77, 0x7b, 0xf2, 0x6b, 0x6f, 0xc5, 0x30, 0x01, 0x67, 0x2b, 0xfe, 0xd7, 0xab, 0x76, 0xca, 0x82, 0xc9, 0x7d, 0xfa, 0x59, 0x47, 0xf0, 0xad, 0xd4, 0xa2, 0xaf, 0x9c, 0xa4, 0x72, 0xc0, 0xb7, 0xfd, 0x93, 0x26, 0x36, 0x3f, 0xf7, 0xcc, 0x34, 0xa5, 0xe5, 0xf1, 0x71, 0xd8, 0x31, 0x15, 0x04, 0xc7, 0x23, 0xc3, 0x18, 0x96, 0x05, 0x9a, 0x07, 0x12, 0x80, 0xe2, 0xeb, 0x27, 0xb2, 0x75, 0x09, 0x83, 0x2c, 0x1a, 0x1b, 0x6e, 0x5a, 0xa0, 0x52, 0x3b, 0xd6, 0xb3, 0x29, 0xe3, 0x2f, 0x84, 0x53, 0xd1, 0x00, 0xed, 0x20, 0xfc, 0xb1, 0x5b, 0x6a, 0xcb, 0xbe, 0x39, 0x4a, 0x4c, 0x58, 0xcf, 0xd0, 0xef, 0xaa, 0xfb, 0x43, 0x4d, 0x33, 0x85, 0x45, 0xf9, 0x02, 0x7f, 0x50, 0x3c, 0x9f, 0xa8, 0x51, 0xa3, 0x40, 0x8f, 0x92, 0x9d, 0x38, 0xf5, 0xbc, 0xb6, 0xda, 0x21, 0x10, 0xff, 0xf3, 0xd2, 0xcd, 0x0c, 0x13, 0xec, 0x5f, 0x97, 0x44, 0x17, 0xc4, 0xa7, 0x7e, 0x3d, 0x64, 0x5d, 0x19, 0x73, 0x60, 0x81, 0x4f, 0xdc, 0x22, 0x2a, 0x90, 0x88, 0x46, 0xee, 0xb8, 0x14, 0xde, 0x5e, 0x0b, 0xdb, 0xe0, 0x32, 0x3a, 0x0a, 0x49, 0x06, 0x24, 0x5c, 0xc2, 0xd3, 0xac, 0x62, 0x91, 0x95, 0xe4, 0x79, 0xe7, 0xc8, 0x37, 0x6d, 0x8d, 0xd5, 0x4e, 0xa9, 0x6c, 0x56, 0xf4, 0xea, 0x65, 0x7a, 0xae, 0x08, 0xba, 0x78, 0x25, 0x2e, 0x1c, 0xa6, 0xb4, 0xc6, 0xe8, 0xdd, 0x74, 0x1f, 0x4b, 0xbd, 0x8b, 0x8a, 0x70, 0x3e, 0xb5, 0x66, 0x48, 0x03, 0xf6, 0x0e, 0x61, 0x35, 0x57, 0xb9, 0x86, 0xc1, 0x1d, 0x9e, 0xe1, 0xf8, 0x98, 0x11, 0x69, 0xd9, 0x8e, 0x94, 0x9b, 0x1e, 0x87, 0xe9, 0xce, 0x55, 0x28, 0xdf, 0x8c, 0xa1, 0x89, 0x0d, 0xbf, 0xe6, 0x42, 0x68, 0x41, 0x99, 0x2d, 0x0f, 0xb0, 0x54, 0xbb, 0x16};
const byte invSbox[256] = {0x52, 0x09, 0x6a, 0xd5, 0x30, 0x36, 0xa5, 0x38, 0xbf, 0x40, 0xa3, 0x9e, 0x81, 0xf3, 0xd7, 0xfb, 0x7c, 0xe3, 0x39, 0x82, 0x9b, 0x2f, 0xff, 0x87, 0x34, 0x8e, 0x43, 0x44, 0xc4, 0xde, 0xe9, 0xcb, 0x54, 0x7b, 0x94, 0x32, 0xa6, 0xc2, 0x23, 0x3d, 0xee, 0x4c, 0x95, 0x0b, 0x42, 0xfa, 0xc3, 0x4e, 0x08, 0x2e, 0xa1, 0x66, 0x28, 0xd9, 0x24, 0xb2, 0x76, 0x5b, 0xa2, 0x49, 0x6d, 0x8b, 0xd1, 0x25, 0x72, 0xf8, 0xf6, 0x64, 0x86, 0x68, 0x98, 0x16, 0xd4, 0xa4, 0x5c, 0xcc, 0x5d, 0x65, 0xb6, 0x92, 0x6c, 0x70, 0x48, 0x50, 0xfd, 0xed, 0xb9, 0xda, 0x5e, 0x15, 0x46, 0x57, 0xa7, 0x8d, 0x9d, 0x84, 0x90, 0xd8, 0xab, 0x00, 0x8c, 0xbc, 0xd3, 0x0a, 0xf7, 0xe4, 0x58, 0x05, 0xb8, 0xb3, 0x45, 0x06, 0xd0, 0x2c, 0x1e, 0x8f, 0xca, 0x3f, 0x0f, 0x02, 0xc1, 0xaf, 0xbd, 0x03, 0x01, 0x13, 0x8a, 0x6b, 0x3a, 0x91, 0x11, 0x41, 0x4f, 0x67, 0xdc, 0xea, 0x97, 0xf2, 0xcf, 0xce, 0xf0, 0xb4, 0xe6, 0x73, 0x96, 0xac, 0x74, 0x22, 0xe7, 0xad, 0x35, 0x85, 0xe2, 0xf9, 0x37, 0xe8, 0x1c, 0x75, 0xdf, 0x6e, 0x47, 0xf1, 0x1a, 0x71, 0x1d, 0x29, 0xc5, 0x89, 0x6f, 0xb7, 0x62, 0x0e, 0xaa, 0x18, 0xbe, 0x1b, 0xfc, 0x56, 0x3e, 0x4b, 0xc6, 0xd2, 0x79, 0x20, 0x9a, 0xdb, 0xc0, 0xfe, 0x78, 0xcd, 0x5a, 0xf4, 0x1f, 0xdd, 0xa8, 0x33, 0x88, 0x07, 0xc7, 0x31, 0xb1, 0x12, 0x10, 0x59, 0x27, 0x80, 0xec, 0x5f, 0x60, 0x51, 0x7f, 0xa9, 0x19, 0xb5, 0x4a, 0x0d, 0x2d, 0xe5, 0x7a, 0x9f, 0x93, 0xc9, 0x9c, 0xef, 0xa0, 0xe0, 0x3b, 0x4d, 0xae, 0x2a, 0xf5, 0xb0, 0xc8, 0xeb, 0xbb, 0x3c, 0x83, 0x53, 0x99, 0x61, 0x17, 0x2b, 0x04, 0x7e, 0xba, 0x77, 0xd6, 0x26, 0xe1, 0x69, 0x14, 0x63, 0x55, 0x21, 0x0c, 0x7d};

void byteXor(byte a[], const byte b[], int length) {
	for (int i = 0; i < length; i++) {
		a[i] ^= b[i];
	}
}

byte multi (byte a, byte b) {
	byte res = 0, carry;

	for (int i = 0; i < 8; i++) {
		if (b & 1) res ^= a;
		carry = a & 0x80;
		a <<= 1;
		if (carry) a ^= 0x1b;
		b >>= 1;
	}

	return res;
}

void addRoundKey (byte state[], byte w[], int round) {
	for (int i = 0; i < 16; i++) {
		state[i] ^= w[round + i];
	}
}

void subBytes (byte state[]) {
	for (int i = 0; i < 16; i++) {
		state[i] = sbox[state[i]];
	}
}

void invSubBytes (byte state[]) {
	for (int i = 0; i < 16; i++) {
		state[i] = invSbox[state[i]];
	}
}

void switchColRows(byte state[]) {
	byte temp[16];
	memcpy(temp, state, 16);

	for (int i = 0; i < 16; i++) {
		state[i] = temp[(i % 4) * 4 + (i / 4)];
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

void mixColumns (byte state[]) {
	byte temp[16];
	byte a_x_mixColumns[16] = {0x02, 0x03, 0x01, 0x01, 0x01, 0x02, 0x03, 0x01, 0x01, 0x01, 0x02, 0x03, 0x03, 0x01, 0x01, 0x02};

	for (int i = 0; i < 16; i++) {
		temp[i] = 0;
		for (int j = 0; j < 4; j++) {
			temp[i] ^= multi(a_x_mixColumns[(i * 4 + j) % 16], state[j + ((i / 4) * 4)]);
		}
	}

	memcpy(state, temp, 16);
}

void invMixColumns (byte state[]) {
	byte temp[16];
	byte a_x_invMixColumns[16] = {0x0e, 0x0b, 0x0d, 0x09, 0x09, 0x0e, 0x0b, 0x0d, 0x0d, 0x09, 0x0e, 0x0b, 0x0b, 0x0d, 0x09, 0x0e};

	for (int i = 0; i < 16; i++) {
		temp[i] = 0;
		for (int j = 0; j < 4; j++) {
			temp[i] ^= multi(a_x_invMixColumns[(i * 4 + j) % 16], state[j + ((i / 4) * 4)]);
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

void splitArr(const byte in[], byte out[], int start, int end) {
	int j = 0;
	for (int i = start; i < end; i++) {
		out[j] = in[i];
		j++;
	}
}
void mergeArr(const byte in[], byte out[], int start, int end) {
	int j = 0;
	for (int i = start; i < end; i++) {
		out[i] = in[j];
		j++;
	}
}

void keyExpansion (const byte key[], byte w[], int nk, int nr) {
	byte ending[4] = {0};
	byte starting[4] = {0};
	byte pRcon[4] = {0};
	memcpy(w, key, 4*nk);

	for (int i = nk; i < 4*(nr+1); i++) { // Nb * (Nr + 1)
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
		subBytes(in);
		shiftRows(in);
		mixColumns(in);
		addRoundKey(in, w, round*16);
	}

	subBytes(in);
	shiftRows(in);
	addRoundKey(in, w, nr*16);
}

void invCipher (byte in[], byte w[], int nr) {
	addRoundKey(in, w, nr*16);
	for (int round = nr-1; round > 0; round--) {
		invShiftRows(in);
		invSubBytes(in);
		addRoundKey(in, w, round*16);
		invMixColumns(in);
	}

	invShiftRows(in);
	invSubBytes(in);
	addRoundKey(in, w, 0);
}

void printByte (byte in[], int length) {
	for (int i = 0; i < length; i++) { printf("%x ", in[i]); }
	printf("\n");
}


int main (void){
	byte in1[16] = {0x00, 0x11, 0x22, 0x33 ,0x44, 0x55, 0x66, 0x77 ,0x88, 0x99, 0xaa, 0xbb ,0xcc, 0xdd, 0xee, 0xff};
	byte in2[16] = {0x00, 0x11, 0x22, 0x33 ,0x44, 0x55, 0x66, 0x77 ,0x88, 0x99, 0xaa, 0xbb ,0xcc, 0xdd, 0xee, 0xff};
	byte in3[16] = {0x00, 0x11, 0x22, 0x33 ,0x44, 0x55, 0x66, 0x77 ,0x88, 0x99, 0xaa, 0xbb ,0xcc, 0xdd, 0xee, 0xff};

	byte key128[16] = {0x00, 0x01, 0x02, 0x03 ,0x04, 0x05, 0x06, 0x07 ,0x08, 0x09, 0x0a, 0x0b ,0x0c, 0x0d, 0x0e, 0x0f};
	byte key192[24] = {0x00, 0x01, 0x02, 0x03 ,0x04, 0x05, 0x06, 0x07 ,0x08, 0x09, 0x0a, 0x0b ,0x0c, 0x0d, 0x0e, 0x0f, 0x10, 0x11, 
					0x12, 0x13 ,0x14, 0x15, 0x16, 0x17};
	byte key256[32] = {0x00, 0x01, 0x02, 0x03 ,0x04, 0x05, 0x06, 0x07 ,0x08, 0x09, 0x0a, 0x0b ,0x0c, 0x0d, 0x0e, 0x0f, 0x10, 0x11, 
					0x12, 0x13 ,0x14, 0x15, 0x16, 0x17, 0x18, 0x19 ,0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f};
	
	byte w1[176] = {0};
	byte w2[208] = {0};
	byte w3[240] = {0};
	
	keyExpansion(key128, w1, 4, 10);
	keyExpansion(key192, w2, 6, 12);
	keyExpansion(key256, w3, 8, 14);

	printByte(in1, 16);
	cipher(in1, w1, 10);
	printByte(in1, 16);
	invCipher(in1, w1, 10);
	printByte(in1, 16);

	cipher(in2, w2, 12);
	printByte(in2, 16);
	invCipher(in2, w2, 12);
	printByte(in2, 16);

	cipher(in3, w3, 14);
	printByte(in3, 16);
	invCipher(in3, w3, 14);
	printByte(in3, 16);


	return 1;
}