#include <stdio.h>
#include <stdlib.h>

typedef unsigned char byte;
typedef unsigned int word;

#define Nb 4
#define Nr 10

byte add (byte a, byte b) { return a ^ b; }

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

void addRoundKey (byte state[4 * Nb], word w [Nb * (Nr + 1)], int round) {
	for (int i = 0; i < 4 * Nb; i++) {
		state[i] ^= w[(round * Nb) + i];
	}
}

void cipher (byte in[4 * Nb], byte out[4 * Nb], word w[Nb * (Nr + 1)]) {
	byte state[4 * Nb];

	// Initialize state
    for (int i = 0; i < 4 * Nb; i++) {
        state[i] = in[i];
    }

	addRoundKey(state, w, 0);

	for (int round = 0; round < Nr; round++) {
		// subBytes(state);
		// shiftRows(state);
		// mixColumns(state);
		addRoundKey(state, w, round);
	}

	// subBytes(state);
	// shiftRows(state);
	addRoundKey(state, w, Nr);

	// Store the result in 'out'
    for (int i = 0; i < 4 * Nb; i++) {
    	out[i] = state[i];
	}
}


int main (void){
	byte in[4 * Nb] = {0x32, 0x43, 0xf6, 0xa8, 0x88, 0x5a, 0x30, 0x8d, 0x31, 0x31, 0x98, 0xa2, 0xe0, 0x37, 0x07, 0x34};
	word key[Nb * (Nr + 1)] = {0x2b, 0x7e, 0x15, 0x16, 0x28 ,0xae, 0xd2, 0xa6, 0xab, 0xf7, 0x15, 0x88 ,0x09, 0xcf, 0x4f, 0x3c};

	for (int i = 0; i < 4 * Nb; i++) {
		printf("%x ", in[i]);
	}
	printf("\n");

	addRoundKey(in, key, 0);
	
	for (int i = 0; i < 4 * Nb; i++) {
		printf("%x ", in[i]);
	}
	printf("\n");

	return 1;
}