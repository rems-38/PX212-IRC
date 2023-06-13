#include <string.h>
#include <stdio.h>

typedef unsigned char byte;


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

void printByte (byte in[], int length) {
	for (int i = 0; i < length; i++) { printf("%x ", in[i]); }
	printf("\n");
}

void switchColRows(byte state[]) {
	byte temp[16];
	memcpy(temp, state, 16);

	for (int i = 0; i < 16; i++) {
		state[i] = temp[(i % 4) * 4 + (i / 4)];
	}
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