#include <stdio.h>
#include <stdlib.h>

typedef unsigned char gf256;

const gf256 p1 = 0x57, p2 = 0x83, resAdd = 0xd4, resMulti = 0xc1;

gf256 add (gf256 a, gf256 b) { return a ^ b; }

gf256 multi (gf256 a, gf256 b) {
	gf256 res = 0, carry;

	for (int i = 0; i < 8; i++) {
		if (b & 1) res ^= a;
		carry = a & 0x80;
		a <<= 1;
		if (carry) a ^= 0x1b;
		b >>= 1;
	}

	return res;
}


int main (void){
	printf("%i", resAdd == add(p1, p2));
	printf("%i", resMulti == multi(p1, p2));

	return 1;
}