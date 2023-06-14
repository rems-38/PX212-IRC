/** @file tools.h
 *  @brief Functions prototypes of the tools.c file
 * 
 *  Contient les prototypes des fonctions de tools.c
 *
 *  @author Mazzone Rémi (rems-38)
 *  @author Moussu Guillemot (guillemotmoussu)
 *  @bug No known bugs.
 */


/* -- Defines -- */
typedef unsigned char byte;


/* -- Functions -- */
/**
 * @brief XOR operation between two byte arrays
 * @param a First byte array
 * @param b Second byte array
 * @param length Length of the arrays
 * @return Void
 */
void byteXor(byte a[], const byte b[], int length);

/**
 * @brief Multiplication in GF(2^8) for two bytes
 * @param a First byte
 * @param b Second byte
 * @return The result of the multiplication
 */
byte multi(byte a, byte b);

/**
 * @brief Print a byte array
 * @param in The byte array to print
 * @param length The length of the array
 * @return Void
 */
void printByte(byte in[], int length);

/**
 * @brief Switch the columns and the rows of a 4x4 matrix
 * @param state The matrix to switch
 * @return Void
 */
void switchColRows(byte state[]);

/**
 * @brief Split an array into another one
 * @param in The array to split
 * @param out The array to fill
 * @param start The start index (for the "in" array)
 * @param end The end index (for the "in" array)
 * @return Void
 */
void splitArr(const byte in[], byte out[], int start, int end);

/**
 * @brief Merge an array into another one (use for append an array)
 * @param in The array to merge
 * @param out The array to fill
 * @param start The start index (for the "out" array)
 * @param end The end index (for the "out" array)
 * @return Void
 */
void mergeArr(const byte in[], byte out[], int start, int end);