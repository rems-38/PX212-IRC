/** @file cipher.h
 *  @brief Function prototypes of the cipher method
 * 
 *  Contient les prototypes pour le cipher
 *
 *  @author Mazzone Rémi (rems-38)
 *  @author Moussu Guillemot (guillemotmoussu)
 *  @bug No known bugs.
 */


/* -- Defines -- */
typedef unsigned char byte;


/* -- Functions -- */
/** @brief Add the key to the state (xor operation)
 *  @param state The current state (16 bytes)
 *  @param w The entire key
 *  @param round The current round (relative to Nr)
 *  @return Void
 */
void addRoundKey(byte state[], byte w[], int round);

/** @brief Substitute the bytes of the state with a box
 *  @param state The current state (16 bytes)
 *  @param box Either the S-Box or the inverse S-Box (256 bytes)
 *  @return Void
 */
void subBytes(byte state[], const byte box[256]);

/** @brief Shift one row of the state
 *  @param state The current state (16 bytes)
 *  @param row The row to shift
 *  @param direction The direction of the shift (1 for right, -1 for left)
 *  @param shift The number of shifts
 *  @return Void
 */
void shiftOneRow(byte state[], int row, int direction, int shift);

/** @brief Shift all the rows of the state
 *  @param state The current state (16 bytes)
 *  @return Void
 */
void shiftRows(byte state[]);

/** @brief Inverse process of shiftRows
 *  @param state The current state (16 bytes)
 *  @return Void
 */
void invShiftRows(byte state[]);

/** @brief Mix the columns of the state
 *  @param state The current state (16 bytes)
 *  @param polyMix Either the "a_x-Mix-Columns" or "a_x-Inverse-Mix-Columns" (16 bytes)
 *  @return Void
 */
void mixColumns(byte state[], const byte polyMix[16]);

/** @brief Equivalent of subBytes for a 4 byte state 
 *  @param state The current word (4 bytes)
 *  @return Void
 */
void subWord(byte state[4]);

/** @brief 1 byte rigth rotation of a 4 byte state
 *  @param state The current word (4 bytes)
 *  @return Void
 */
void rotWord(byte state[4]);

/** @brief Create the rcon polynome associated to the round
 *  @param i The current round
 *  @param out The word generated (4 bytes)
 *  @return Void
 */
void rcon(int i, byte out[4]);

/** @brief Key expansion method
 *  @param key The key (16, 24 or 32 bytes)
 *  @param w The expanded key generated (16*(Nr+1) bytes)
 *  @param nk The number of words in the key (4, 6 or 8 refering to the key size (16, 24 or 32 bytes)))
 *  @param nr The number of rounds
 *  @return Void
 */
void keyExpansion(byte key[], byte w[], int nk, int nr);

/** @brief Cipher method
 *  @param in The input block (16 bytes) enlarged over the rounds
 *  @param w The expanded key (16*(Nr+1) bytes)
 *  @param nr The number of rounds
 *  @return Void
 */
void cipher(byte in[], byte w[], int nr);

/** @brief Inverse cipher method
 *  @param in The input block (16 bytes) enlarged over the rounds
 *  @param w The expanded key (16*(Nr+1) bytes)
 *  @param nr The number of rounds
 *  @return Void
 */
void invCipher(byte in[], byte w[], int nr);