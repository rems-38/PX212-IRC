/** @file aes.h
 *  @brief Function prototypes of the aes method
 * 
 *  Contient les prototypes pour le protocole AES
 *
 *  @author Mazzone Rémi (rems-38)
 *  @author Moussu Guillemot (guillemotmoussu)
 *  @bug No known bugs.
 */


/* -- Defines -- */
typedef unsigned char byte;


/* -- Functions -- */
/** @brief Process of keyExpansion
 * 
 *  Alloue la mémoire pour la clé étendue
 *  Calcule les valeurs de Nr et Nk
 *  Rempli la clé étendue
 * 
 *  @param key The initial key (16, 24, 32 bytes)
 *  @param keysize The size of the key (16, 24, 32 bytes)
 *  @param nr The number of rounds (10, 12, 14) (output variable)
 *  @return The extended key
 */
byte *keyprocess(char *key, int keysize, int *nr);

/** @brief Convert a hexadecimal string to an ascii string 
 *  @param in The hexadecimal string
 *  @return The ascii string
 */
char* hextoascii(const char* in);

/** @brief Convert an ascii string to a hexadecimal string
 *  @param in The ascii string
 *  @return The hexadecimal string
 */
char* asciitohex(const char* in);

/** @brief Encrypt data with AES
 *  @param data The data to encrypt
 *  @param size The size of the data (multiple of 16 bytes)
 *  @param key The key to encrypt the data
 *  @param keysize The size of the key (16, 24, 32 bytes)
 *  @return 0 if success, 1 if error
 */
int aes_encrypt (char *data, int size, char *key, int keysize);

/** @brief Decrypt data with AES
 *  @param data The data to decrypt
 *  @param size The size of the data (multiple of 16 bytes)
 *  @param key The key to decrypt the data
 *  @param keysize The size of the key (16, 24, 32 bytes)
 *  @return 0 if success, 1 if error
 */
int aes_decrypt (char *data, int size, char *key, int keysize);