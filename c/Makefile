FLAGS = -Wall -Wextra -g

aes: aes.o
	gcc $(FLAGS) $^ -o $@

aes.o: aes.c cipher.h
	gcc $(FLAGS) -c $^

cipher.o: cipher.c cipher.h
	gcc $(FLAGS) -c $^


clean:
	rm *.o *.h.gch main