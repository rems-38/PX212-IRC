FLAGS = -Wall -Wextra -g

aes: aes.o cipher.o tools.o
	gcc $(FLAGS) $^ -o $@

aes.o: aes.c cipher.h
	gcc $(FLAGS) -c $^

cipher.o: cipher.c cipher.h
	gcc $(FLAGS) -c $^

tools.o: tools.c tools.h
	gcc $(FLAGS) -c $^

clean:
	rm *.o *.h.gch aes