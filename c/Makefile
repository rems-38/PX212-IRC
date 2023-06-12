FLAGS = -Wall -Wextra -g

main: main.o
	gcc $(FLAGS) $^ -o $@

main.o: main.c main.h
	gcc $(FLAGS) -c $^


clean:
	rm *.o main