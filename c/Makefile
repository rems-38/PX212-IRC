FLAGS = -Wall -Wextra

main: main.o
	gcc $(FLAGS) $^ -o $@

main.o: main.c
	gcc $(FLAGS) -c $^


clean:
	rm *.o main