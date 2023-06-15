CC = gcc
FLAGS = -Wall -Wextra -g -fsanitize=address -fsanitize=undefined -fsanitize=shift -fsanitize=shift-exponent -fsanitize=shift-base -fsanitize=integer-divide-by-zero -fsanitize=unreachable -fsanitize=vla-bound -fsanitize=null -fsanitize=return -fsanitize=signed-integer-overflow -fsanitize=bounds -fsanitize=bounds-strict -fsanitize=bool -fsanitize=enum
LINK = -lasan -lubsan

COMPIL ?= gcc
OPTMIZE ?= yes
CALLGRIND ?= no

ifeq ($(COMPIL), clang)
	CC = clang
	CALLGRIND = yes
endif

ifeq ($(CALLGRIND), yes)
	FLAGS = -g
	LINK =
endif

ifeq ($(OPTMIZE), yes)
	FLAGS += -O3 -march=native -flto
	LINK += -flto
endif

aes: aes.o cipher.o tools.o
	$(CC) $(FLAGS) $^ -o $@

callgrind: bitmap.o cipher.o tools.o aes.o
	$(CC) $(FLAGS) $^ -o $@
	valgrind --tool=callgrind --callgrind-out-file=callgrind.out --dump-instr=yes --collect-jumps=yes ./callgrind

aes.o: aes.c cipher.h
	$(CC) $(LINK) -c $^

tests: tests.o cipher.o tools.o aes.o
	$(CC) $(FLAGS) $^ -o $@

tests.o: tests.c cipher.h aes.h
	$(CC) $(LINK) -c $^

bitmap: bitmap.o cipher.o tools.o aes.o
	$(CC) $(FLAGS) $^ -o $@

bitmap.o: bitmap.c cipher.h aes.h
	$(CC) $(LINK) -c $^

entropie: entropie.o
	$(CC) $(FLAGS) $^ -o $@ -lm

entropie.o: entropie.c
	$(CC) $(LINK) -c $^
 
cipher.o: cipher.c cipher.h
	$(CC) $(LINK) -c $^
 
tools.o: tools.c tools.h
	$(CC) $(LINK) -c $^

clean:
	rm *.o *.h.gch aes tests callgrind callgrind.out bitmap