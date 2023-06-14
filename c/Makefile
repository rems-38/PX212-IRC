FLAGS = -Wall -Wextra -g -fsanitize=address -fsanitize=undefined -fsanitize=shift -fsanitize=shift-exponent -fsanitize=shift-base -fsanitize=integer-divide-by-zero -fsanitize=unreachable -fsanitize=vla-bound -fsanitize=null -fsanitize=return -fsanitize=signed-integer-overflow -fsanitize=bounds -fsanitize=bounds-strict -fsanitize=bool -fsanitize=enum
LINK = -lasan -lubsan

OPTMIZE ?= no
CALLGRIND ?= no

ifeq ($(OPTMIZE), yes)
	FLAGS += -O3
endif

ifeq ($(CALLGRIND), yes)
	FLAGS = -g
	LINK =
endif

aes: aes.o cipher.o tools.o
	gcc $(FLAGS) $^ -o $@

callgrind: aes.o cipher.o tools.o
	gcc $(FLAGS) $^ -o $@
	valgrind --tool=callgrind --callgrind-out-file=callgrind.out --dump-instr=yes --collect-jumps=yes ./callgrind

aes.o: aes.c cipher.h
	gcc $(LINK) -c $^

tests: tests.o cipher.o tools.o
	gcc $(FLAGS) $^ -o $@

tests.o: tests.c cipher.h
	gcc $(LINK) -c $^

cipher.o: cipher.c cipher.h
	gcc $(LINK) -c $^

tools.o: tools.c tools.h
	gcc $(LINK) -c $^

clean:
	rm *.o *.h.gch aes tests callgrind callgrind.out