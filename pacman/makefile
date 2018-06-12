CC = gcc
DEPS = pacman.h
OBJ = pacman.o
CFLAGS = -lcurses

%.o: %.c $(DEPS)
	$(CC) -c -o $@ $< $(CFLAGS)

pacman: $(OBJ)
	$(CC) -o $@ $^ $(CFLAGS)


.PHONY: clean
clean:
	rm -f *.o pacman
