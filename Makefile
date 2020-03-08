##
## EPITECH PROJECT, 2018
## KOAK
## File description:
## Makefile
##

BIN = koak
RESOLVER = lts-12.18

all:
	stack build
	cp .stack-work/install/x86_64-linux-tinfo6/$(RESOLVER)/8.4.4/bin/KOAK-exe $(BIN)

clean:
	stack clean
	rm -f $(BIN)

fclean: clean

test:
	stack test

re: test fclean all 

.PHONY: test fclean all
