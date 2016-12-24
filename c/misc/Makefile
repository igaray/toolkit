.PHONY: all clean

CC_OPTS=-Wall -Werror -pedantic -std=c11
all:
	mkdir -p build
	clang -o build/arraysort arraysort.c
	clang -o build/casts casts.c
	clang -o build/enum enum.c
	clang -o build/fgets fgets.c
	clang -o build/hello hello.c
	clang -o build/overflow overflow.c
	clang -o build/sizeof sizeof.c
	clang -o build/str str.c
	clang -o build/types types.c

clean:
	rm -rf build
