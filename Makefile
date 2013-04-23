.PHONY: all

RM=rm -f

all:
	ghc -o main Main.hs
clean:
	find -name "*.hi" -exec $(RM) '{}' +
	find -name "*.o" -exec $(RM) '{}' +
	$(RM) main
