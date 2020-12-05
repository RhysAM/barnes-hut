build:
	stack build

run: build
	stack run

clean:
	stack clean

all: clean run
