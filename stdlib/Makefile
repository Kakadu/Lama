SHELL := /bin/bash

FILES=$(wildcard *.lama)
ALL=$(sort $(FILES:.lama=.o))
ifneq ("$(wildcard ../src/Driver.exe)","")
    #$(info $(shell ls ../src))
    #$(info $(shell ls ../runtime))
    LAMAC=../src/Driver.exe -g
else
    #$(info There is driver.exe)
    LAMAC=../src/lamac -g
endif


all: $(ALL)

Fun.o: Ref.o

Data.o: Ref.o Collection.o

Collection.o: List.o Ref.o

Array.o: List.o

Ostap.o: List.o Collection.o Ref.o Fun.o Matcher.o

Buffer.o: List.o

STM.o: List.o Fun.o

%.o: %.lama
	LAMA=../runtime $(LAMAC) -I . -c $<

clean:
	rm -Rf *.s *.o *.i *~
	pushd regression && make clean && popd

