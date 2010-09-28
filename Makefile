CFLAGS = -I. -fPIC -Wall -O2 -pipe -g

OBJS := JudyLCascade.o 		\
	JudyLTables.o 		\
	JudyLCount.o		\
	JudyLCreateBranch.o 	\
	JudyLDecascade.o 	\
	JudyLDel.o		\
	JudyLFreeArray.o	\
	JudyLGet.o		\
	JudyLInsArray.o		\
	JudyLIns.o		\
	JudyLMallocIF.o		\
	JudyLNext.o		\
	JudyLPrev.o		\
	JudyLByCount.o		\
	JudyLWalk.o		\
	JudySL.o		\
	JudyHtb.o		\
	JudyHS.o

HEADFILE := Judy.h JudyL.h JudyPrivate.h JudyPrivateBranch.h

all: libjudy.so test

libjudy.so : $(OBJS)
	$(CC) $(CFLAGS) -shared $^ -o libjudy.so

main.o: $(HEADFILE)

test : $(OBJS) main.o
	$(CC) -O2 -g -Wall $(OBJS) main.o -o test

$(OBJS) : $(HEADFILE)

JudyLNext.o : JudyLIter.c
	$(CC) $(CFLAGS) -DJUDYNEXT -c -o JudyLNext.o JudyLIter.c

JudyLPrev.o : JudyLIter.c
	$(CC) $(CFLAGS) -DJUDYPREV -c -o JudyLPrev.o JudyLIter.c

clean:
	rm -f $(OBJS) libjudy.so test main.o


