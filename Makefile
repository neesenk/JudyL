CFLAGS = -I. -fPIC -Wall -O0 -pipe -g

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
	JudyLInsertBranch.o	\
	JudyLMallocIF.o		\
	JudyLNext.o		\
	JudyLPrev.o		\
	JudyLPrevEmpty.o	\
	JudyLNextEmpty.o	\
	JudyLByCount.o		\
	JudySL.o		\
	JudyHS.o
HEADFILE := Judy.h JudyL.h JudyPrivate.h JudyPrivateBranch.h

all: libjudy.so test

libjudy.so : $(OBJS) 
	$(CC) $(CFLAGS) -shared $^ -o libjudy.so

main.o: $(HEADFILE)

test : libjudy.so main.o
	$(CC) -O2 -g -Wall main.o -L. -ljudy -o test

$(OBJS) : $(HEADFILE)

JudyLNext.o : JudyLNext.c
	$(CC) $(CFLAGS) -DJUDYNEXT -c -o JudyLNext.o JudyLNext.c
JudyLPrev.o : JudyLNext.c
	$(CC) $(CFLAGS) -DJUDYPREV -c -o JudyLPrev.o JudyLNext.c
JudyLNextEmpty.o : JudyLNextEmpty.c
	$(CC) $(CFLAGS) -DJUDYNEXT -c -o JudyLNextEmpty.o JudyLNextEmpty.c
JudyLPrevEmpty.o : JudyLNextEmpty.c
	$(CC) $(CFLAGS) -DJUDYPREV -c -o JudyLPrevEmpty.o JudyLNextEmpty.c

clean:
	rm -f $(OBJS) libjudy.so test main.o


