#include "JudyL.h"

static void *JudyMalloc(size_t Words)
{
	return calloc(Words, sizeof(Word_t));
}

static void JudyFree(void *PWord, size_t Words)
{
	(void)Words;
	free(PWord);
}

static const Word_t juMaxWords = ~0UL;

#define MALLOC(Ptr, Words, Pjpm)	do {			\
	if ((Ptr = JudyMalloc(Words)) != NULL)			\
		Pjpm->jpm_TotalMemWords += Words;		\
	else 							\
		JL_SET_ERRNO(JLE_NOMEM);			\
} while (0)

Pjpm_t judyLAllocJPM(void)
{
	Word_t Words = (sizeof(jLpm_t) + cJL_BYTESPERPTR - 1) / cJL_BYTESPERPTR;
	Pjpm_t Pjpm = JudyMalloc(Words);
	assert((Words * cJL_BYTESPERPTR) == sizeof(jLpm_t));

	if (Pjpm)
		Pjpm->jpm_TotalMemWords = Words;
	return Pjpm;
}

void judyLFreeJPM(Pjpm_t PjpmFree, Pjpm_t PjpmStats)
{
	Word_t Words = (sizeof(jLpm_t) + cJL_BYTESPERPTR - 1) / cJL_BYTESPERPTR;
	JudyFree((void *) PjpmFree, Words);
	if (PjpmStats != NULL)
		PjpmStats->jpm_TotalMemWords -= Words;
}

Pjbl_t judyLAllocJBL(Pjpm_t Pjpm)
{
	Pjbl_t PjblRaw = NULL;
	Word_t Words = sizeof(jbl_t) / cJL_BYTESPERPTR;
	assert((Words * cJL_BYTESPERPTR) == sizeof(jbl_t));

	MALLOC(PjblRaw, Words, Pjpm);
	return PjblRaw;
}

void judyLFreeJBL(Pjbl_t Pjbl, Pjpm_t Pjpm)
{
	Word_t Words = sizeof(jbl_t) / cJL_BYTESPERPTR;
	JudyFree((void *) Pjbl, Words);
	Pjpm->jpm_TotalMemWords -= Words;
}

Pjbb_t judyLAllocJBB(Pjpm_t Pjpm)
{
	Word_t Words = sizeof(jbb_t) / cJL_BYTESPERPTR;
	Pjbb_t PjbbRaw = NULL;
	assert((Words * cJL_BYTESPERPTR) == sizeof(jbb_t));

	MALLOC(PjbbRaw, Words, Pjpm);
	return PjbbRaw;
}

void judyLFreeJBB(Pjbb_t Pjbb, Pjpm_t Pjpm)
{
	Word_t Words = sizeof(jbb_t) / cJL_BYTESPERPTR;
	JudyFree((void *) Pjbb, Words);
	Pjpm->jpm_TotalMemWords -= Words;
}

Pjp_t judyLAllocJBBJP(Word_t NumJPs, Pjpm_t Pjpm)
{
	Word_t Words = JL_BRANCHJP_NUMJPSTOWORDS(NumJPs);
	Pjp_t PjpRaw;

	MALLOC(PjpRaw, Words, Pjpm);
	return PjpRaw;
}

void judyLFreeJBBJP(Pjp_t Pjp, Word_t NumJPs, Pjpm_t Pjpm)
{
	Word_t Words = JL_BRANCHJP_NUMJPSTOWORDS(NumJPs);
	JudyFree((void *) Pjp, Words);
	Pjpm->jpm_TotalMemWords -= Words;
}

Pjbu_t judyLAllocJBU(Pjpm_t Pjpm)
{
	Word_t Words = sizeof(jbu_t) / cJL_BYTESPERPTR;
	Pjbu_t PjbuRaw = NULL;
	assert((Words * cJL_BYTESPERPTR) == sizeof(jbu_t));

	MALLOC(PjbuRaw, Words, Pjpm);
	return PjbuRaw;
}

void judyLFreeJBU(Pjbu_t Pjbu, Pjpm_t Pjpm)
{
	Word_t Words = sizeof(jbu_t) / cJL_BYTESPERPTR;
	JudyFree((void *) Pjbu, Words);
	Pjpm->jpm_TotalMemWords -= Words;
}

Pjll_t judyLAllocJLL1(Word_t Pop1, Pjpm_t Pjpm)
{
	Word_t Words = JL_LEAF1POPTOWORDS(Pop1);
	Pjll_t PjllRaw = NULL;

	MALLOC(PjllRaw, Words, Pjpm);
	return PjllRaw;
}

void judyLFreeJLL1(Pjll_t Pjll, Word_t Pop1, Pjpm_t Pjpm)
{
	Word_t Words = JL_LEAF1POPTOWORDS(Pop1);
	JudyFree((void *) Pjll, Words);
	Pjpm->jpm_TotalMemWords -= Words;
}

Pjll_t judyLAllocJLL2(Word_t Pop1, Pjpm_t Pjpm)
{
	Word_t Words = JL_LEAF2POPTOWORDS(Pop1);
	Pjll_t PjllRaw = NULL;

	MALLOC(PjllRaw, Words, Pjpm);
	return PjllRaw;
}

void judyLFreeJLL2(Pjll_t Pjll, Word_t Pop1, Pjpm_t Pjpm)
{
	Word_t Words = JL_LEAF2POPTOWORDS(Pop1);
	JudyFree((void *) Pjll, Words);
	Pjpm->jpm_TotalMemWords -= Words;
}

Pjll_t judyLAllocJLL3(Word_t Pop1, Pjpm_t Pjpm)
{
	Word_t Words = JL_LEAF3POPTOWORDS(Pop1);
	Pjll_t PjllRaw = NULL;

	MALLOC(PjllRaw, Words, Pjpm);
	return PjllRaw;
}

void judyLFreeJLL3(Pjll_t Pjll, Word_t Pop1, Pjpm_t Pjpm)
{
	Word_t Words = JL_LEAF3POPTOWORDS(Pop1);
	JudyFree((void *) Pjll, Words);
	Pjpm->jpm_TotalMemWords -= Words;
}

Pjlw_t judyLAllocJLW(Word_t Pop1)
{
	Word_t Words = JL_LEAFWPOPTOWORDS(Pop1);
	Pjlw_t Pjlw = JudyMalloc(Words);
	if (Pjlw == NULL)
		JL_SET_ERRNO(JLE_NOMEM);
	return Pjlw;
}

void judyLFreeJLW(Pjlw_t Pjlw, Word_t Pop1, Pjpm_t Pjpm)
{
	Word_t Words = JL_LEAFWPOPTOWORDS(Pop1);
	JudyFree((void *) Pjlw, Words);
	if (Pjpm)
		Pjpm->jpm_TotalMemWords -= Words;
}

Pjlb_t judyLAllocJLB1(Pjpm_t Pjpm)
{
	Word_t Words = sizeof(jlb_t) / cJL_BYTESPERPTR;
	Pjlb_t PjlbRaw = NULL;
	assert((Words * cJL_BYTESPERPTR) == sizeof(jlb_t));

	MALLOC(PjlbRaw, Words, Pjpm);
	return PjlbRaw;
}

void judyLFreeJLB1(Pjlb_t Pjlb, Pjpm_t Pjpm)
{
	Word_t Words = sizeof(jlb_t) / cJL_BYTESPERPTR;
	JudyFree((void *) Pjlb, Words);
	Pjpm->jpm_TotalMemWords -= Words;
}

Pjv_t judyLAllocJV(Word_t Pop1, Pjpm_t Pjpm)
{
	Word_t Words = JL_LEAFVPOPTOWORDS(Pop1);
	Pjv_t PjvRaw = NULL;

	MALLOC(PjvRaw, Words, Pjpm);
	return PjvRaw;
}

void judyLFreeJV(Pjv_t Pjv, Word_t Pop1, Pjpm_t Pjpm)
{
	Word_t Words = JL_LEAFVPOPTOWORDS(Pop1);
	JudyFree((void *) Pjv, Words);
	Pjpm->jpm_TotalMemWords -= Words;
}

size_t JudyLMemUsed(const void *PArray)
{
	Word_t Words = 0;

        if (PArray == NULL) return 0;

	if (JL_LEAFW_POP0(PArray) < cJL_LEAFW_MAXPOP1) {
	    Pjlw_t Pjlw = P_JLW(PArray);
	    Words = JL_LEAFWPOPTOWORDS(Pjlw[0] + 1);
	} else {
	    Pjpm_t Pjpm = P_JPM(PArray);
	    Words = Pjpm->jpm_TotalMemWords;
	}

	return (Words * sizeof(Word_t));
}
