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
		JL_SET_ERRNO(JL_ERRNO_NOMEM);			\
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
		JL_SET_ERRNO(JL_ERRNO_NOMEM);
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

static Word_t judyLGetMemActive(Pjp_t Pjp)
{
	Word_t offset;		// in a branch.
	Word_t Bytes = 0;	// actual bytes used at this level.

	switch (JL_JPTYPE(Pjp)) {
	case cJL_JPBRANCH_L2: case cJL_JPBRANCH_L3: case cJL_JPBRANCH_L:
	{
		Pjbl_t Pjbl = P_JBL(Pjp->jp_Addr);

		for (offset = 0; offset < (Pjbl->jbl_NumJPs); ++offset)
			Bytes += judyLGetMemActive((Pjbl->jbl_jp) + offset);

		return (Bytes + sizeof(jbl_t));
	}
	case cJL_JPBRANCH_B2: case cJL_JPBRANCH_B3: case cJL_JPBRANCH_B:
	{
		Word_t subexp;
		Word_t jpcount;
		Pjbb_t Pjbb = P_JBB(Pjp->jp_Addr);

		for (subexp = 0; subexp < cJL_NUMSUBEXPB; ++subexp) {
			jpcount = judyCountBits(JL_JBB_BITMAP(Pjbb, subexp));
			Bytes += jpcount * sizeof(jp_t);

			for (offset = 0; offset < jpcount; ++offset) {
				Bytes += judyLGetMemActive(P_JP(JL_JBB_PJP(Pjbb, subexp))
						     + offset);
			}
		}

		return (Bytes + sizeof(jbb_t));
	}
	case cJL_JPBRANCH_U2: case cJL_JPBRANCH_U3: case cJL_JPBRANCH_U:
	{
		Pjbu_t Pjbu = P_JBU(Pjp->jp_Addr);

		for (offset = 0; offset < cJL_BRANCHUNUMJPS; ++offset) {
			if (((Pjbu->jbu_jp[offset].jp_Type) >= cJL_JPNULL1)
			    && ((Pjbu->jbu_jp[offset].jp_Type) <= cJL_JPNULLMAX)) {
				continue;	// skip null JP to save time.
			}

			Bytes += judyLGetMemActive(Pjbu->jbu_jp + offset);
		}

		return (Bytes + sizeof(jbu_t));
	}
#define JUDY_LEAF_TOTAL(_N, _Pjp) ((_N + sizeof(Word_t)) * (JL_JPLEAF_POP0(_Pjp) + 1))
	case cJL_JPLEAF1: return JUDY_LEAF_TOTAL(1, Pjp);
	case cJL_JPLEAF2: return JUDY_LEAF_TOTAL(2, Pjp);
	case cJL_JPLEAF3: return JUDY_LEAF_TOTAL(3, Pjp);
	case cJL_JPLEAF_B1: return (JL_JPLEAF_POP0(Pjp) +1) * sizeof(Word_t) + sizeof(jlb_t);
	case cJL_JPIMMED_1_01: return 0;
	case cJL_JPIMMED_2_01: return 0;
	case cJL_JPIMMED_3_01: return 0;
	case cJL_JPIMMED_1_02: return (sizeof(Word_t) * 2);
	case cJL_JPIMMED_1_03: return (sizeof(Word_t) * 3);
	}

	return 0;
}

size_t JudyLMemActive(const void *PArray)
{
	if (PArray == NULL)
		return 0;

	if (JL_LEAFW_POP0(PArray) < cJL_LEAFW_MAXPOP1) {
		Pjlw_t Pjlw = P_JLW(PArray);	// first word of leaf.
		Word_t Words = Pjlw[0] + 1;	// population.
		return (((Words * 2) + 1) * sizeof(Word_t));
	} else {
		Pjpm_t Pjpm = P_JPM(PArray);
		return (judyLGetMemActive(&Pjpm->jpm_JP) + sizeof(jLpm_t));
	}
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
