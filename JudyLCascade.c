#include "JudyL.h"

extern int judyCreateBranchL(Pjp_t, Pjp_t, uint8_t *, Word_t, void *);
extern int judyCreateBranchB(Pjp_t, Pjp_t, uint8_t *, Word_t, void *);
static const jbb_t StageJBBZero;	// zeroed versions of namesake struct.

static void judyCopy3toW(PWord_t PDest, uint8_t * PSrc, Word_t LeafIndexes)
{
	do {
		JL_COPY3_PINDEX_TO_LONG(*PDest, PSrc);
		PSrc += 3;
		PDest += 1;
	} while (--LeafIndexes);
}

static void judyCopyWto3(uint8_t *PDest, PWord_t PSrc, Word_t LeafIndexes)
{
	do {
		JL_COPY3_LONG_TO_PINDEX(PDest, *PSrc);
		PSrc += 1;
		PDest += 3;
	} while (--LeafIndexes);
}

#define FREEALLEXIT(ExpCnt,StageJP,Pjpm) {			\
    Word_t _expct = (ExpCnt);					\
    while (_expct--) judyLFreeSM(&((StageJP)[_expct]), Pjpm);    \
    return -1;                                                 \
}

#define ZEROJP(SubJPCount) {						\
	int ii;								\
	for (ii = 0; ii < cJL_NUMSUBEXPB; ii++) (SubJPCount[ii]) = 0;	\
}

static int judyStageJBBtoJBB(Pjp_t PjpLeaf,	// JP of leaf being splayed.
			     Pjbb_t PStageJBB,	// temp jbb_t on stack.
			     Pjp_t PjpArray,	// array of JPs to splayed new leaves.
			     uint8_t * PSubCount,	// count of JPs for each subexpanse.
			     Pjpm_t Pjpm)	// the jpm_t for JudyAlloc*().
{
	Pjbb_t PjbbRaw;		// pointer to new bitmap branch.
	Pjbb_t Pjbb;
	Word_t subexp;

	if ((PjbbRaw = judyLAllocJBB(Pjpm)) == NULL)
		return -1;
	Pjbb = P_JBB(PjbbRaw);
	*Pjbb = *PStageJBB;

	for (subexp = 0; subexp < cJL_NUMSUBEXPB; subexp++) {
		Pjp_t PjpRaw, Pjp;
		Word_t NumJP;	// number of JPs in each subexpanse.

		if ((NumJP = PSubCount[subexp]) == 0)
			continue;	// empty.

		if ((PjpRaw = judyLAllocJBBJP(NumJP, Pjpm)) == NULL) {
			while (subexp--) {
				if ((NumJP = PSubCount[subexp]) == 0)
					continue;

				PjpRaw = JL_JBB_PJP(Pjbb, subexp);
				judyLFreeJBBJP(PjpRaw, NumJP, Pjpm);
			}
			judyLFreeJBB(PjbbRaw, Pjpm);
			return -1;	// out of memory.
		}
		Pjp = P_JP(PjpRaw);

		JL_JBB_PJP(Pjbb, subexp) = PjpRaw;
		JL_COPYMEM(Pjp, PjpArray, NumJP);
		PjpArray += NumJP;
	}
	PjpLeaf->jp_Addr = (Word_t) PjbbRaw;
	PjpLeaf->jp_Type += cJL_JPBRANCH_B2 - cJL_JPLEAF2;	// Leaf to BranchB.

	return 1;
}

static Pjlb_t judyJLL2toJLB1(uint16_t * Pjll,	// array of 16-bit indexes.
			     Pjv_t Pjv,	// array of associated values.
			     Word_t LeafPop1,	// number of indexes/values.
			     void *Pjpm)	// jpm_t for JudyAlloc*()/JudyFree*().
{
	Pjlb_t PjlbRaw, Pjlb;
	int offset, subexp;

	if ((PjlbRaw = judyLAllocJLB1(Pjpm)) == NULL)
		return NULL;
	Pjlb = P_JLB(PjlbRaw);
	for (offset = 0; offset < LeafPop1; ++offset)
		JL_BITMAPSETL(Pjlb, Pjll[offset]);

	for (subexp = 0; subexp < cJL_NUMSUBEXPL; ++subexp) {
		struct _POINTER_VALUES {
			Word_t pv_Pop1;	// size of value area.
			Pjv_t pv_Pjv;	// raw pointer to value area.
		} pv[cJL_NUMSUBEXPL];

		pv[subexp].pv_Pop1 = judyCountBits(JL_JLB_BITMAP(Pjlb, subexp));
		if (pv[subexp].pv_Pop1) {
			Pjv_t Pjvnew;

			pv[subexp].pv_Pjv = judyLAllocJV(pv[subexp].pv_Pop1, Pjpm);
			if (pv[subexp].pv_Pjv == NULL) {
				while (subexp--) {
					if (pv[subexp].pv_Pop1)
						judyLFreeJV(pv[subexp].pv_Pjv, 
							    pv[subexp].pv_Pop1, Pjpm);
				}
				judyLFreeJLB1(PjlbRaw, Pjpm);
				return NULL;
			}

			Pjvnew = P_JV(pv[subexp].pv_Pjv);
			JL_COPYMEM(Pjvnew, Pjv, pv[subexp].pv_Pop1);
			Pjv += pv[subexp].pv_Pop1;	// advance value pointer.

			JL_JLB_PVALUE(Pjlb, subexp) = pv[subexp].pv_Pjv;
		}
	}

	return PjlbRaw;
}

int judyCascade1(Pjp_t Pjp, void *Pjpm)
{
	uint8_t *PLeaf;
	Pjlb_t PjlbRaw, Pjlb;
	Word_t DcdP0, Pop1, ii;
	Pjv_t Pjv;

	assert(JL_JPTYPE(Pjp) == cJL_JPLEAF1);
	assert((JL_JPDCDPOP0(Pjp) & 0xFF) == (cJL_LEAF1_MAXPOP1 - 1));

	PjlbRaw = judyLAllocJLB1(Pjpm);
	if (PjlbRaw == NULL)
		return -1;

	Pjlb = P_JLB(PjlbRaw);
	PLeaf = (uint8_t *) P_JLL(Pjp->jp_Addr);
	Pop1 = JL_JPLEAF_POP0(Pjp) + 1;

	Pjv = JL_LEAF1VALUEAREA(PLeaf, Pop1);

	for (ii = 0; ii < Pop1; ii++)
		JL_BITMAPSETL(Pjlb, PLeaf[ii]);

	for (ii = 0; ii < cJL_NUMSUBEXPL; ii++) {
		if ((Pop1 = judyCountBits(JL_JLB_BITMAP(Pjlb, ii)))) {
			Pjv_t PjvnewRaw, Pjvnew;	// value area of new leaf.

			PjvnewRaw = judyLAllocJV(Pop1, Pjpm);
			if (PjvnewRaw == NULL) {
				while (ii--) {
					if ((Pop1 = judyCountBits(JL_JLB_BITMAP(Pjlb, ii)))) {
						PjvnewRaw = JL_JLB_PVALUE(Pjlb, ii);
						judyLFreeJV(PjvnewRaw, Pop1, Pjpm);
					}
				}
				judyLFreeJLB1(PjlbRaw, Pjpm);
				return -1;
			}
			Pjvnew = P_JV(PjvnewRaw);
			JL_COPYMEM(Pjvnew, Pjv, Pop1);

			Pjv += Pop1;
			JL_JLB_PVALUE(Pjlb, ii) = PjvnewRaw;
		}
	}

	DcdP0 = JL_JPDCDPOP0(Pjp) | (PLeaf[0] & cJL_DCDMASK(1));
	JL_JPSETADT(Pjp, (Word_t)PjlbRaw, DcdP0, cJL_JPLEAF_B1);

	return 1;
}

int judyCascade2(Pjp_t Pjp, void *Pjpm)
{
	uint16_t *PLeaf;	// pointer to leaf, explicit type.
	Word_t End, Start;	// temporaries.
	Word_t ExpCnt;		// count of expanses of splay.
	Word_t CIndex;		// current Index word.
	Pjv_t Pjv;		// value area of leaf.
	jp_t StageJP[cJL_LEAF2_MAXPOP1];	// JPs of new leaves
	uint8_t StageExp[cJL_LEAF2_MAXPOP1];	// Expanses of new leaves
	uint8_t SubJPCount[cJL_NUMSUBEXPB];	// JPs in each subexpanse
	jbb_t StageJBB;		// staged bitmap branch

	assert(JL_JPTYPE(Pjp) == cJL_JPLEAF2);
	assert((JL_JPDCDPOP0(Pjp) & 0xFFFF) == (cJL_LEAF2_MAXPOP1 - 1));

	PLeaf = (uint16_t *) P_JLL(Pjp->jp_Addr);
	Pjv = JL_LEAF2VALUEAREA(PLeaf, cJL_LEAF2_MAXPOP1);

	CIndex = PLeaf[0];
	if (!JL_DIGITATSTATE(CIndex ^ PLeaf[cJL_LEAF2_MAXPOP1 - 1], 2)) {
		Word_t DcdP0;
		Pjlb_t PjlbRaw;
		PjlbRaw = judyJLL2toJLB1(PLeaf, Pjv, cJL_LEAF2_MAXPOP1, Pjpm);
		if (PjlbRaw == NULL)
			return -1;	// out of memory

		DcdP0 = (CIndex & cJL_DCDMASK(1)) | JL_JPDCDPOP0(Pjp);
		JL_JPSETADT(Pjp, (Word_t) PjlbRaw, DcdP0, cJL_JPLEAF_B1);

		return 1;
	}
	StageJBB = StageJBBZero;
	ZEROJP(SubJPCount);

	for (ExpCnt = Start = 0, End = 1;; End++) {
		if ((End == cJL_LEAF2_MAXPOP1) || (JL_DIGITATSTATE(CIndex ^ PLeaf[End], 2))) {
			Pjp_t PjpJP = StageJP + ExpCnt;
			Word_t Pop1 = End - Start;
			Word_t expanse = JL_DIGITATSTATE(CIndex, 2);
			Word_t subexp = expanse / cJL_BITSPERSUBEXPB;
			JL_JBB_BITMAP(&StageJBB, subexp) |= JL_BITPOSMASKB(expanse);
			SubJPCount[subexp]++;

			StageExp[ExpCnt] = JL_DIGITATSTATE(CIndex, 2);

			if (Pop1 == 1) {
				Word_t DcdP0;
				DcdP0 = (JL_JPDCDPOP0(Pjp) & cJL_DCDMASK(1)) | CIndex;
				JL_JPSETADT(PjpJP, Pjv[Start], DcdP0, cJL_JPIMMED_1_01);
			} else if (Pop1 <= cJL_IMMED1_MAXPOP1) {
				Pjv_t PjvnewRaw;
				Pjv_t Pjvnew;

				PjvnewRaw = judyLAllocJV(Pop1, Pjpm);
				if (PjvnewRaw == NULL)
					FREEALLEXIT(ExpCnt, StageJP, Pjpm);

				Pjvnew = P_JV(PjvnewRaw);

				JL_COPYMEM(Pjvnew, Pjv + Start, Pop1);
				PjpJP->jp_Addr = (Word_t) PjvnewRaw;

				JL_COPYMEM(PjpJP->jp_LIndex, PLeaf + Start, Pop1);
				PjpJP->jp_Type = cJL_JPIMMED_1_02 + Pop1 - 2;
			} else if (Pop1 <= cJL_LEAF1_MAXPOP1) {
				Word_t DcdP0;
				Pjll_t PjllRaw, Pjll;
				Pjv_t Pjvnew;	// value area of new leaf.

				PjllRaw = judyLAllocJLL1(Pop1, Pjpm);
				if (PjllRaw == NULL)
					FREEALLEXIT(ExpCnt, StageJP, Pjpm);

				Pjll = P_JLL(PjllRaw);
				Pjvnew = JL_LEAF1VALUEAREA(Pjll, Pop1);
				JL_COPYMEM(Pjvnew, Pjv + Start, Pop1);
				JL_COPYMEM((uint8_t *) Pjll, PLeaf + Start, Pop1);

				DcdP0 = (JL_JPDCDPOP0(Pjp) & cJL_DCDMASK(2))
				    | (CIndex & cJL_DCDMASK(2 - 1)) | (Pop1 - 1);

				JL_JPSETADT(PjpJP, (Word_t) PjllRaw, DcdP0, cJL_JPLEAF1);
			} else {
				Word_t DcdP0;
				Pjlb_t PjlbRaw;
				PjlbRaw = judyJLL2toJLB1(PLeaf + Start,
							 Pjv + Start, Pop1, Pjpm);
				if (PjlbRaw == NULL)
					FREEALLEXIT(ExpCnt, StageJP, Pjpm);

				DcdP0 = (JL_JPDCDPOP0(Pjp) & cJL_DCDMASK(2))
				    | (CIndex & cJL_DCDMASK(2 - 1)) | (Pop1 - 1);

				JL_JPSETADT(PjpJP, (Word_t) PjlbRaw, DcdP0, cJL_JPLEAF_B1);
			}
			ExpCnt++;
			if (End == cJL_LEAF2_MAXPOP1)
				break;
			CIndex = PLeaf[End];
			Start = End;
		}
	}

	if (ExpCnt <= cJL_BRANCHLMAXJPS) {
		if (judyCreateBranchL(Pjp, StageJP, StageExp, ExpCnt, Pjpm) == -1)
			FREEALLEXIT(ExpCnt, StageJP, Pjpm);
		Pjp->jp_Type = cJL_JPBRANCH_L2;
	} else {
		if (judyStageJBBtoJBB(Pjp, &StageJBB, StageJP, SubJPCount, Pjpm) == -1)
			FREEALLEXIT(ExpCnt, StageJP, Pjpm);
	}
	return 1;
}

int judyCascade3(Pjp_t Pjp, void *Pjpm)
{
	uint8_t *PLeaf;		// pointer to leaf, explicit type.
	Word_t End, Start;	// temporaries.
	Word_t ExpCnt;		// count of expanses of splay.
	Word_t CIndex;		// current Index word.
	Pjv_t Pjv;		// value area of leaf.
	jp_t StageJP[cJL_LEAF3_MAXPOP1];	// JPs of new leaves
	Word_t StageA[cJL_LEAF3_MAXPOP1];
	uint8_t StageExp[cJL_LEAF3_MAXPOP1];	// Expanses of new leaves
	uint8_t SubJPCount[cJL_NUMSUBEXPB];	// JPs in each subexpanse
	jbb_t StageJBB;		// staged bitmap branch

	assert(JL_JPTYPE(Pjp) == cJL_JPLEAF3);
	assert((JL_JPDCDPOP0(Pjp) & 0xFFFFFF) == (cJL_LEAF3_MAXPOP1 - 1));

	PLeaf = (uint8_t *) P_JLL(Pjp->jp_Addr);
	judyCopy3toW(StageA, PLeaf, cJL_LEAF3_MAXPOP1);
	Pjv = JL_LEAF3VALUEAREA(PLeaf, cJL_LEAF3_MAXPOP1);

	CIndex = StageA[0];
	if (!JL_DIGITATSTATE(CIndex ^ StageA[cJL_LEAF3_MAXPOP1 - 1], 3)) {
		Word_t DcdP0;
		Pjll_t PjllRaw, Pjll;
		Pjv_t Pjvnew;

		PjllRaw = judyLAllocJLL2(cJL_LEAF3_MAXPOP1, Pjpm);
		if (PjllRaw == NULL)
			return -1;
		Pjll = P_JLL(PjllRaw);

		JL_COPYMEM((uint16_t *) Pjll, StageA, cJL_LEAF3_MAXPOP1);
		Pjvnew = JL_LEAF2VALUEAREA(Pjll, cJL_LEAF3_MAXPOP1);
		JL_COPYMEM(Pjvnew, Pjv, cJL_LEAF3_MAXPOP1);

		DcdP0 = (CIndex & cJL_DCDMASK(2)) | JL_JPDCDPOP0(Pjp);
		JL_JPSETADT(Pjp, (Word_t) PjllRaw, DcdP0, cJL_JPLEAF2);

		return 1;
	}

	StageJBB = StageJBBZero;	// zero staged bitmap branch
	ZEROJP(SubJPCount);
	for (ExpCnt = Start = 0, End = 1;; End++) {
		if ((End == cJL_LEAF3_MAXPOP1) || (JL_DIGITATSTATE(CIndex^StageA[End], 3))) {
			Pjp_t PjpJP = StageJP + ExpCnt;
			Word_t Pop1 = End - Start;
			Word_t expanse = JL_DIGITATSTATE(CIndex, 3);
			Word_t subexp = expanse / cJL_BITSPERSUBEXPB;
			JL_JBB_BITMAP(&StageJBB, subexp) |= JL_BITPOSMASKB(expanse);
			SubJPCount[subexp]++;

			StageExp[ExpCnt] = JL_DIGITATSTATE(CIndex, 3);

			if (Pop1 == 1) {
				Word_t DcdP0;
				DcdP0 = (JL_JPDCDPOP0(Pjp) & cJL_DCDMASK(2)) | CIndex;
				JL_JPSETADT(PjpJP, Pjv[Start], DcdP0, cJL_JPIMMED_2_01);
			} else {
				Word_t DcdP0;
				Pjll_t PjllRaw, Pjll;
				Pjv_t Pjvnew;

				PjllRaw = judyLAllocJLL2(Pop1, Pjpm);
				if (PjllRaw == NULL)
					FREEALLEXIT(ExpCnt, StageJP, Pjpm);

				Pjll = P_JLL(PjllRaw);
				Pjvnew = JL_LEAF2VALUEAREA(Pjll, Pop1);
				JL_COPYMEM(Pjvnew, Pjv + Start, Pop1);
				JL_COPYMEM((uint16_t *) Pjll, StageA + Start, Pop1);

				DcdP0 = (JL_JPDCDPOP0(Pjp) & cJL_DCDMASK(3))
				    | (CIndex & cJL_DCDMASK(3 - 1)) | (Pop1 - 1);

				JL_JPSETADT(PjpJP, (Word_t) PjllRaw, DcdP0, cJL_JPLEAF2);
			}
			ExpCnt++;
			if (End == cJL_LEAF3_MAXPOP1)
				break;

			CIndex = StageA[End];
			Start = End;
		}
	}

	if (ExpCnt <= cJL_BRANCHLMAXJPS) {	// put the Leaves below a BranchL
		if (judyCreateBranchL(Pjp, StageJP, StageExp, ExpCnt, Pjpm) == -1)
			FREEALLEXIT(ExpCnt, StageJP, Pjpm);
		Pjp->jp_Type = cJL_JPBRANCH_L3;
	} else {
		if (judyStageJBBtoJBB(Pjp, &StageJBB, StageJP, SubJPCount, Pjpm)
		    == -1)
			FREEALLEXIT(ExpCnt, StageJP, Pjpm);
	}
	return 1;
}

/**
 * (Compressed) cJL_LEAF3[7], cJ1_JPBRANCH_L.
 * Cascade from a LEAFW (under Pjp) to one of the following:
 *  1. if LEAFW is in 1 expanse:
 *        create linear branch with a JPLEAF3[7] under it
 *  2. LEAFW contains multiple expanses:
 *        create linear or bitmap branch containing new expanses
 *        each new expanse is either a: 32   64
 *               JPIMMED_3_01  branch    Y    N
 *               JPIMMED_7_01  branch    N    Y
 *               JPLEAF3                 Y    N
 *               JPLEAF7                 N    Y
 */
int judyCascadeL(Pjp_t Pjp, void *Pjpm)
{
	Pjlw_t Pjlw;		// leaf to work on.
	Word_t End, Start;	// temporaries.
	Word_t ExpCnt;		// count of expanses of splay.
	Word_t CIndex;		// current Index word.
	Pjv_t Pjv;		// value area of leaf.
	jp_t StageJP[cJL_LEAFW_MAXPOP1];
	uint8_t StageExp[cJL_LEAFW_MAXPOP1];
	uint8_t SubJPCount[cJL_NUMSUBEXPB];	// JPs in each subexpanse
	jbb_t StageJBB;		// staged bitmap branch

	Pjlw = P_JLW(Pjp->jp_Addr);
	assert(Pjlw[0] == (cJL_LEAFW_MAXPOP1 - 1));
	Pjv = JL_LEAFWVALUEAREA(Pjlw, cJL_LEAFW_MAXPOP1);
	Pjlw++;			// Now point to Index area

	CIndex = Pjlw[0];	// also used far below
	if (!JL_DIGITATSTATE(CIndex ^ Pjlw[cJL_LEAFW_MAXPOP1 - 1], cJL_ROOTSTATE)) {
		Pjll_t PjllRaw;	// pointer to new leaf.
		Pjll_t Pjll;
		Pjv_t Pjvnew;	// value area of new leaf.

		StageExp[0] = JL_DIGITATSTATE(CIndex, cJL_ROOTSTATE);

		PjllRaw = judyLAllocJLL3(cJL_LEAFW_MAXPOP1, Pjpm);
		if (PjllRaw == NULL)
			return -1;

		Pjll = P_JLL(PjllRaw);
		judyCopyWto3((uint8_t *) Pjll, Pjlw, cJL_LEAFW_MAXPOP1);
		Pjvnew = JL_LEAF3VALUEAREA(Pjll, cJL_LEAFW_MAXPOP1);
		JL_COPYMEM(Pjvnew, Pjv, cJL_LEAFW_MAXPOP1);

		JL_JPSETADT(&(StageJP[0]), (Word_t)PjllRaw, cJL_LEAFW_MAXPOP1-1, cJL_JPLEAF3);
		if (judyCreateBranchL(Pjp, StageJP, StageExp, 1, Pjpm) == -1)
			return -1;

		Pjp->jp_Type = cJL_JPBRANCH_L;
		return 1;
	}
	StageJBB = StageJBBZero;	// zero staged bitmap branch
	ZEROJP(SubJPCount);

	for (ExpCnt = Start = 0, End = 1;; End++) {
		if ((End == cJL_LEAFW_MAXPOP1) || (JL_DIGITATSTATE(CIndex ^ Pjlw[End], cJL_ROOTSTATE))) {
			Pjp_t PjpJP = StageJP + ExpCnt;
			Word_t Pop1 = End - Start;
			Word_t expanse = JL_DIGITATSTATE(CIndex, cJL_ROOTSTATE);
			Word_t subexp = expanse / cJL_BITSPERSUBEXPB;
			JL_JBB_BITMAP(&StageJBB, subexp) |= JL_BITPOSMASKB(expanse);
			SubJPCount[subexp]++;

			StageExp[ExpCnt] = JL_DIGITATSTATE(CIndex, cJL_ROOTSTATE);

			if (Pop1 == 1) {	// cJL_JPIMMED_3[7]_01
				JL_JPSETADT(PjpJP, Pjv[Start], CIndex, cJL_JPIMMED_3_01);
			} else { // Linear Leaf JPLEAF3[7]
				Pjll_t PjllRaw, Pjll;	// pointer to new leaf.
				Pjv_t Pjvnew;	// value area of new leaf.
				PjllRaw = judyLAllocJLL3(Pop1, Pjpm);
				if (PjllRaw == NULL)
					return -1;
				Pjll = P_JLL(PjllRaw);

				judyCopyWto3((uint8_t *) Pjll, Pjlw + Start, Pop1);
				Pjvnew = JL_LEAF3VALUEAREA(Pjll, Pop1);
				JL_COPYMEM(Pjvnew, Pjv + Start, Pop1);

				JL_JPSETADT(PjpJP, (Word_t) PjllRaw, Pop1 - 1, cJL_JPLEAF3);
			}
			ExpCnt++;
			if (End == cJL_LEAFW_MAXPOP1)
				break;

			CIndex = Pjlw[End];
			Start = End;
		}
	}

	if (ExpCnt <= cJL_BRANCHLMAXJPS) {
		if (judyCreateBranchL(Pjp, StageJP, StageExp, ExpCnt, Pjpm) == -1)
			FREEALLEXIT(ExpCnt, StageJP, Pjpm);
		Pjp->jp_Type = cJL_JPBRANCH_L;
	} else {
		if (judyStageJBBtoJBB(Pjp, &StageJBB, StageJP, SubJPCount, Pjpm) == -1)
			FREEALLEXIT(ExpCnt, StageJP, Pjpm);
		Pjp->jp_Type = cJL_JPBRANCH_B;
	}
	return 1;
}
