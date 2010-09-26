#include "JudyL.h"

void **JudyLFirst(const void *PArray, uint32_t *PIndex)
{
	void **PValue;
	if (PIndex == NULL) {
		JL_SET_ERRNO(JL_ERRNO_NULLPINDEX);
		return PPJERR;
	}

	if ((PValue = JudyLGet(PArray, *PIndex)) == PPJERR)
		return PPJERR;
	if (PValue != NULL)
		return PValue;	// found *PIndex.
	return JudyLNext(PArray, PIndex);
}

void **JudyLLast(const void *PArray, uint32_t *PIndex)
{
	void **PValue;
	if (PIndex == NULL) {
		JL_SET_ERRNO(JL_ERRNO_NULLPINDEX);	// caller error.
		return PPJERR;
	}

	if ((PValue = JudyLGet(PArray, *PIndex)) == PPJERR)
		return PPJERR;
	if (PValue != NULL)
		return PValue;
	return JudyLPrev(PArray, PIndex);
}

void **JudyLGet(const void *PArray, uint32_t Index)
{
	Pjp_t Pjp;		// current JP while walking the tree.
	Pjpm_t Pjpm;		// for global accounting.
	uint8_t Digit;		// byte just decoded from Index.
	Word_t Pop1;		// leaf population (number of indexes).
	Pjll_t Pjll;		// pointer to LeafL.
	int posidx;		// signed offset in leaf.

	if (PArray == NULL)
		return NULL;

	if (JL_LEAFW_POP0(PArray) < cJL_LEAFW_MAXPOP1) {	// must be a LEAFW
		Pjlw_t Pjlw = P_JLW(PArray);	// first word of leaf.
		Pop1 = Pjlw[0] + 1;
		posidx = judySearchLeafW(Pjlw + 1, Pop1, Index);
		if (posidx >= 0)
			return (void **)(JL_LEAFWVALUEAREA(Pjlw, Pop1) + posidx);
		return NULL;
	}

	Pjpm = P_JPM(PArray);
	Pjp = &Pjpm->jpm_JP;	// top branch is below JPM.

ContinueWalk:	// for going down one level; come here with Pjp set.
	switch (JL_JPTYPE(Pjp)) {
	case cJL_JPNULL1: case cJL_JPNULL2: case cJL_JPNULL3:
		return NULL;
	case cJL_JPBRANCH_L2:
		if (JL_DCDNOTMATCHINDEX(Index, Pjp, 2))
			break;
		Digit = JL_DIGITATSTATE(Index, 2);
		goto JudyBranchL;
	case cJL_JPBRANCH_L3:
		Digit = JL_DIGITATSTATE(Index, 3);
		goto JudyBranchL;
	case cJL_JPBRANCH_L:{
		Pjbl_t Pjbl;
		Digit = JL_DIGITATSTATE(Index, cJL_ROOTSTATE);
	JudyBranchL: // Common code for all BranchLs; come here with Digit set:
		Pjbl = P_JBL(Pjp->jp_Addr);
		posidx = 0;
		do {
			if (Pjbl->jbl_Expanse[posidx] == Digit) {
				Pjp = Pjbl->jbl_jp + posidx;
				goto ContinueWalk;
			}
		} while (++posidx != Pjbl->jbl_NumJPs);

		break;
	}
	case cJL_JPBRANCH_B2:
		if (JL_DCDNOTMATCHINDEX(Index, Pjp, 2))
			break;
		Digit = JL_DIGITATSTATE(Index, 2);
		goto JudyBranchB;
	case cJL_JPBRANCH_B3:
		Digit = JL_DIGITATSTATE(Index, 3);
		goto JudyBranchB;
	case cJL_JPBRANCH_B:{
		Pjbb_t Pjbb;
		Word_t subexp;	// in bitmap, 0..7.
		BITMAPB_t BitMap;	// for one subexpanse.
		BITMAPB_t BitMask;	// bit in BitMap for Indexs Digit.
		Digit = JL_DIGITATSTATE(Index, cJL_ROOTSTATE);
      JudyBranchB:
		Pjbb = P_JBB(Pjp->jp_Addr);
		subexp = Digit / cJL_BITSPERSUBEXPB;

		BitMap = JL_JBB_BITMAP(Pjbb, subexp);
		Pjp = P_JP(JL_JBB_PJP(Pjbb, subexp));

		BitMask = JL_BITPOSMASKB(Digit);

		// No JP in subexpanse for Index => Index not found:
		if (!(BitMap & BitMask))
			break;
		// Count JPs in the subexpanse below the one for Index:
		Pjp += judyCountBits(BitMap & (BitMask - 1));
		goto ContinueWalk;
	}
	// Notice the reverse order of the cases, and falling through to the next case,
	// for performance.
	case cJL_JPBRANCH_U:
		Pjp = JL_JBU_PJP(Pjp, Index, cJL_ROOTSTATE);
		if (JL_JPTYPE(Pjp) != cJL_JPBRANCH_U3)
			goto ContinueWalk;
	case cJL_JPBRANCH_U3:
		Pjp = JL_JBU_PJP(Pjp, Index, 3);
		if (JL_JPTYPE(Pjp) != cJL_JPBRANCH_U2)
			goto ContinueWalk;
	// and fall through.
	case cJL_JPBRANCH_U2:
		if (JL_DCDNOTMATCHINDEX(Index, Pjp, 2)) break;
		Pjp = JL_JBU_PJP(Pjp, Index, 2);
		goto ContinueWalk;
	case cJL_JPLEAF1:
		if (JL_DCDNOTMATCHINDEX(Index, Pjp, 1)) break;

		Pop1 = JL_JPLEAF_POP0(Pjp) + 1;
		Pjll = P_JLL(Pjp->jp_Addr);
		if ((posidx = judySearchLeaf1(Pjll, Pop1, Index)) < 0) break;
		return (void **)(JL_LEAF1VALUEAREA(Pjll, Pop1) + posidx);
	case cJL_JPLEAF2:
		if (JL_DCDNOTMATCHINDEX(Index, Pjp, 2)) break;

		Pop1 = JL_JPLEAF_POP0(Pjp) + 1;
		Pjll = P_JLL(Pjp->jp_Addr);

		if ((posidx = judySearchLeaf2(Pjll, Pop1, Index)) < 0) break;
		return ((void **)(JL_LEAF2VALUEAREA(Pjll, Pop1) + posidx));
	case cJL_JPLEAF3:
		Pop1 = JL_JPLEAF_POP0(Pjp) + 1;
		Pjll = P_JLL(Pjp->jp_Addr);

		if ((posidx = judySearchLeaf3(Pjll, Pop1, Index)) < 0) break;
		return ((void **)(JL_LEAF3VALUEAREA(Pjll, Pop1) + posidx));
	case cJL_JPLEAF_B1:{
		Pjlb_t Pjlb;
		Word_t subexp;
		BITMAPL_t BitMap, BitMask;
		Pjv_t Pjv;
		if (JL_DCDNOTMATCHINDEX(Index, Pjp, 1)) break;

		Pjlb = P_JLB(Pjp->jp_Addr);
		// JudyL is much more complicated because of value area subarrays:
		Digit = JL_DIGITATSTATE(Index, 1);
		subexp = Digit / cJL_BITSPERSUBEXPL;
		BitMap = JL_JLB_BITMAP(Pjlb, subexp);
		BitMask = JL_BITPOSMASKL(Digit);

		// No value in subexpanse for Index => Index not found:
		if (!(BitMap & BitMask)) break;

		// Count value areas in the subexpanse below the one for Index:
		Pjv = P_JV(JL_JLB_PVALUE(Pjlb, subexp));
		assert(Pjv != NULL);
		posidx = judyCountBits(BitMap & (BitMask - 1));

		return (void **)(Pjv + posidx);
	}
	case cJL_JPIMMED_1_01: case cJL_JPIMMED_2_01: case cJL_JPIMMED_3_01:
		if (JL_JPDCDPOP0(Pjp) != JL_TRIMTODCDSIZE(Index)) break;
		return (void **)(&Pjp->jp_Addr);	// immediate value area.
	case cJL_JPIMMED_1_03:
		if (((uint8_t *)Pjp->jp_LIndex)[2] == (uint8_t)Index)
			return (void **)(P_JV(Pjp->jp_Addr) + 2);
	case cJL_JPIMMED_1_02:
		if (((uint8_t *)Pjp->jp_LIndex)[1] == (uint8_t)Index)
			return (void **)(P_JV(Pjp->jp_Addr) + 1);
		if (((uint8_t *)Pjp->jp_LIndex)[0] == (uint8_t)Index)
			return (void **)(P_JV(Pjp->jp_Addr) + 0);
		break;
	default:
		JL_SET_ERRNO(JL_ERRNO_CORRUPT);
		return PPJERR;
	}

	return NULL;
}
