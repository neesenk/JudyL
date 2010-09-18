#include "JudyL.h"

PPvoid_t JudyLFirst(Pcvoid_t PArray, Word_t *PIndex, PJError_t PJError) 
{
	PPvoid_t PValue;
	if (PIndex == (PWord_t) NULL) {
		JL_SET_ERRNO(PJError, JL_ERRNO_NULLPINDEX);
		return (PPJERR);
	}

	if ((PValue = JudyLGet(PArray, *PIndex, PJError)) == PPJERR)
		return (PPJERR);
	if (PValue != (PPvoid_t) NULL)
		return (PValue);	// found *PIndex.
	return (JudyLNext(PArray, PIndex, PJError));
}

PPvoid_t JudyLLast(Pcvoid_t PArray, Word_t * PIndex, PJError_t PJError) 
{
	PPvoid_t PValue;
	if (PIndex == (PWord_t) NULL) {
		JL_SET_ERRNO(PJError, JL_ERRNO_NULLPINDEX);	// caller error.
		return (PPJERR);
	}

	if ((PValue = JudyLGet(PArray, *PIndex, PJError)) == PPJERR)
		return (PPJERR);
	if (PValue != (PPvoid_t) NULL)
		return (PValue);	// found *PIndex.
	return (JudyLPrev(PArray, PIndex, PJError));
}

int JudyLFirstEmpty(Pcvoid_t PArray, Word_t * PIndex, PJError_t PJError)
{
	PPvoid_t PValue;
	if (PIndex == (PWord_t) NULL) {
		JL_SET_ERRNO(PJError, JL_ERRNO_NULLPINDEX);
		return (JERRI);
	}

	if ((PValue = JudyLGet(PArray, *PIndex, PJError)) == PPJERR)
		return (JERRI);
	if (PValue == (PPvoid_t) NULL)
		return (1);	// found *PIndex.
	return (JudyLNextEmpty(PArray, PIndex, PJError));
}

int JudyLLastEmpty(Pcvoid_t PArray, Word_t * PIndex, PJError_t PJError)
{
	PPvoid_t PValue;
	if (PIndex == (PWord_t) NULL) {
		JL_SET_ERRNO(PJError, JL_ERRNO_NULLPINDEX);	// caller error.
		return (JERRI);
	}

	if ((PValue = JudyLGet(PArray, *PIndex, PJError)) == PPJERR)
		return (JERRI);
	if (PValue == (PPvoid_t) NULL)
		return (1);	// found *PIndex.
	return (JudyLPrevEmpty(PArray, PIndex, PJError));
}

PPvoid_t JudyLGet(Pcvoid_t PArray, Word_t Index, PJError_t PJError)
{
	Pjp_t Pjp;		// current JP while walking the tree.
	Pjpm_t Pjpm;		// for global accounting.
	uint8_t Digit;		// byte just decoded from Index.
	Word_t Pop1;		// leaf population (number of indexes).
	Pjll_t Pjll;		// pointer to LeafL.
	int posidx;		// signed offset in leaf.

	if (PArray == (Pcvoid_t) NULL)
		return ((PPvoid_t) NULL);

	if (JL_LEAFW_POP0(PArray) < cJL_LEAFW_MAXPOP1) {	// must be a LEAFW
		Pjlw_t Pjlw = P_JLW(PArray);	// first word of leaf.
		Pop1 = Pjlw[0] + 1;
		posidx = judySearchLeafW(Pjlw + 1, Pop1, Index);
		if (posidx >= 0)
			return ((PPvoid_t) (JL_LEAFWVALUEAREA(Pjlw, Pop1) + posidx));
		return ((PPvoid_t) NULL);
	}

	Pjpm = P_JPM(PArray);
	Pjp = &(Pjpm->jpm_JP);	// top branch is below JPM.

ContinueWalk:	// for going down one level; come here with Pjp set.
	switch (JL_JPTYPE(Pjp)) {
	// Ensure the switch table starts at 0 for speed; otherwise more code is executed:
	case 0:
		goto ReturnCorrupt;	// save a little code.
	// Note:  These are legitimate in a BranchU (only) and do not constitute a fault.
	case cJL_JPNULL1:
	case cJL_JPNULL2:
	case cJL_JPNULL3:
		return ((PPvoid_t) NULL);
	// Note:  The use of JL_DCDNOTMATCHINDEX() in branches is not strictly
	// required,since this can be done at leaf level, but it costs nothing to do it
	// sooner, and it aborts an unnecessary traversal sooner.
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
		// Common code for all BranchLs; come here with Digit set:
	      JudyBranchL:
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
// Common code for all BranchBs; come here with Digit set:
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
		Pjp += judyCountBitsB(BitMap & (BitMask - 1));

		goto ContinueWalk;
	}		// case cJL_JPBRANCH_B*
		// Notice the reverse order of the cases, and falling through to the next case,
		// for performance.
	case cJL_JPBRANCH_U:
		Pjp = JL_JBU_PJP(Pjp, Index, cJL_ROOTSTATE);
		// If not a BranchU, traverse; otherwise fall into the next case, which makes
		// this very fast code for a large Judy array (mainly BranchUs), especially
		// when branches are already in the cache, such as for prev/next:
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
		// Note:  BranchU2 is a special case that must continue traversal to a leaf,
		// immed, full, or null type:
		goto ContinueWalk;
	// Note:  Here the calls of JL_DCDNOTMATCHINDEX() are necessary and check
	// whether Index is out of the expanse of a narrow pointer.
	case cJL_JPLEAF1:
		if (JL_DCDNOTMATCHINDEX(Index, Pjp, 1)) break;

		Pop1 = JL_JPLEAF_POP0(Pjp) + 1;
		Pjll = P_JLL(Pjp->jp_Addr);
		if ((posidx = judySearchLeaf1(Pjll, Pop1, Index)) < 0) break;
		return ((PPvoid_t) (JL_LEAF1VALUEAREA(Pjll, Pop1) + posidx));
	case cJL_JPLEAF2:
		if (JL_DCDNOTMATCHINDEX(Index, Pjp, 2)) break;

		Pop1 = JL_JPLEAF_POP0(Pjp) + 1;
		Pjll = P_JLL(Pjp->jp_Addr);

		if ((posidx = judySearchLeaf2(Pjll, Pop1, Index)) < 0) break;
		return ((PPvoid_t) (JL_LEAF2VALUEAREA(Pjll, Pop1) + posidx));
	case cJL_JPLEAF3:
		Pop1 = JL_JPLEAF_POP0(Pjp) + 1;
		Pjll = P_JLL(Pjp->jp_Addr);

		if ((posidx = judySearchLeaf3(Pjll, Pop1, Index)) < 0) break;
		return ((PPvoid_t) (JL_LEAF3VALUEAREA(Pjll, Pop1) + posidx));
	case cJL_JPLEAF_B1:{
		Pjlb_t Pjlb;
		Word_t subexp;	// in bitmap, 0..7.
		BITMAPL_t BitMap;	// for one subexpanse.
		BITMAPL_t BitMask;	// bit in BitMap for Indexs Digit.
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
		assert(Pjv != (Pjv_t) NULL);
		posidx = judyCountBitsL(BitMap & (BitMask - 1));

		return ((PPvoid_t) (Pjv + posidx));
	}		// case cJL_JPLEAF_B1
	// Note that the contents of jp_DcdPopO are different for cJL_JPIMMED_*_01:
	case cJL_JPIMMED_1_01:
	case cJL_JPIMMED_2_01:
	case cJL_JPIMMED_3_01:
		if (JL_JPDCDPOP0(Pjp) != JL_TRIMTODCDSIZE(Index)) break;
		return ((PPvoid_t) & (Pjp->jp_Addr));	// immediate value area.

#define CHECKINDEXNATIVE(LEAF_T, PJP, IDX, INDEX)                       \
if (((LEAF_T *)((PJP)->jp_LIndex))[(IDX) - 1] == (LEAF_T)(INDEX))       \
        return((PPvoid_t)(P_JV((PJP)->jp_Addr) + (IDX) - 1))

#define CHECKLEAFNONNAT(LFBTS, PJP, INDEX, IDX, COPY) {                 \
    Word_t   i_ndex;                                                    \
    uint8_t *a_ddr;                                                     \
    a_ddr  = (PJP)->jp_LIndex + (((IDX) - 1) * (LFBTS));                \
    COPY(i_ndex, a_ddr);                                                \
    if (i_ndex == JL_LEASTBYTES((INDEX), (LFBTS)))                      \
        return((PPvoid_t)(P_JV((PJP)->jp_Addr) + (IDX) - 1));           \
}
	case cJL_JPIMMED_1_03:
		CHECKINDEXNATIVE(uint8_t, Pjp, 3, Index);
	case cJL_JPIMMED_1_02:
		CHECKINDEXNATIVE(uint8_t, Pjp, 2, Index);
		CHECKINDEXNATIVE(uint8_t, Pjp, 1, Index);
		break;
	default:
	      ReturnCorrupt:
		JL_SET_ERRNO(PJError, JL_ERRNO_CORRUPT);
		return (PPJERR);
	}			// switch on JP type

	return ((PPvoid_t) NULL);
}
