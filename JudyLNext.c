#ifndef JUDYNEXT
#ifndef JUDYPREV
#define	JUDYPREV 1		// neither set => use default.
#endif
#endif

#include "JudyL.h"

#ifdef JUDYPREV
void **JudyLPrev(const void *PArray, uint32_t *PIndex)
#else
void **JudyLNext(const void *PArray, uint32_t *PIndex)
#endif
{
	Pjp_t Pjp, Pjp2;	// current JPs.
	Pjbl_t Pjbl;		// Pjp->jp_Addr masked and cast to types:
	Pjbb_t Pjbb;
	Pjbu_t Pjbu;
	Pjll_t Pjll = NULL;
	Word_t state;		// current state in SM.
	Word_t digit;		// next digit to decode from Index.
#if defined(JUDYPREV)
	Word_t pop1 = 0;	// in a leaf.
#else
	Word_t pop1;		// in a leaf.
#endif
	int offset;		// linear branch/leaf, from judySearchLeaf*().
	int subexp;		// subexpanse in a bitmap branch.
	Word_t bitposmask;	// bit in bitmap for Index.
	Pjp_t APjphist[cJL_ROOTSTATE];	// list of branch JPs traversed.
	int Aoffhist[cJL_ROOTSTATE];	// list of next JP offsets; see above.
	int histnum = 0;	// number of JPs now in list.

#define	HISTPUSH(Pjp,Offset)			\
	APjphist[histnum] = (Pjp);		\
	Aoffhist[histnum] = (Offset);		\
	if (++histnum >= cJL_ROOTSTATE) {	\
	    JL_SET_ERRNO(JL_ERRNO_CORRUPT);	\
	    return PPJERR;			\
	}

#define	HISTPOP(Pjp,Offset)			\
	if ((histnum--) < 1) return NULL;	\
	(Pjp)	 = APjphist[histnum];		\
	(Offset) = Aoffhist[histnum]

#ifdef JUDYPREV
#define	HISTPUSHBOFF(Subexp,Offset,Digit)	  \
	(((Subexp) * cJL_BITSPERSUBEXPB) | (Offset))
#define	HISTPOPBOFF(Subexp,Offset,Digit)	  \
	(Subexp)  = (Offset) / cJL_BITSPERSUBEXPB; \
	(Offset) %= cJL_BITSPERSUBEXPB
#else
#define	HISTPUSHBOFF(Subexp,Offset,Digit)	 \
	 (((Digit) << cJL_BITSPERBYTE)		 \
	| ((Subexp) * cJL_BITSPERSUBEXPB) | (Offset))
#define	HISTPOPBOFF(Subexp,Offset,Digit)	 \
	(Digit)   = (Offset) >> cJL_BITSPERBYTE; \
	(Subexp)  = ((Offset) & JL_LEASTBYTESMASK(1)) / cJL_BITSPERSUBEXPB; \
	(Offset) %= cJL_BITSPERSUBEXPB
#endif
#define	JPNULL(Type)  (((Type) >= cJL_JPNULL1) && ((Type) <= cJL_JPNULLMAX))

#define	SEARCHBITMAPB(Bitmap,Digit,Bitposmask)				\
	(((Bitmap) == cJL_FULLBITMAPB) ? (Digit % cJL_BITSPERSUBEXPB) :	\
	 judyCountBits((Bitmap) & JL_MASKLOWERINC(Bitposmask)) - 1)
#define	SEARCHBITMAPL(Bitmap,Digit,Bitposmask)				\
	(((Bitmap) == cJL_FULLBITMAPL) ? (Digit % cJL_BITSPERSUBEXPL) :	\
	 judyCountBits((Bitmap) & JL_MASKLOWERINC(Bitposmask)) - 1)
#ifdef JUDYPREV
#define	SEARCHBITMAPMAXB(Bitmap)				  \
	(((Bitmap) == cJL_FULLBITMAPB) ? cJL_BITSPERSUBEXPB - 1 : \
	 judyCountBits(Bitmap) - 1)
#define	SEARCHBITMAPMAXL(Bitmap)				  \
	(((Bitmap) == cJL_FULLBITMAPL) ? cJL_BITSPERSUBEXPL - 1 : \
	 judyCountBits(Bitmap) - 1)
#endif

#ifdef JUDYPREV
#define	CDcmp__ <
#else
#define	CDcmp__ >
#endif
#define	CHECKDCD(cState)						\
	if (JL_DCDNOTMATCHINDEX(*PIndex, Pjp, cState)) {                \
	    if ((*PIndex & cJL_DCDMASK(cState))				\
	         CDcmp__(JL_JPDCDPOP0(Pjp) & cJL_DCDMASK(cState))) 	\
		goto SM2Backtrack;					\
	    goto SM3Findlimit;						\
	}

#define	SM1PREPB(cState,Next)				\
	state = (cState);				\
	digit = JL_DIGITATSTATE(*PIndex, cState);	\
	goto Next

#define	SM3PREPB_DCD(cState,Next)			\
	JL_SETDCD(*PIndex, Pjp, cState);	        \
	SM3PREPB(cState,Next)
#define	SM3PREPB(cState,Next)  state = (cState); goto Next

	if (PIndex == NULL) {
		JL_SET_ERRNO(JL_ERRNO_NULLPINDEX);
		return PPJERR;
	}
#ifdef JUDYPREV
	if ((PArray == NULL) || ((*PIndex)-- == 0))
#else
	if ((PArray == NULL) || ((*PIndex)++ == cJL_ALLONES))
#endif
		return NULL;

	if (JL_LEAFW_POP0(PArray) < cJL_LEAFW_MAXPOP1) {
		Pjlw_t Pjlw = P_JLW(PArray);
		pop1 = Pjlw[0] + 1;

		if ((offset = judySearchLeafW(Pjlw + 1, pop1, *PIndex)) >= 0) {	
			assert(offset < pop1);	// in expected range.
			return (void **)(JL_LEAFWVALUEAREA(Pjlw, pop1) + offset);
		}
#ifdef JUDYPREV
		if ((offset = ~offset) == 0)
#else
		if ((offset = ~offset) >= pop1)
#endif
			return NULL;

		assert(offset <= pop1);
#ifdef JUDYPREV
		*PIndex = Pjlw[offset--];
#else
		*PIndex = Pjlw[offset + 1];
#endif
		return (void **)(JL_LEAFWVALUEAREA(Pjlw, pop1) + offset);
	} else {
		Pjpm_t Pjpm = P_JPM(PArray);
		Pjp = &(Pjpm->jpm_JP);
	}

      SM1Get:
	switch (JL_JPTYPE(Pjp)) {
	case cJL_JPBRANCH_L2:
		CHECKDCD(2);
		SM1PREPB(2, SM1BranchL);
	case cJL_JPBRANCH_L3:
		CHECKDCD(3);
		SM1PREPB(3, SM1BranchL);
	case cJL_JPBRANCH_L:
		SM1PREPB(cJL_ROOTSTATE, SM1BranchL);
	SM1BranchL:
		Pjbl = P_JBL(Pjp->jp_Addr);
		if ((offset = judySearchLeaf1((Pjll_t) (Pjbl->jbl_Expanse),
					      Pjbl->jbl_NumJPs, digit)) >= 0) {
			HISTPUSH(Pjp, offset);
			Pjp = (Pjbl->jbl_jp) + offset;
			goto SM1Get;
		}
#ifdef JUDYPREV
		if ((offset = (~offset) - 1) < 0)	// no next-left JP in BranchL.
#else
		if ((offset = (~offset)) >= Pjbl->jbl_NumJPs)	// no next-right.
#endif
			goto SM2Backtrack;
		JL_SETDIGIT(*PIndex, Pjbl->jbl_Expanse[offset], state);
		Pjp = (Pjbl->jbl_jp) + offset;
		goto SM3Findlimit;
	case cJL_JPBRANCH_B2:
		CHECKDCD(2);
		SM1PREPB(2, SM1BranchB);
	case cJL_JPBRANCH_B3:
		CHECKDCD(3);
		SM1PREPB(3, SM1BranchB);
	case cJL_JPBRANCH_B:
		SM1PREPB(cJL_ROOTSTATE, SM1BranchB);
	      SM1BranchB:
		Pjbb = P_JBB(Pjp->jp_Addr);
		subexp = digit / cJL_BITSPERSUBEXPB;

		assert(subexp < cJL_NUMSUBEXPB);	// falls in expected range.
		bitposmask = JL_BITPOSMASKB(digit);
		offset = SEARCHBITMAPB(JL_JBB_BITMAP(Pjbb, subexp), digit, bitposmask);
		assert((offset >= -1) && (offset < (int)cJL_BITSPERSUBEXPB));
		if (JL_JBB_BITMAP(Pjbb, subexp) & bitposmask) {	// faster.
			assert(offset >= 0);
			HISTPUSH(Pjp, HISTPUSHBOFF(subexp, offset, digit));
			if ((Pjp = P_JP(JL_JBB_PJP(Pjbb, subexp))) == NULL) {
				JL_SET_ERRNO(JL_ERRNO_CORRUPT);
				return PPJERR;
			}
			Pjp += offset;
			goto SM1Get;
		}
#ifdef JUDYPREV
		if (offset >= 0)	// next-left JP is in this subexpanse.
			goto SM1BranchBFindlimit;
		while (--subexp >= 0)	// search next-left subexpanses.
#else
		if (JL_JBB_BITMAP(Pjbb, subexp) & JL_MASKHIGHEREXC(bitposmask)) {
			++offset;	// next-left => next-right.
			goto SM1BranchBFindlimit;
		}
		while (++subexp < cJL_NUMSUBEXPB)	// search next-right subexps.
#endif
		{
			if (!JL_JBB_PJP(Pjbb, subexp))
				continue;	// empty subexpanse.
#ifdef JUDYPREV
			offset = SEARCHBITMAPMAXB(JL_JBB_BITMAP(Pjbb, subexp));
			assert((offset >= 0) && (offset < cJL_BITSPERSUBEXPB));
#else
			offset = 0;
#endif
		      SM1BranchBFindlimit:
			JL_BITMAPDIGITB(digit, subexp, JL_JBB_BITMAP(Pjbb, subexp), offset);
			JL_SETDIGIT(*PIndex, digit, state);

			if ((Pjp = P_JP(JL_JBB_PJP(Pjbb, subexp))) == NULL) {
				JL_SET_ERRNO(JL_ERRNO_CORRUPT);
				return PPJERR;
			}

			Pjp += offset;
			goto SM3Findlimit;
		}
		goto SM2Backtrack;
	case cJL_JPBRANCH_U2:
		CHECKDCD(2);
		SM1PREPB(2, SM1BranchU);
	case cJL_JPBRANCH_U3:
		CHECKDCD(3);
		SM1PREPB(3, SM1BranchU);
	case cJL_JPBRANCH_U:
		SM1PREPB(cJL_ROOTSTATE, SM1BranchU);
	      SM1BranchU:
		Pjbu = P_JBU(Pjp->jp_Addr);
		Pjp2 = (Pjbu->jbu_jp) + digit;
		if (!JPNULL(JL_JPTYPE(Pjp2))) {	// digit has a JP.
			HISTPUSH(Pjp, digit);
			Pjp = Pjp2;
			goto SM1Get;
		}
#ifdef JUDYPREV
		while (digit >= 1) {
			Pjp = (Pjbu->jbu_jp) + (--digit);
#else
		while (digit < cJL_BRANCHUNUMJPS - 1) {
			Pjp = (Pjbu->jbu_jp) + (++digit);
#endif
			if (JPNULL(JL_JPTYPE(Pjp)))
				continue;

			JL_SETDIGIT(*PIndex, digit, state);
			goto SM3Findlimit;
		}
		goto SM2Backtrack;
#define	SM1LEAFL(Func)					\
	Pjll   = P_JLL(Pjp->jp_Addr);			\
	pop1   = JL_JPLEAF_POP0(Pjp) + 1;	        \
	offset = Func(Pjll, pop1, *PIndex);		\
	goto SM1LeafLImm

	case cJL_JPLEAF1:
		CHECKDCD(1);
		SM1LEAFL(judySearchLeaf1);
	case cJL_JPLEAF2:
		CHECKDCD(2);
		SM1LEAFL(judySearchLeaf2);
	case cJL_JPLEAF3:
		CHECKDCD(3);
		SM1LEAFL(judySearchLeaf3);
	      SM1LeafLImm:
		if (offset >= 0) {	// *PIndex is in LeafL / Immed.
			switch (JL_JPTYPE(Pjp)) {
			case cJL_JPLEAF1:
				return (void **) (JL_LEAF1VALUEAREA(Pjll, pop1) + offset);
			case cJL_JPLEAF2:
				return (void **) (JL_LEAF2VALUEAREA(Pjll, pop1) + offset);
			case cJL_JPLEAF3:
				return (void **) (JL_LEAF3VALUEAREA(Pjll, pop1) + offset);
			case cJL_JPIMMED_1_01:
			case cJL_JPIMMED_2_01:
			case cJL_JPIMMED_3_01:
				return (void **)(&Pjp->jp_Addr);
			case cJL_JPIMMED_1_02:
			case cJL_JPIMMED_1_03:
				return (void **)(P_JV(Pjp->jp_Addr) + offset);
			}

			JL_SET_ERRNO(JL_ERRNO_CORRUPT);	// impossible?
			return PPJERR;
		}		// found *PIndex
#ifdef JUDYPREV
		if ((offset = (~offset) - 1) < 0)	// no next-left Index.
#else
		if ((offset = (~offset)) >= pop1)	// no next-right Index.
#endif
			goto SM2Backtrack;

		switch (JL_JPTYPE(Pjp)) {
		case cJL_JPLEAF1:
			JL_SETDIGIT1(*PIndex, ((uint8_t *) Pjll)[offset]);
			return (void **) (JL_LEAF1VALUEAREA(Pjll, pop1) + offset);
		case cJL_JPLEAF2:
			*PIndex = (*PIndex & (~JL_LEASTBYTESMASK(2)))
			    | ((uint16_t *) Pjll)[offset];
			return (void **) (JL_LEAF2VALUEAREA(Pjll, pop1) + offset);
		case cJL_JPLEAF3: {
			Word_t lsb;
			JL_COPY3_PINDEX_TO_LONG(lsb, ((uint8_t *) Pjll) + (3 * offset));
			*PIndex = (*PIndex & (~JL_LEASTBYTESMASK(3))) | lsb;
			return (void **) (JL_LEAF3VALUEAREA(Pjll, pop1) + offset);
		}
#define	SET_01(cState)  JL_SETDIGITS(*PIndex, JL_JPDCDPOP0(Pjp), cState)
		case cJL_JPIMMED_1_01:
			SET_01(1);
			goto SM1Imm_01;
		case cJL_JPIMMED_2_01:
			SET_01(2);
			goto SM1Imm_01;
		case cJL_JPIMMED_3_01:
			SET_01(3);
			goto SM1Imm_01;
		      SM1Imm_01:
			return (void **)(&Pjp->jp_Addr);

#define	PJI (Pjp->jp_LIndex)
		case cJL_JPIMMED_1_02:
		case cJL_JPIMMED_1_03:
			JL_SETDIGIT1(*PIndex, ((uint8_t *) PJI)[offset]);
			return (void **)(P_JV(Pjp->jp_Addr) + offset);
		}		// switch for not-found *PIndex
		JL_SET_ERRNO(JL_ERRNO_CORRUPT);	// impossible?
		return PPJERR;
	case cJL_JPLEAF_B1: {
		Pjlb_t Pjlb;
		CHECKDCD(1);

		Pjlb = P_JLB(Pjp->jp_Addr);
		digit = JL_DIGITATSTATE(*PIndex, 1);
		subexp = JL_SUBEXPL(digit);
		bitposmask = JL_BITPOSMASKL(digit);
		assert(subexp < cJL_NUMSUBEXPL);	// falls in expected range.
		if (JL_JLB_BITMAP(Pjlb, subexp) & bitposmask) {	// faster.
			offset = SEARCHBITMAPL(JL_JLB_BITMAP(Pjlb, subexp), digit, bitposmask);
			return (void **)(P_JV(JL_JLB_PVALUE(Pjlb, subexp)) + offset);
		}

		offset = SEARCHBITMAPL(JL_JLB_BITMAP(Pjlb, subexp), digit, bitposmask);
		assert((offset >= -1) && (offset < (int)cJL_BITSPERSUBEXPL));
#ifdef JUDYPREV
		if (offset >= 0)	// next-left JP is in this subexpanse.
			goto SM1LeafB1Findlimit;
		while (--subexp >= 0)	// search next-left subexpanses.
#else
		if (JL_JLB_BITMAP(Pjlb, subexp) & JL_MASKHIGHEREXC(bitposmask)) {
			++offset;	// next-left => next-right.
			goto SM1LeafB1Findlimit;
		}
		while (++subexp < cJL_NUMSUBEXPL)	// search next-right subexps.
#endif
		{
			if (!JL_JLB_BITMAP(Pjlb, subexp))
				continue;	// empty subexp.
#ifdef JUDYPREV
			offset = SEARCHBITMAPMAXL(JL_JLB_BITMAP(Pjlb, subexp));
			// expected range:
			assert((offset >= 0) && (offset < (int)cJL_BITSPERSUBEXPL));
#else
			offset = 0;
#endif
		SM1LeafB1Findlimit:
			JL_BITMAPDIGITL(digit, subexp, JL_JLB_BITMAP(Pjlb, subexp), offset);
			JL_SETDIGIT1(*PIndex, digit);
			return (void **)(P_JV(JL_JLB_PVALUE(Pjlb, subexp)) + offset);
		}
		goto SM2Backtrack;
	}
#ifdef JUDYPREV
#define	SM1IMM_SETPOP1(cPop1)
#else
#define SM1IMM_SETPOP1(cPop1)  pop1 = (cPop1)
#endif
#define	SM1IMM(Func,cPop1)				\
	SM1IMM_SETPOP1(cPop1);				\
	offset = Func((Pjll_t) (PJI), cPop1, *PIndex);	\
	goto SM1LeafLImm

#ifdef JUDYPREV
#define	SM1IMM_01_SETPOP1
#else
#define SM1IMM_01_SETPOP1  pop1 = 1
#endif
#define	SM1IMM_01							  \
	SM1IMM_01_SETPOP1;						  \
	offset = ((JL_JPDCDPOP0(Pjp) <  JL_TRIMTODCDSIZE(*PIndex)) ? ~1 : \
		  (JL_JPDCDPOP0(Pjp) == JL_TRIMTODCDSIZE(*PIndex)) ?  0 : ~0); \
	goto SM1LeafLImm

	case cJL_JPIMMED_1_01:
	case cJL_JPIMMED_2_01:
	case cJL_JPIMMED_3_01:
		SM1IMM_01;
	case cJL_JPIMMED_1_02:
		SM1IMM(judySearchLeaf1, 2);
	case cJL_JPIMMED_1_03:
		SM1IMM(judySearchLeaf1, 3);
	default:
		JL_SET_ERRNO(JL_ERRNO_CORRUPT);
		return PPJERR;

	}

      SM2Backtrack:
	HISTPOP(Pjp, offset);
	switch (JL_JPTYPE(Pjp)) {
	case cJL_JPBRANCH_L2:
		state = 2;
		goto SM2BranchL;
	case cJL_JPBRANCH_L3:
		state = 3;
		goto SM2BranchL;
	case cJL_JPBRANCH_L:
		state = cJL_ROOTSTATE;
		goto SM2BranchL;
	SM2BranchL:
#ifdef JUDYPREV
		if (--offset < 0)
			goto SM2Backtrack;	// no next-left JP in BranchL.
#endif
		Pjbl = P_JBL(Pjp->jp_Addr);
#ifdef JUDYNEXT
		if (++offset >= (Pjbl->jbl_NumJPs))
			goto SM2Backtrack;
#endif
		JL_SETDIGIT(*PIndex, Pjbl->jbl_Expanse[offset], state);
		Pjp = (Pjbl->jbl_jp) + offset;
		goto SM3Findlimit;
	case cJL_JPBRANCH_B2:
		state = 2;
		goto SM2BranchB;
	case cJL_JPBRANCH_B3:
		state = 3;
		goto SM2BranchB;
	case cJL_JPBRANCH_B:
		state = cJL_ROOTSTATE;
		goto SM2BranchB;
	      SM2BranchB:
		Pjbb = P_JBB(Pjp->jp_Addr);
		HISTPOPBOFF(subexp, offset, digit);	// unpack values.
#ifdef JUDYPREV
		if (offset > 0) {	// next-left JP is in this subexpanse.
			--offset;
			goto SM2BranchBFindlimit;
		}
		while (--subexp >= 0)	// search next-left subexpanses.
#else
		if (JL_JBB_BITMAP(Pjbb, subexp) & JL_MASKHIGHEREXC(JL_BITPOSMASKB(digit))) {
			++offset;
			goto SM2BranchBFindlimit;
		}
		while (++subexp < cJL_NUMSUBEXPB)
#endif
		{
			if (!JL_JBB_PJP(Pjbb, subexp))
				continue;	// empty subexpanse.
#ifdef JUDYPREV
			offset = SEARCHBITMAPMAXB(JL_JBB_BITMAP(Pjbb, subexp));
			assert((offset >= 0) && (offset < cJL_BITSPERSUBEXPB));
#else
			offset = 0;
#endif
		SM2BranchBFindlimit:
			JL_BITMAPDIGITB(digit, subexp, JL_JBB_BITMAP(Pjbb, subexp), offset);
			JL_SETDIGIT(*PIndex, digit, state);
			if ((Pjp = P_JP(JL_JBB_PJP(Pjbb, subexp))) == NULL) {
				JL_SET_ERRNO(JL_ERRNO_CORRUPT);
				return PPJERR;
			}

			Pjp += offset;
			goto SM3Findlimit;
		}
		goto SM2Backtrack;
	case cJL_JPBRANCH_U2:
		state = 2;
		goto SM2BranchU;
	case cJL_JPBRANCH_U3:
		state = 3;
		goto SM2BranchU;
	case cJL_JPBRANCH_U:
		state = cJL_ROOTSTATE;
		goto SM2BranchU;
	      SM2BranchU:
		Pjbu = P_JBU(Pjp->jp_Addr);
		digit = offset;
#ifdef JUDYPREV
		while (digit >= 1) {
			Pjp = (Pjbu->jbu_jp) + (--digit);
#else
		while (digit < cJL_BRANCHUNUMJPS - 1) {
			Pjp = (Pjbu->jbu_jp) + (++digit);
#endif
			if (JPNULL(JL_JPTYPE(Pjp)))
				continue;
			JL_SETDIGIT(*PIndex, digit, state);
			goto SM3Findlimit;
		}
		goto SM2Backtrack;
	default:
		JL_SET_ERRNO(JL_ERRNO_CORRUPT);
		return PPJERR;
	}

      SM3Findlimit:
	switch (JL_JPTYPE(Pjp)) {
	case cJL_JPBRANCH_L2:
		SM3PREPB_DCD(2, SM3BranchL);
	case cJL_JPBRANCH_L3:
		SM3PREPB(3, SM3BranchL);
	case cJL_JPBRANCH_L:
		SM3PREPB(cJL_ROOTSTATE, SM3BranchL);
	      SM3BranchL:
		Pjbl = P_JBL(Pjp->jp_Addr);
#ifdef JUDYPREV
		if ((offset = (Pjbl->jbl_NumJPs) - 1) < 0)
#else
		offset = 0;
		if ((Pjbl->jbl_NumJPs) == 0)
#endif
		{
			JL_SET_ERRNO(JL_ERRNO_CORRUPT);
			return PPJERR;
		}

		JL_SETDIGIT(*PIndex, Pjbl->jbl_Expanse[offset], state);
		Pjp = (Pjbl->jbl_jp) + offset;
		goto SM3Findlimit;

	case cJL_JPBRANCH_B2:
		SM3PREPB_DCD(2, SM3BranchB);
	case cJL_JPBRANCH_B3:
		SM3PREPB(3, SM3BranchB);
	case cJL_JPBRANCH_B:
		SM3PREPB(cJL_ROOTSTATE, SM3BranchB);
	      SM3BranchB:
		Pjbb = P_JBB(Pjp->jp_Addr);
#ifdef JUDYPREV
		subexp = cJL_NUMSUBEXPB;
		while (!(JL_JBB_BITMAP(Pjbb, --subexp))) {	// find non-empty subexp.
			if (subexp <= 0) {	// wholly empty bitmap.
				JL_SET_ERRNO(JL_ERRNO_CORRUPT);
				return PPJERR;
			}
		}

		offset = SEARCHBITMAPMAXB(JL_JBB_BITMAP(Pjbb, subexp));
		assert((offset >= 0) && (offset < cJL_BITSPERSUBEXPB));
#else
		subexp = -1;
		while (!(JL_JBB_BITMAP(Pjbb, ++subexp))) {	// find non-empty subexp.
			if (subexp >= cJL_NUMSUBEXPB - 1) {	// didnt find one.
				JL_SET_ERRNO(JL_ERRNO_CORRUPT);
				return PPJERR;
			}
		}
		offset = 0;
#endif
		JL_BITMAPDIGITB(digit, subexp, JL_JBB_BITMAP(Pjbb, subexp), offset);
		JL_SETDIGIT(*PIndex, digit, state);
		if ((Pjp = P_JP(JL_JBB_PJP(Pjbb, subexp))) == NULL) {
			JL_SET_ERRNO(JL_ERRNO_CORRUPT);
			return PPJERR;
		}

		Pjp += offset;
		goto SM3Findlimit;

	case cJL_JPBRANCH_U2:
		SM3PREPB_DCD(2, SM3BranchU);
	case cJL_JPBRANCH_U3:
		SM3PREPB(3, SM3BranchU);
	case cJL_JPBRANCH_U:
		SM3PREPB(cJL_ROOTSTATE, SM3BranchU);
	      SM3BranchU:
		Pjbu = P_JBU(Pjp->jp_Addr);
#ifdef JUDYPREV
		digit = cJL_BRANCHUNUMJPS;
		while (digit >= 1) {
			Pjp = (Pjbu->jbu_jp) + (--digit);
#else
		for (digit = 0; digit < cJL_BRANCHUNUMJPS; ++digit) {
			Pjp = (Pjbu->jbu_jp) + digit;
#endif
			if (JPNULL(JL_JPTYPE(Pjp)))
				continue;
			JL_SETDIGIT(*PIndex, digit, state);
			goto SM3Findlimit;
		}
		JL_SET_ERRNO(JL_ERRNO_CORRUPT);
		return PPJERR;

#define	SM3LEAFLDCD(cState)				\
	JL_SETDCD(*PIndex, Pjp, cState);	        \
	SM3LEAFLNODCD

#define	SM3LEAFL_SETPOP1  pop1 = JL_JPLEAF_POP0(Pjp) + 1

#ifdef JUDYPREV
#define	SM3LEAFLNODCD			\
	Pjll = P_JLL(Pjp->jp_Addr);	\
	SM3LEAFL_SETPOP1;		\
	offset = JL_JPLEAF_POP0(Pjp); assert(offset >= 0)
#else
#define	SM3LEAFLNODCD			\
	Pjll = P_JLL(Pjp->jp_Addr);	\
	SM3LEAFL_SETPOP1;		\
	offset = 0; assert(JL_JPLEAF_POP0(Pjp) >= 0);
#endif
	case cJL_JPLEAF1:
		SM3LEAFLDCD(1);
		JL_SETDIGIT1(*PIndex, ((uint8_t *) Pjll)[offset]);
		return (void **) (JL_LEAF1VALUEAREA(Pjll, pop1) + offset);
	case cJL_JPLEAF2:
		SM3LEAFLDCD(2);
		*PIndex = (*PIndex & (~JL_LEASTBYTESMASK(2))) | ((uint16_t *) Pjll)[offset];
		return (void **) (JL_LEAF2VALUEAREA(Pjll, pop1) + offset);
	case cJL_JPLEAF3: {
			Word_t lsb;
			SM3LEAFLNODCD;
			JL_COPY3_PINDEX_TO_LONG(lsb, ((uint8_t *) Pjll) + (3 * offset));
			*PIndex = (*PIndex & (~JL_LEASTBYTESMASK(3))) | lsb;
			return (void **) (JL_LEAF3VALUEAREA(Pjll, pop1) + offset);
		}

	case cJL_JPLEAF_B1: {
		Pjlb_t Pjlb;
		JL_SETDCD(*PIndex, Pjp, 1);
		Pjlb = P_JLB(Pjp->jp_Addr);
#ifdef JUDYPREV
		subexp = cJL_NUMSUBEXPL;
		while (!JL_JLB_BITMAP(Pjlb, --subexp)) {	// find non-empty subexp.
			if (subexp <= 0) {	// wholly empty bitmap.
				JL_SET_ERRNO(JL_ERRNO_CORRUPT);
				return PPJERR;
			}
		}
		offset = SEARCHBITMAPMAXL(JL_JLB_BITMAP(Pjlb, subexp));
		assert((offset >= 0) && (offset < cJL_BITSPERSUBEXPL));
#else
		subexp = -1;
		while (!JL_JLB_BITMAP(Pjlb, ++subexp)) {	// find non-empty subexp.
			if (subexp >= cJL_NUMSUBEXPL - 1) {	// didnt find one.
				JL_SET_ERRNO(JL_ERRNO_CORRUPT);
				return PPJERR;
			}
		}
		offset = 0;
#endif
		JL_BITMAPDIGITL(digit, subexp, JL_JLB_BITMAP(Pjlb, subexp), offset);
		JL_SETDIGIT1(*PIndex, digit);
	}

	case cJL_JPIMMED_1_01:
		SET_01(1);
		goto SM3Imm_01;
	case cJL_JPIMMED_2_01:
		SET_01(2);
		goto SM3Imm_01;
	case cJL_JPIMMED_3_01:
		SET_01(3);
		goto SM3Imm_01;
      SM3Imm_01:
		return (void **)(&Pjp->jp_Addr);
#ifdef JUDYPREV
#define	SM3IMM_OFFSET(cPop1)  (cPop1) - 1	// highest.
#else
#define	SM3IMM_OFFSET(cPop1)  0	// lowest.
#endif
#define	SM3IMM(cPop1,Next)		\
	offset = SM3IMM_OFFSET(cPop1);	\
	goto Next
	case cJL_JPIMMED_1_02:
		SM3IMM(2, SM3Imm1);
	case cJL_JPIMMED_1_03:
		SM3IMM(3, SM3Imm1);
	      SM3Imm1:
		JL_SETDIGIT1(*PIndex, ((uint8_t *) PJI)[offset]);
		return (void **)(P_JV(Pjp->jp_Addr) + offset);
	default:
		JL_SET_ERRNO(JL_ERRNO_CORRUPT);
		return PPJERR;
	}
}
