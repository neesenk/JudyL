#ifndef JUDYNEXT
#ifndef JUDYPREV
#define	JUDYPREV 1
#endif
#endif

#include "JudyL.h"

#ifdef JUDYPREV
int JudyLPrevEmpty(const void *PArray, uint32_t *PIndex)
#else
int JudyLNextEmpty(const void *PArray, uint32_t *PIndex)
#endif
{
	Word_t Index;
	Pjp_t Pjp;
	Pjbl_t Pjbl;
	Pjbb_t Pjbb;
	Pjbu_t Pjbu;
	Pjlb_t Pjlb;
	PWord_t Pword;
	long offset, subexp;
	BITMAPB_t bitposmaskB;
	BITMAPL_t bitposmaskL;
	Word_t digit, digits, pop0, pop0mask;
	Word_t possfullJP1, possfullJP2, possfullJP3;
#define	JPNULL(Type)  (((Type) >= cJL_JPNULL1) && ((Type) <= cJL_JPNULLMAX))
#define	JPFULL_BRANCH(Pjp) ((((JL_JPDCDPOP0(Pjp) ^ cJL_ALLONES) & pop0mask) == 0) \
	&& ((JL_JPTYPE(Pjp) == possfullJP1) || (JL_JPTYPE(Pjp) == possfullJP2)	  \
	 || (JL_JPTYPE(Pjp) == possfullJP3)))
#define	JPFULL(Pjp) ((digits == 2) ? (JL_JPTYPE(Pjp) == cJL_JPLEAF_B1)	\
	 && (((JL_JPDCDPOP0(Pjp) & cJL_POP0MASK(1)) == cJL_POP0MASK(1))) : \
	 JPFULL_BRANCH(Pjp))
#define	RET_SUCCESS { *PIndex = Index; return 1; }
#define	RET_CORRUPT { JL_SET_ERRNO(JLE_CORRUPT); return JERR; }
#define	SEARCHBITMAPB(Bitmap,Digit,Bitposmask)				\
	(((Bitmap) == cJL_FULLBITMAPB) ? (Digit % cJL_BITSPERSUBEXPB) :	\
	 judyCountBits((Bitmap) & JL_MASKLOWERINC(Bitposmask)) - 1)

#ifdef JUDYPREV
#define	SEARCHBITMAPMAXB(Bitmap) (((Bitmap) == cJL_FULLBITMAPB) ?	\
	cJL_BITSPERSUBEXPB - 1 : judyCountBits(Bitmap) - 1)
#endif
#define	CHECKDCD(cDigits) if (JL_DCDNOTMATCHINDEX(Index, Pjp, cDigits)) RET_SUCCESS
#define	CLEARLEASTDIGITS(Digits) Index &= ~JL_LEASTBYTESMASK(Digits)
#define	SETLEASTDIGITS(  Digits) Index |=  JL_LEASTBYTESMASK(Digits)
#define	CLEARLEASTDIGITS_D(Digit,Digits) do {	\
	CLEARLEASTDIGITS(Digits);		\
	JL_SETDIGIT(Index, Digit, Digits);	\
} while (0)
#define	SETLEASTDIGITS_D(Digit,Digits) do {	\
	SETLEASTDIGITS(Digits);			\
	JL_SETDIGIT(Index, Digit, Digits);	\
} while (0)
#define	SET_AND_RETURN(OpLeastDigits,Digit,Digits) do {	\
	OpLeastDigits(Digit, Digits);			\
	RET_SUCCESS;					\
} while (0)
#define	SET_AND_CONTINUE(OpLeastDigits,Digit,Digits) do {\
	OpLeastDigits(Digit, Digits);			 \
	goto SMGetContinue;				 \
} while (0)
#define	SMPREPB(cDigits,Next,PossFullJP1,PossFullJP2,PossFullJP3)	\
	digits	 = (cDigits);						\
	digit	 = JL_DIGITATSTATE(Index, cDigits);			\
	pop0mask = cJL_POP0MASK((cDigits) - 1);	 /* for branchs JPs */	\
	possfullJP1 = (PossFullJP1);					\
	possfullJP2 = (PossFullJP2);					\
	possfullJP3 = (PossFullJP3);					\
	goto Next
#define	SMPREPB2(Next)				\
	digits	 = 2;				\
	digit	 = JL_DIGITATSTATE(Index, 2);	\
	pop0mask = cJL_POP0MASK(1);  /* for branchs JPs */ \
	possfullJP1 = possfullJP2 = possfullJP3 = 0;	    \
	goto Next
#define	SMPREPB3(Next) SMPREPB(3, Next, cJL_JPBRANCH_L2, cJL_JPBRANCH_B2, cJL_JPBRANCH_U2)
#define	SMPREPBL(Next) SMPREPB(cJL_ROOTSTATE, Next, cJL_JPBRANCH_L3,	\
			       cJL_JPBRANCH_B3, cJL_JPBRANCH_U3)
#ifdef JUDYPREV
#define	SMRESTART(Digits) { CLEARLEASTDIGITS(Digits); goto SMGetRestart; }
#else
#define	SMRESTART(Digits) { SETLEASTDIGITS(  Digits); goto SMGetRestart; }
#endif

#ifdef JUDYPREV
#define	LEAF_EDGE(MinIndex,Digits) do {			\
	if (MinIndex) { --Index; RET_SUCCESS; }		\
	SMRESTART(Digits);				\
} while (0)
#else
#define	LEAF_EDGE(MaxIndex,Digits) do {				\
	if ((MaxIndex) != JL_LEASTBYTES(cJL_ALLONES, Digits))	\
	{ ++Index; RET_SUCCESS; }				\
	SMRESTART(Digits);					\
} while (0)
#endif

#ifdef JUDYPREV
#define	LEAF_EDGE_SET(MinIndex,Digits)	do {				\
	if (MinIndex)							\
	{ JL_SETDIGITS(Index, MinIndex, Digits); --Index; RET_SUCCESS; }\
	SMRESTART(Digits);						\
} while (0)
#else
#define	LEAF_EDGE_SET(MaxIndex,Digits)	do {				\
	if ((MaxIndex) != JL_LEASTBYTES(cJL_ALLONES, Digits))		\
	{ JL_SETDIGITS(Index, MaxIndex, Digits); ++Index; RET_SUCCESS; }\
	SMRESTART(Digits);						\
} while (0)
#endif

#ifdef JUDYPREV
#define	LEAF_HOLE_EVEN(cDigits,Pjll,IndexLSB) do {			\
	while (*(Pjll) > (IndexLSB)) --(Pjll);				\
	if (*(Pjll) < (IndexLSB)) RET_SUCCESS				\
	while (*(--(Pjll)) == --(IndexLSB)) ;				\
	JL_SETDIGITS(Index, IndexLSB, cDigits);				\
	RET_SUCCESS;							\
} while (0)
#else
#define	LEAF_HOLE_EVEN(cDigits,Pjll,IndexLSB) do {			\
	while (*(Pjll) < (IndexLSB)) ++(Pjll);				\
	if (*(Pjll) > (IndexLSB)) RET_SUCCESS				\
	while (*(++(Pjll)) == ++(IndexLSB)) ;				\
	JL_SETDIGITS(Index, IndexLSB, cDigits);				\
	RET_SUCCESS;							\
} while (0)
#endif

#ifdef JUDYPREV
#define	JSLE_EVEN(Addr,Pop0,cDigits,LeafType) do {			\
	LeafType * PjllLSB  = (LeafType *) (Addr);			\
	LeafType   IndexLSB = Index;					\
	if (*PjllLSB >= IndexLSB) {					\
		if (*PjllLSB > IndexLSB) RET_SUCCESS;			\
		LEAF_EDGE(*PjllLSB, cDigits);				\
	}								\
	offset = IndexLSB - *PjllLSB;					\
	if (offset <= (Pop0)) {						\
		PjllLSB += offset;					\
		if (*PjllLSB <= IndexLSB) {				\
		    if (*PjllLSB == IndexLSB)				\
			LEAF_EDGE_SET(PjllLSB[-offset], cDigits);	\
		    RET_CORRUPT;					\
		}							\
		--PjllLSB;						\
	} else PjllLSB = ((LeafType *) (Addr)) + (Pop0);		\
	LEAF_HOLE_EVEN(cDigits, PjllLSB, IndexLSB);			\
} while (0)

#define	JSLE_ODD(cDigits,Pjll,Pop0,Search,Copy)	 do {			\
	Word_t IndexLSB;						\
	Word_t IndexFound;						\
	if ((offset = Search(Pjll, (Pop0) + 1, Index)) < 0) RET_SUCCESS;\
	IndexLSB = JL_LEASTBYTES(Index, cDigits);			\
	offset  *= (cDigits);						\
	while ((offset -= (cDigits)) >= 0) {				\
		Copy(IndexFound, ((uint8_t *) (Pjll)) + offset);	\
		if (IndexFound != (--IndexLSB))				\
		{ JL_SETDIGITS(Index, IndexLSB, cDigits); RET_SUCCESS; }\
	}								\
	LEAF_EDGE_SET(IndexLSB, cDigits);				\
} while (0)
#else // JUDYNEXT
#define	JSLE_EVEN(Addr,Pop0,cDigits,LeafType) do {			\
	LeafType * PjllLSB   = ((LeafType *) (Addr)) + (Pop0);		\
	LeafType   IndexLSB = Index;					\
	if (*PjllLSB <= IndexLSB) {					\
		if (*PjllLSB < IndexLSB) RET_SUCCESS;			\
		LEAF_EDGE(*PjllLSB, cDigits);				\
	}								\
	offset = *PjllLSB - IndexLSB;					\
	if (offset <= (Pop0)) {						\
		PjllLSB -= offset;					\
		if (*PjllLSB >= IndexLSB) {				\
		    if (*PjllLSB == IndexLSB)				\
			LEAF_EDGE_SET(PjllLSB[offset], cDigits);	\
		    RET_CORRUPT;					\
		}							\
		++PjllLSB;						\
	} else PjllLSB = (LeafType *) (Addr);				\
	LEAF_HOLE_EVEN(cDigits, PjllLSB, IndexLSB);			\
} while (0)
#define	JSLE_ODD(cDigits,Pjll,Pop0,Search,Copy)	do {			\
	Word_t IndexLSB;						\
	Word_t IndexFound;						\
	int	   offsetmax;						\
	if ((offset = Search(Pjll, (Pop0) + 1, Index)) < 0)		\
		RET_SUCCESS;						\
	IndexLSB  = JL_LEASTBYTES(Index, cDigits);			\
	offset   *= (cDigits);						\
	offsetmax = (Pop0) * (cDigits);					\
	while ((offset += (cDigits)) <= offsetmax) {			\
		Copy(IndexFound, ((uint8_t *) (Pjll)) + offset);	\
		if (IndexFound != (++IndexLSB))				\
		{ JL_SETDIGITS(Index, IndexLSB, cDigits); RET_SUCCESS; }\
	}								\
	LEAF_EDGE_SET(IndexLSB, cDigits);				\
} while (0)
#endif // JUDYNEXT

#define	judySearchLeafEmpty1(Addr,Pop0) JSLE_EVEN(Addr, Pop0, 1, uint8_t)
#define	judySearchLeafEmpty2(Addr,Pop0) JSLE_EVEN(Addr, Pop0, 2, uint16_t)
#define	judySearchLeafEmptyL(Addr,Pop0) JSLE_EVEN(Addr, Pop0, 4, Word_t )
#define	judySearchLeafEmpty3(Addr,Pop0) \
	JSLE_ODD(3, Addr, Pop0, judySearchLeaf3, JL_COPY3_PINDEX_TO_LONG)

	if (PIndex == NULL) {
		JL_SET_ERRNO(JLE_NULLPINDEX);
		return JERR;
	}
	Index = *PIndex;
      SMGetRestart:
#ifdef JUDYPREV
	if (Index-- == 0) return 0;
#else
	if (++Index == 0) return 0;
#endif
	if (PArray == NULL)
		RET_SUCCESS;
	if (JL_LEAFW_POP0(PArray) < cJL_LEAFW_MAXPOP1) {
		Pjlw_t Pjlw = P_JLW(PArray);
		pop0 = Pjlw[0];
		judySearchLeafEmptyL(Pjlw + 1, pop0);
	} else {
		Pjpm_t Pjpm = P_JPM(PArray);
		Pjp = &(Pjpm->jpm_JP);
	}
      SMGetContinue:
	switch (JL_JPTYPE(Pjp)) {
	case cJL_JPBRANCH_L2:
		CHECKDCD(2);
		SMPREPB2(SMBranchL);
	case cJL_JPBRANCH_L3:
		CHECKDCD(3);
		SMPREPB3(SMBranchL);
	case cJL_JPBRANCH_L:
		SMPREPBL(SMBranchL);
	      SMBranchL:
		Pjbl = P_JBL(Pjp->jp_Addr);
#ifdef JUDYPREV
		if ((Pjbl->jbl_Expanse[0]) > digit) RET_SUCCESS;
		for (offset = (Pjbl->jbl_NumJPs) - 1; /* null */ ; --offset)
#else
		if ((Pjbl->jbl_Expanse[(Pjbl->jbl_NumJPs) - 1]) < digit) RET_SUCCESS;
		for (offset = 0; /* null */ ; ++offset)
#endif
		{
#ifdef JUDYPREV
			if ((Pjbl->jbl_Expanse[offset]) > digit)
				continue;
			if ((Pjbl->jbl_Expanse[offset]) < digit)
				RET_SUCCESS;
#else
			if ((Pjbl->jbl_Expanse[offset]) < digit)
				continue;
			if ((Pjbl->jbl_Expanse[offset]) > digit)
				RET_SUCCESS;
#endif
			if (!JPFULL((Pjbl->jbl_jp) + offset)) {
				Pjp = (Pjbl->jbl_jp) + offset;
				goto SMGetContinue;
			}
#define	BRANCHL_CHECK(OpIncDec,OpLeastDigits,Digit,Digits) do {	\
	if ((Pjbl->jbl_Expanse[offset]) != OpIncDec digit)	\
		SET_AND_RETURN(OpLeastDigits, Digit, Digits);	\
	if (! JPFULL((Pjbl->jbl_jp) + offset)) {		\
		Pjp = (Pjbl->jbl_jp) + offset;			\
		SET_AND_CONTINUE(OpLeastDigits, Digit, Digits);	\
	}							\
} while (0)
#ifdef JUDYPREV
			while (--offset >= 0)
				BRANCHL_CHECK(--, SETLEASTDIGITS_D, digit, digits);
#else
			while (++offset < Pjbl->jbl_NumJPs)
				BRANCHL_CHECK(++, CLEARLEASTDIGITS_D, digit, digits);
#endif
#ifdef JUDYPREV
			if (digit == 0)
				break;
			--digit;
			SET_AND_RETURN(SETLEASTDIGITS_D, digit, digits);
#else
			if (digit == JL_LEASTBYTES(cJL_ALLONES, 1))
				break;
			++digit;
			SET_AND_RETURN(CLEARLEASTDIGITS_D, digit, digits);
#endif
		}
		SMRESTART(digits);
	case cJL_JPBRANCH_B2:
		CHECKDCD(2);
		SMPREPB2(SMBranchB);
	case cJL_JPBRANCH_B3:
		CHECKDCD(3);
		SMPREPB3(SMBranchB);
	case cJL_JPBRANCH_B:
		SMPREPBL(SMBranchB);
	      SMBranchB:
		Pjbb = P_JBB(Pjp->jp_Addr);
		subexp = digit / cJL_BITSPERSUBEXPB;
		assert(subexp < cJL_NUMSUBEXPB);	// falls in expected range.
		bitposmaskB = JL_BITPOSMASKB(digit);

		if (!(JL_JBB_BITMAP(Pjbb, subexp) & bitposmaskB))	// faster.
			RET_SUCCESS;
		offset = SEARCHBITMAPB(JL_JBB_BITMAP(Pjbb, subexp), digit, bitposmaskB);
		assert(offset >= 0);
		assert(offset < (int)cJL_BITSPERSUBEXPB);
		if ((Pjp = P_JP(JL_JBB_PJP(Pjbb, subexp))) == NULL)
			RET_CORRUPT;
		Pjp += offset;
		if (!JPFULL(Pjp))
			goto SMGetContinue;

#define	BRANCHB_CHECKBIT(OpLeastDigits)					\
    if (! (JL_JBB_BITMAP(Pjbb, subexp) & bitposmaskB))  /* absent JP */	\
	SET_AND_RETURN(OpLeastDigits, digit, digits)
#define	BRANCHB_CHECKJPFULL(OpLeastDigits)				\
    if (! JPFULL(Pjp))							\
	SET_AND_CONTINUE(OpLeastDigits, digit, digits)
#define	BRANCHB_STARTSUBEXP(OpLeastDigits)				\
    if (! JL_JBB_BITMAP(Pjbb, subexp)) /* empty subexpanse, shortcut */ \
	SET_AND_RETURN(OpLeastDigits, digit, digits);			\
    if ((Pjp = P_JP(JL_JBB_PJP(Pjbb, subexp))) == NULL) RET_CORRUPT

#ifdef JUDYPREV
		--digit;	// skip initial digit.
		bitposmaskB >>= 1;	// see TBD above.
	      BranchBNextSubexp:	// return here to check next bitmap subexpanse.
		while (bitposmaskB) {	// more bits to check in subexp.
			BRANCHB_CHECKBIT(SETLEASTDIGITS_D);
			--Pjp;	// previous in subarray.
			BRANCHB_CHECKJPFULL(SETLEASTDIGITS_D);
			assert(digit >= 0);
			--digit;
			bitposmaskB >>= 1;
		}

		if (subexp-- > 0) {	// more subexpanses.
			BRANCHB_STARTSUBEXP(SETLEASTDIGITS_D);
			Pjp += SEARCHBITMAPMAXB(JL_JBB_BITMAP(Pjbb, subexp)) + 1;
			bitposmaskB = (1U << (cJL_BITSPERSUBEXPB - 1));
			goto BranchBNextSubexp;
		}
#else // JUDYNEXT
		++digit;	// skip initial digit.
		bitposmaskB <<= 1;	// note:  BITMAPB_t.
	      BranchBNextSubexp:	// return here to check next bitmap subexpanse.
		while (bitposmaskB) {	// more bits to check in subexp.
			BRANCHB_CHECKBIT(CLEARLEASTDIGITS_D);
			++Pjp;	// previous in subarray.
			BRANCHB_CHECKJPFULL(CLEARLEASTDIGITS_D);
			assert(digit < cJL_SUBEXPPERSTATE);
			++digit;
			bitposmaskB <<= 1;	// note:  BITMAPB_t.
		}
		if (++subexp < cJL_NUMSUBEXPB) {	// more subexpanses.
			BRANCHB_STARTSUBEXP(CLEARLEASTDIGITS_D);
			--Pjp;	// pre-decrement.
			bitposmaskB = 1;
			goto BranchBNextSubexp;
		}
#endif // JUDYNEXT
		SMRESTART(digits);
	case cJL_JPBRANCH_U2:
		CHECKDCD(2);
		SMPREPB2(SMBranchU);
	case cJL_JPBRANCH_U3:
		CHECKDCD(3);
		SMPREPB3(SMBranchU);
	case cJL_JPBRANCH_U:
		SMPREPBL(SMBranchU);
	      SMBranchU:
		Pjbu = P_JBU(Pjp->jp_Addr);
		Pjp = (Pjbu->jbu_jp) + digit;
		if (JPNULL(JL_JPTYPE(Pjp)))
			RET_SUCCESS;
		if (!JPFULL(Pjp))
			goto SMGetContinue;
#define	BRANCHU_CHECKJP(OpIncDec,OpLeastDigits) do {		\
	OpIncDec Pjp;						\
	if (JPNULL(JL_JPTYPE(Pjp)))				\
		SET_AND_RETURN(OpLeastDigits, digit, digits);	\
	if (! JPFULL(Pjp))					\
		SET_AND_CONTINUE(OpLeastDigits, digit, digits);	\
} while (0)
#ifdef JUDYPREV
		while (digit-- > 0)
			BRANCHU_CHECKJP(--, SETLEASTDIGITS_D);
#else
		while (++digit < cJL_BRANCHUNUMJPS)
			BRANCHU_CHECKJP(++, CLEARLEASTDIGITS_D);
#endif
		SMRESTART(digits);
#define	SMLEAFL(cDigits,Func)                   \
	Pword = (PWord_t ) P_JLW(Pjp->jp_Addr);  \
	pop0  = JL_JPLEAF_POP0(Pjp);            \
	Func(Pword, pop0)
	case cJL_JPLEAF1:
		CHECKDCD(1);
		SMLEAFL(1, judySearchLeafEmpty1);
	case cJL_JPLEAF2:
		CHECKDCD(2);
		SMLEAFL(2, judySearchLeafEmpty2);
	case cJL_JPLEAF3:
		CHECKDCD(3);
		SMLEAFL(3, judySearchLeafEmpty3);
	case cJL_JPLEAF_B1:
		CHECKDCD(1);
		Pjlb = P_JLB(Pjp->jp_Addr);
		digit = JL_DIGITATSTATE(Index, 1);
		subexp = digit / cJL_BITSPERSUBEXPL;
		bitposmaskL = JL_BITPOSMASKL(digit);
		assert(subexp < cJL_NUMSUBEXPL);	// falls in expected range.
		// Absent index = no index matches current digit in Index:
		if (!(JL_JLB_BITMAP(Pjlb, subexp) & bitposmaskL))	// faster.
			RET_SUCCESS;
#define	LEAFB1_CHECKBIT(OpLeastDigits)				\
	if (! (JL_JLB_BITMAP(Pjlb, subexp) & bitposmaskL))	\
	    SET_AND_RETURN(OpLeastDigits, digit, 1)
#define	LEAFB1_STARTSUBEXP(OpLeastDigits)			\
	if (! JL_JLB_BITMAP(Pjlb, subexp)) /* empty subexp */	\
	    SET_AND_RETURN(OpLeastDigits, digit, 1)
#ifdef JUDYPREV
		--digit;	// skip initial digit.
		bitposmaskL >>= 1;	// see TBD above.
	      LeafB1NextSubexp:	// return here to check next bitmap subexpanse.
		while (bitposmaskL) {	// more bits to check in subexp.
			LEAFB1_CHECKBIT(SETLEASTDIGITS_D);
			assert(digit >= 0);
			--digit;
			bitposmaskL >>= 1;
		}

		if (subexp-- > 0) {	// more subexpanses.
			LEAFB1_STARTSUBEXP(SETLEASTDIGITS_D);
			bitposmaskL = (1UL << (cJL_BITSPERSUBEXPL - 1));
			goto LeafB1NextSubexp;
		}
#else // JUDYNEXT
		++digit;	// skip initial digit.
		bitposmaskL <<= 1;	// note:  BITMAPL_t.
	      LeafB1NextSubexp:	// return here to check next bitmap subexpanse.
		while (bitposmaskL) {	// more bits to check in subexp.
			LEAFB1_CHECKBIT(CLEARLEASTDIGITS_D);
			assert(digit < cJL_SUBEXPPERSTATE);
			++digit;
			bitposmaskL <<= 1;	// note:  BITMAPL_t.
		}

		if (++subexp < cJL_NUMSUBEXPL) {	// more subexpanses.
			LEAFB1_STARTSUBEXP(CLEARLEASTDIGITS_D);
			bitposmaskL = 1;
			goto LeafB1NextSubexp;
		}
#endif // JUDYNEXT
		SMRESTART(1);
	case cJL_JPIMMED_1_01: case cJL_JPIMMED_2_01: case cJL_JPIMMED_3_01:
		if (JL_JPDCDPOP0(Pjp) != JL_TRIMTODCDSIZE(Index))
			RET_SUCCESS;
		digits = JL_JPTYPE(Pjp) - cJL_JPIMMED_1_01 + 1;
		LEAF_EDGE(JL_LEASTBYTES(JL_JPDCDPOP0(Pjp), digits), digits);
#define	IMM_MULTI(Func,BaseJPType)			\
	Pword = (PWord_t ) (Pjp->jp_LIndex);	\
	Func(Pword, JL_JPTYPE(Pjp) - (BaseJPType) + 1)
	case cJL_JPIMMED_1_02: case cJL_JPIMMED_1_03:
		IMM_MULTI(judySearchLeafEmpty1, cJL_JPIMMED_1_02);
	default:
		RET_CORRUPT;
	}
}
