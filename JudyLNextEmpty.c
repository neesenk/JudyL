#ifndef JUDYNEXT
#ifndef JUDYPREV
#define	JUDYPREV 1		// neither set => use default.
#endif
#endif

#include "JudyL.h"

#ifdef JUDYPREV
int JudyLPrevEmpty(const void *PArray, uint32_t *PIndex)
#else
int JudyLNextEmpty(const void *PArray, uint32_t *PIndex)
#endif
{
	Word_t Index;		// fast copy, in a register.
	Pjp_t Pjp;		// current JP.
	Pjbl_t Pjbl;		// Pjp->jp_Addr masked and cast to types:
	Pjbb_t Pjbb;
	Pjbu_t Pjbu;
	Pjlb_t Pjlb;
	PWord_t Pword;		// alternate name for use by GET* macros.

	Word_t digit;		// next digit to decode from Index.
	Word_t digits;		// current state in SM = digits left to decode.
	Word_t pop0;		// in a leaf.
	Word_t pop0mask;	// precalculated to avoid variable shifts.
	long offset;		// within a branch or leaf (can be large).
	int subexp;		// subexpanse in a bitmap branch.
	BITMAPB_t bitposmaskB;	// bit in bitmap for bitmap branch.
	BITMAPL_t bitposmaskL;	// bit in bitmap for bitmap leaf.
	Word_t possfullJP1;	// JP types for possibly full subexpanses:
	Word_t possfullJP2;
	Word_t possfullJP3;
#define	JPNULL(Type)  (((Type) >= cJL_JPNULL1) && ((Type) <= cJL_JPNULLMAX))
#define	JPFULL_BRANCH(Pjp)						\
	  ((((JL_JPDCDPOP0(Pjp) ^ cJL_ALLONES) & pop0mask) == 0)	\
	&& ((JL_JPTYPE(Pjp) == possfullJP1)				\
	 || (JL_JPTYPE(Pjp) == possfullJP2)				\
	 || (JL_JPTYPE(Pjp) == possfullJP3)))
#define	JPFULL(Pjp)							\
	((digits == 2) ?						\
	   (JL_JPTYPE(Pjp) == cJL_JPLEAF_B1)				\
	 && (((JL_JPDCDPOP0(Pjp) & cJL_POP0MASK(1)) == cJL_POP0MASK(1))) : \
	 JPFULL_BRANCH(Pjp))
#define	RET_SUCCESS { *PIndex = Index; return 1; }
#define	RET_CORRUPT { JL_SET_ERRNO(JL_ERRNO_CORRUPT); return JERR; }
#define	SEARCHBITMAPB(Bitmap,Digit,Bitposmask)				\
	(((Bitmap) == cJL_FULLBITMAPB) ? (Digit % cJL_BITSPERSUBEXPB) :	\
	 judyCountBits((Bitmap) & JL_MASKLOWERINC(Bitposmask)) - 1)

#ifdef JUDYPREV
#define	SEARCHBITMAPMAXB(Bitmap)					\
	(((Bitmap) == cJL_FULLBITMAPB) ? cJL_BITSPERSUBEXPB - 1 :	\
	 judyCountBits(Bitmap) - 1)
#endif
#define	CHECKDCD(cDigits) \
	if (JL_DCDNOTMATCHINDEX(Index, Pjp, cDigits)) RET_SUCCESS
#define	CLEARLEASTDIGITS(Digits) Index &= ~JL_LEASTBYTESMASK(Digits)
#define	SETLEASTDIGITS(  Digits) Index |=  JL_LEASTBYTESMASK(Digits)
#define	CLEARLEASTDIGITS_D(Digit,Digits)	\
	{					\
	    CLEARLEASTDIGITS(Digits);		\
	    JL_SETDIGIT(Index, Digit, Digits);	\
	}
#define	SETLEASTDIGITS_D(Digit,Digits)		\
	{					\
	    SETLEASTDIGITS(Digits);		\
	    JL_SETDIGIT(Index, Digit, Digits);	\
	}
#define	SET_AND_RETURN(OpLeastDigits,Digit,Digits)	\
	{						\
	    OpLeastDigits(Digit, Digits);		\
	    RET_SUCCESS;				\
	}
#define	SET_AND_CONTINUE(OpLeastDigits,Digit,Digits)	\
	{						\
	    OpLeastDigits(Digit, Digits);		\
	    goto SMGetContinue;				\
	}
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
#define	LEAF_EDGE(MinIndex,Digits)			\
	{						\
	    if (MinIndex) { --Index; RET_SUCCESS; }	\
	    SMRESTART(Digits);				\
	}
#else
#define	LEAF_EDGE(MaxIndex,Digits)			\
	{						\
	    if ((MaxIndex) != JL_LEASTBYTES(cJL_ALLONES, Digits)) \
	    { ++Index; RET_SUCCESS; }			\
	    SMRESTART(Digits);				\
	}
#endif

#ifdef JUDYPREV
#define	LEAF_EDGE_SET(MinIndex,Digits)	\
	{				\
	    if (MinIndex)		\
	    { JL_SETDIGITS(Index, MinIndex, Digits); --Index; RET_SUCCESS; } \
	    SMRESTART(Digits);		\
	}
#else
#define	LEAF_EDGE_SET(MaxIndex,Digits)	\
	{				\
	    if ((MaxIndex) != JL_LEASTBYTES(cJL_ALLONES, Digits))	    \
	    { JL_SETDIGITS(Index, MaxIndex, Digits); ++Index; RET_SUCCESS; } \
	    SMRESTART(Digits);		\
	}
#endif

#ifdef JUDYPREV
#define	LEAF_HOLE_EVEN(cDigits,Pjll,IndexLSB)				\
	{								\
	    while (*(Pjll) > (IndexLSB)) --(Pjll); /* too high */	\
	    if (*(Pjll) < (IndexLSB)) RET_SUCCESS  /* Index is empty */	\
	    while (*(--(Pjll)) == --(IndexLSB)) /* null, find a hole */;\
	    JL_SETDIGITS(Index, IndexLSB, cDigits);			\
	    RET_SUCCESS;						\
	}
#else
#define	LEAF_HOLE_EVEN(cDigits,Pjll,IndexLSB)				\
	{								\
	    while (*(Pjll) < (IndexLSB)) ++(Pjll); /* too low */	\
	    if (*(Pjll) > (IndexLSB)) RET_SUCCESS  /* Index is empty */	\
	    while (*(++(Pjll)) == ++(IndexLSB)) /* null, find a hole */;\
	    JL_SETDIGITS(Index, IndexLSB, cDigits);			\
	    RET_SUCCESS;						\
	}
#endif
// SEARCH FOR AN EMPTY INDEX IN AN IMMEDIATE OR LEAF:
// Given a pointer to the first index in a leaf (or equivalently an immediate
// JP), the population of the leaf, and a first empty Index to find (inclusive,
// as Index in the context), where Index is known to fall within the expanse of
// the leaf to search, efficiently find the previous/next empty index in the
// leaf, if any.  For simplicity the following overview is stated in terms of
// Judy*NextEmpty() only, but the same concepts apply symmetrically for
// Judy*PrevEmpty().  Also, in each case the comparisons are for the LSBs of
// Index and leaf indexes, according to the leafs level.
// 1.  If Index is GREATER than the last (highest) index in the leaf
//     (maxindex), return success, Index is empty.  (Remember, Index is known
//     to be in the leafs expanse.)
// 2.  If Index is EQUAL to maxindex:  If maxindex is not at the edge of the
//     leafs expanse, increment Index and return success, there is an empty
//     Index one higher than any in the leaf; otherwise restart with Index
//     reset to the upper edge of the leafs expanse.  Note:  This might cause
//     an extra cache line fill, but this is OK for repeatedly-called search
//     code, and it saves CPU time.
// 3.  If Index is LESS than maxindex, check for "dense to end of leaf":
//     Subtract Index from maxindex, and back up that many slots in the leaf.
//     If the resulting offset is not before the start of the leaf then compare
//     the index at this offset (baseindex) with Index:
// 3a.  If GREATER, the leaf must be corrupt, since indexes are sorted and
//      there are no duplicates.
// 3b.  If EQUAL, the leaf is "dense" from Index to maxindex, meaning there is
//      no reason to search it.  "Slide right" to the high end of the leaf
//      (modify Index to maxindex) and continue with step 2 above.
// 3c.  If LESS, continue with step 4.
// 4.  If the offset based on maxindex minus Index falls BEFORE the start of
//     the leaf, or if, per 3c above, baseindex is LESS than Index, the leaf is
//     guaranteed "not dense to the end" and a usable empty Index must exist.
//     This supports a more efficient search loop.  Start at the FIRST index in
//     the leaf, or one BEYOND baseindex, respectively, and search the leaf as
//     follows, comparing each current index (currindex) with Index:
// 4a.  If LESS, keep going to next index.  Note:  This is certain to terminate
//      because maxindex is known to be greater than Index, hence the loop can
//      be small and fast.
// 4b.  If EQUAL, loop and increment Index until finding currindex greater than
//      Index, and return success with the modified Index.
// 4c.  If GREATER, return success, Index (unmodified) is empty.
// Note:  These are macros rather than functions for speed.
#ifdef JUDYPREV
#define	JSLE_EVEN(Addr,Pop0,cDigits,LeafType)				\
	{								\
	    LeafType * PjllLSB  = (LeafType *) (Addr);			\
	    LeafType   IndexLSB = Index;	/* auto-masking */	\
	/* Index before or at start of leaf: */				\
	    if (*PjllLSB >= IndexLSB)		/* no need to search */	\
	    {								\
		if (*PjllLSB > IndexLSB) RET_SUCCESS; /* Index empty */	\
		LEAF_EDGE(*PjllLSB, cDigits);				\
	    }								\
	/* Index in or after leaf: */					\
	    offset = IndexLSB - *PjllLSB;	/* tentative offset  */	\
	    if (offset <= (Pop0))		/* can check density */	\
	    {								\
		PjllLSB += offset;		/* move to slot */	\
									\
		if (*PjllLSB <= IndexLSB)	/* dense or corrupt */	\
		{							\
		    if (*PjllLSB == IndexLSB)	/* dense, check edge */	\
			LEAF_EDGE_SET(PjllLSB[-offset], cDigits);	\
		    RET_CORRUPT;					\
		}							\
		--PjllLSB;	/* not dense, start at previous */	\
	    }								\
	    else PjllLSB = ((LeafType *) (Addr)) + (Pop0); /* start at max */ \
	    LEAF_HOLE_EVEN(cDigits, PjllLSB, IndexLSB);			\
	}

#define	JSLE_ODD(cDigits,Pjll,Pop0,Search,Copy)				\
	{								\
	    Word_t IndexLSB;		/* least bytes only */		\
	    Word_t IndexFound;		/* in leaf	    */		\
	    if ((offset = Search(Pjll, (Pop0) + 1, Index)) < 0)		\
		RET_SUCCESS;		/* Index is empty */		\
	    IndexLSB = JL_LEASTBYTES(Index, cDigits);			\
	    offset  *= (cDigits);					\
	    while ((offset -= (cDigits)) >= 0)				\
	    {				/* skip until empty or start */	\
		Copy(IndexFound, ((uint8_t *) (Pjll)) + offset);	\
		if (IndexFound != (--IndexLSB))	/* found an empty */	\
		{ JL_SETDIGITS(Index, IndexLSB, cDigits); RET_SUCCESS; }\
	    }								\
	    LEAF_EDGE_SET(IndexLSB, cDigits);				\
	}
#else // JUDYNEXT
#define	JSLE_EVEN(Addr,Pop0,cDigits,LeafType)				\
	{								\
	    LeafType * PjllLSB   = ((LeafType *) (Addr)) + (Pop0);	\
	    LeafType   IndexLSB = Index;	/* auto-masking */	\
	/* Index at or after end of leaf: */				\
	    if (*PjllLSB <= IndexLSB)		/* no need to search */	\
	    {								\
		if (*PjllLSB < IndexLSB) RET_SUCCESS;  /* Index empty */\
		LEAF_EDGE(*PjllLSB, cDigits);				\
	    }								\
	/* Index before or in leaf: */					\
	    offset = *PjllLSB - IndexLSB;	/* tentative offset  */	\
	    if (offset <= (Pop0))		/* can check density */	\
	    {								\
		PjllLSB -= offset;		/* move to slot */	\
									\
		if (*PjllLSB >= IndexLSB)	/* dense or corrupt */	\
		{							\
		    if (*PjllLSB == IndexLSB)	/* dense, check edge */	\
			LEAF_EDGE_SET(PjllLSB[offset], cDigits);	\
		    RET_CORRUPT;					\
		}							\
		++PjllLSB;		/* not dense, start at next */	\
	    }								\
	    else PjllLSB = (LeafType *) (Addr);	/* start at minimum */	\
									\
	    LEAF_HOLE_EVEN(cDigits, PjllLSB, IndexLSB);			\
	}
#define	JSLE_ODD(cDigits,Pjll,Pop0,Search,Copy)				\
	{								\
	    Word_t IndexLSB;		/* least bytes only */		\
	    Word_t IndexFound;		/* in leaf	    */		\
	    int	   offsetmax;		/* in bytes	    */		\
									\
	    if ((offset = Search(Pjll, (Pop0) + 1, Index)) < 0)		\
		RET_SUCCESS;			/* Index is empty */	\
									\
	    IndexLSB  = JL_LEASTBYTES(Index, cDigits);			\
	    offset   *= (cDigits);					\
	    offsetmax = (Pop0) * (cDigits);	/* single multiply */	\
									\
	    while ((offset += (cDigits)) <= offsetmax)			\
	    {				/* skip until empty or end */	\
		Copy(IndexFound, ((uint8_t *) (Pjll)) + offset);	\
		if (IndexFound != (++IndexLSB))	/* found an empty */	\
		{ JL_SETDIGITS(Index, IndexLSB, cDigits); RET_SUCCESS; } \
	    }								\
	    LEAF_EDGE_SET(IndexLSB, cDigits);				\
	}
#endif // JUDYNEXT

#define	judySearchLeafEmpty1(Addr,Pop0) \
	JSLE_EVEN(Addr, Pop0, 1, uint8_t)
#define	judySearchLeafEmpty2(Addr,Pop0) \
	JSLE_EVEN(Addr, Pop0, 2, uint16_t)
#define	judySearchLeafEmpty3(Addr,Pop0) \
	JSLE_ODD(3, Addr, Pop0, judySearchLeaf3, JL_COPY3_PINDEX_TO_LONG)
#define	judySearchLeafEmptyL(Addr,Pop0) \
	JSLE_EVEN(Addr, Pop0, 4, Word_t )

	if (PIndex == NULL) {
		JL_SET_ERRNO(JL_ERRNO_NULLPINDEX);
		return JERR;
	}
	Index = *PIndex;
      SMGetRestart:
#ifdef JUDYPREV
	if (Index-- == 0)
		return 0;
#else
	if (++Index == 0)
		return 0;
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
		if ((Pjbl->jbl_Expanse[0]) > digit)
			RET_SUCCESS;
		for (offset = (Pjbl->jbl_NumJPs) - 1; /* null */ ; --offset)
#else
		if ((Pjbl->jbl_Expanse[(Pjbl->jbl_NumJPs) - 1]) < digit)
			RET_SUCCESS;
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
#define	BRANCHL_CHECK(OpIncDec,OpLeastDigits,Digit,Digits)	\
	{							\
	    if ((Pjbl->jbl_Expanse[offset]) != OpIncDec digit)	\
		SET_AND_RETURN(OpLeastDigits, Digit, Digits);	\
	    if (! JPFULL((Pjbl->jbl_jp) + offset))		\
	    {							\
		Pjp = (Pjbl->jbl_jp) + offset;			\
		SET_AND_CONTINUE(OpLeastDigits, Digit, Digits);	\
	    }							\
	}
#ifdef JUDYPREV
			while (--offset >= 0)
				BRANCHL_CHECK(--, SETLEASTDIGITS_D, digit, digits)
#else
			while (++offset < Pjbl->jbl_NumJPs)
				BRANCHL_CHECK(++, CLEARLEASTDIGITS_D, digit, digits)
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
	SET_AND_RETURN(OpLeastDigits, digit, digits)			\
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
#define	BRANCHU_CHECKJP(OpIncDec,OpLeastDigits)			\
	{							\
	    OpIncDec Pjp;					\
	    if (JPNULL(JL_JPTYPE(Pjp)))				\
		SET_AND_RETURN(OpLeastDigits, digit, digits)	\
	    if (! JPFULL(Pjp))					\
		SET_AND_CONTINUE(OpLeastDigits, digit, digits)	\
	}
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
	case cJL_JPIMMED_1_01:
	case cJL_JPIMMED_2_01:
	case cJL_JPIMMED_3_01:
		if (JL_JPDCDPOP0(Pjp) != JL_TRIMTODCDSIZE(Index))
			RET_SUCCESS;
		digits = JL_JPTYPE(Pjp) - cJL_JPIMMED_1_01 + 1;
		LEAF_EDGE(JL_LEASTBYTES(JL_JPDCDPOP0(Pjp), digits), digits);
// Immediate JPs with Pop1 > 1:
#define	IMM_MULTI(Func,BaseJPType)			\
	Pword = (PWord_t ) (Pjp->jp_LIndex);	\
	Func(Pword, JL_JPTYPE(Pjp) - (BaseJPType) + 1)
	case cJL_JPIMMED_1_02:
	case cJL_JPIMMED_1_03:
		IMM_MULTI(judySearchLeafEmpty1, cJL_JPIMMED_1_02);
	default:
		RET_CORRUPT;
	}
}
