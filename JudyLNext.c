#ifndef JUDYNEXT
#ifndef JUDYPREV
#define	JUDYPREV 1		// neither set => use default.
#endif
#endif
#include "JudyL.h"

/* OVERVIEW OF Judy*Prev():
 * Use a reentrant switch statement (state machine, SM1 = "get") to decode the
 * callers *PIndex-1, starting with the (PArray), through branches, if
 * any, down to an immediate or a leaf.  Look for *PIndex-1 in that leaf, and
 * if found, return it.
 *
 * A dead end is either a branch that does not contain a JP for the appropriate
 * digit in *PIndex-1, or a leaf that does not contain the undecoded digits of
 * *PIndex-1.  Upon reaching a dead end, backtrack through the leaf/branches
 * that were just traversed, using a list (history) of parent JPs that is built
 * while going forward in SM1Get.  Start with the current leaf or branch.  In a
 * backtracked leaf, look for an Index less than *PIndex-1.  In each
 * backtracked branch, look "sideways" for the next JP, if any, lower than the
 * one for the digit (from *PIndex-1) that was previously decoded.  While
 * backtracking, if a leaf has no previous Index or a branch has no lower JP,
 * go to its parent branch in turn.  Upon reaching the JRP, return failure, "no
 * previous Index".  The backtrack process is sufficiently different from
 * SM1Get to merit its own separate reentrant switch statement (SM2 =
 * "backtrack").
 *
 * While backtracking, upon finding a lower JP in a branch, there is certain to
 * be a "prev" Index under that JP (unless the Judy array is corrupt).
 * Traverse forward again, this time taking the last (highest, right-most) JP
 * in each branch, and the last (highest) Index upon reaching an immediate or a
 * leaf.  This traversal is sufficiently different from SM1Get and SM2Backtrack
 * to merit its own separate reentrant switch statement (SM3 = "findlimit").
 *
 * "Decode" bytes in JPs complicate this process a little.  In SM1Get, when a
 * JP is a narrow pointer, that is, when states are skipped (so the skipped
 * digits are stored in jp_DcdPopO), compare the relevant digits to the same
 * digits in *PIndex-1.  If they are EQUAL, proceed in SM1Get as before.  If
 * jp_DcdPopOs digits are GREATER, treat the JP as a dead end and proceed in
 * SM2Backtrack.  If jp_DcdPopOs digits are LESS, treat the JP as if it had
 * just been found during a backtrack and proceed directly in SM3Findlimit.
 *
 * Note that Decode bytes can be ignored in SM3Findlimit; they dont matter.
 * Also note that in practice the Decode bytes are routinely compared with
 * *PIndex-1 because thats simpler and no slower than first testing for
 * narrowness.
 *
 * Decode bytes also make it unnecessary to construct the Index to return (the
 * revised *PIndex) during the search.  This step is deferred until finding an
 * Index during backtrack or findlimit, before returning it.  The first digit
 * of *PIndex is derived (saved) based on which JP is used in a JRP branch.
 * The remaining digits are obtained from the jp_DcdPopO field in the JP (if
 * any) above the immediate or leaf containing the found (prev) Index, plus the
 * remaining digit(s) in the immediate or leaf itself.  In the case of a LEAFW,
 * the Index to return is found directly in the leaf.
 *
 * Note:  Theoretically, as described above, upon reaching a dead end, SM1Get
 * passes control to SM2Backtrack to look sideways, even in a leaf.  Actually
 * its a little more efficient for the SM1Get leaf cases to shortcut this and
 * take care of the sideways searches themselves.  Hence the history list only
 * contains branch JPs, and SM2Backtrack only handles branches.  In fact, even
 * the branch handling cases in SM1Get do some shortcutting (sideways
 * searching) to avoid pushing history and calling SM2Backtrack unnecessarily.
 *
 * Upon reaching an Index to return after backtracking, *PIndex must be
 * modified to the found Index.  In principle this could be done by building
 * the Index from a saved rootdigit (in the top branch) plus the Dcd bytes from
 * the parent JP plus the appropriate Index bytes from the leaf.  However,
 * Immediates are difficult because their parent JPs lack one (last) digit.  So
 * instead just build the *PIndex to return "top down" while backtracking and
 * findlimiting.
 *
 * This function is written iteratively for speed, rather than recursively.
 *
 * CAVEATS:
 * Why use a backtrack list (history stack), since it has finite size?  The
 * size is small for Judy on both 32-bit and 64-bit systems, and a list (really
 * just an array) is fast to maintain and use.  Other alternatives include
 * doing a lookahead (lookaside) in each branch while traversing forward
 * (decoding), and restarting from the top upon a dead end.
 *
 * A lookahead means noting the last branch traversed which contained a
 * non-null JP lower than the one specified by a digit in *PIndex-1, and
 * returning to that point for SM3Findlimit.  This seems like a good idea, and
 * should be pretty cheap for linear and bitmap branches, but it could result
 * in up to 31 unnecessary additional cache line fills (in extreme cases) for
 * every uncompressed branch traversed.  We have considered means of attaching
 * to or hiding within an uncompressed branch (in null JPs) a "cache line map"
 * or other structure, such as an offset to the next non-null JP, that would
 * speed this up, but it seems unnecessary merely to avoid having a
 * finite-length list (array).  (If JudySL is ever made "native", the finite
 * list length will be an issue.)
 *
 * Restarting at the top of the Judy array after a dead end requires a careful
 * modification of *PIndex-1 to decrement the digit for the parent branch and
 * set the remaining lower digits to all 1s.  This must be repeated each time a
 * parent branch contains another dead end, so even though it should all happen
 * in cache, the CPU time can be excessive.  (For JudySL or an equivalent
 * "infinitely deep" Judy array, consider a hybrid of a large, finite,
 * "circular" list and a restart-at-top when the list is backtracked to
 * exhaustion.)
 *
 * Why search for *PIndex-1 instead of *PIndex during SM1Get?  In rare
 * instances this prevents an unnecessary decode down the wrong path followed
 * by a backtrack; its pretty cheap to set up initially; and it means the
 * SM1Get machine can simply return if/when it finds that Index.
 *
 * VARIATIONS FOR Judy*Next():
 *
 * The Judy*Next() code is nearly a perfect mirror of the Judy*Prev() code.
 * See the Judy*Prev() overview comments, and mentally switch the following:
 *
 * - "*PIndex-1"  => "*PIndex+1"
 * - "less than"  => "greater than"
 * - "lower"      => "higher"
 * - "lowest"     => "highest"
 * - "next-left"  => "next-right"
 * - "right-most" => "left-most"
 *
 * Note:  SM3Findlimit could be called SM3Findmax/SM3Findmin, but a common name
 * for both Prev and Next means many fewer ifdefs in this code. */

#ifdef JUDYPREV
PPvoid_t JudyLPrev(Pcvoid_t PArray, Word_t * PIndex, PJError_t PJError)
#else
PPvoid_t JudyLNext(Pcvoid_t PArray, Word_t * PIndex, PJError_t PJError)
#endif
{
	Pjp_t Pjp, Pjp2;	// current JPs.
	Pjbl_t Pjbl;		// Pjp->jp_Addr masked and cast to types:
	Pjbb_t Pjbb;
	Pjbu_t Pjbu;
	Pjll_t Pjll = (Pjll_t) NULL;
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
#define	HISTNUMMAX cJL_ROOTSTATE	// maximum branches traversable.
	Pjp_t APjphist[HISTNUMMAX];	// list of branch JPs traversed.
	int Aoffhist[HISTNUMMAX];	// list of next JP offsets; see above.
	int histnum = 0;	// number of JPs now in list.

#define	HISTPUSH(Pjp,Offset)			\
	APjphist[histnum] = (Pjp);		\
	Aoffhist[histnum] = (Offset);		\
						\
	if (++histnum >= HISTNUMMAX)		\
	{					\
	    JL_SET_ERRNO(PJError, JL_ERRNO_CORRUPT) \
	    return(PPJERR);		\
	}

#define	HISTPOP(Pjp,Offset)			\
	if ((histnum--) < 1) JL_RET_NOTFOUND;	\
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
	 judyCountBitsB((Bitmap) & JL_MASKLOWERINC(Bitposmask)) - 1)
#define	SEARCHBITMAPL(Bitmap,Digit,Bitposmask)				\
	(((Bitmap) == cJL_FULLBITMAPL) ? (Digit % cJL_BITSPERSUBEXPL) :	\
	 judyCountBitsL((Bitmap) & JL_MASKLOWERINC(Bitposmask)) - 1)
#ifdef JUDYPREV
#define	SEARCHBITMAPMAXB(Bitmap)				  \
	(((Bitmap) == cJL_FULLBITMAPB) ? cJL_BITSPERSUBEXPB - 1 : \
	 judyCountBitsB(Bitmap) - 1)
#define	SEARCHBITMAPMAXL(Bitmap)				  \
	(((Bitmap) == cJL_FULLBITMAPL) ? cJL_BITSPERSUBEXPL - 1 : \
	 judyCountBitsL(Bitmap) - 1)
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

	if (PIndex == (PWord_t) NULL) {
		JL_SET_ERRNO(PJError, JL_ERRNO_NULLPINDEX);
		return (PPJERR);
	}
#ifdef JUDYPREV
	if ((PArray == (Pvoid_t) NULL) || ((*PIndex)-- == 0))
#else
	if ((PArray == (Pvoid_t) NULL) || ((*PIndex)++ == cJL_ALLONES))
#endif
		JL_RET_NOTFOUND;

	if (JL_LEAFW_POP0(PArray) < cJL_LEAFW_MAXPOP1) {	// must be a LEAFW
		Pjlw_t Pjlw = P_JLW(PArray);	// first word of leaf.
		pop1 = Pjlw[0] + 1;

		if ((offset = judySearchLeafW(Pjlw + 1, pop1, *PIndex)) >= 0) {	// Index is present.
			assert(offset < pop1);	// in expected range.
			JL_RET_FOUND_LEAFW(Pjlw, pop1, offset);	// *PIndex is set.
		}
#ifdef JUDYPREV
		if ((offset = ~offset) == 0)
#else
		if ((offset = ~offset) >= pop1)
#endif
			JL_RET_NOTFOUND;
		assert(offset <= pop1);
#ifdef JUDYPREV
		*PIndex = Pjlw[offset--];
#else
		*PIndex = Pjlw[offset + 1];
#endif
		JL_RET_FOUND_LEAFW(Pjlw, pop1, offset);	
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
			if ((Pjp = P_JP(JL_JBB_PJP(Pjbb, subexp))) == (Pjp_t) NULL) {
				JL_SET_ERRNO(PJError, JL_ERRNO_CORRUPT);
				return (PPJERR);
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

			if ((Pjp = P_JP(JL_JBB_PJP(Pjbb, subexp))) == (Pjp_t) NULL) {
				JL_SET_ERRNO(PJError, JL_ERRNO_CORRUPT);
				return (PPJERR);
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
				JL_RET_FOUND_LEAF1(Pjll, pop1, offset);
			case cJL_JPLEAF2:
				JL_RET_FOUND_LEAF2(Pjll, pop1, offset);
			case cJL_JPLEAF3:
				JL_RET_FOUND_LEAF3(Pjll, pop1, offset);
			case cJL_JPIMMED_1_01:
			case cJL_JPIMMED_2_01:
			case cJL_JPIMMED_3_01:
				JL_RET_FOUND_IMM_01(Pjp);
			case cJL_JPIMMED_1_02:
			case cJL_JPIMMED_1_03:
				JL_RET_FOUND_IMM(Pjp, offset);
			}

			JL_SET_ERRNO(PJError, JL_ERRNO_CORRUPT);	// impossible?
			return (PPJERR);
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
			JL_RET_FOUND_LEAF1(Pjll, pop1, offset);
		case cJL_JPLEAF2:
			*PIndex = (*PIndex & (~JL_LEASTBYTESMASK(2)))
			    | ((uint16_t *) Pjll)[offset];
			JL_RET_FOUND_LEAF2(Pjll, pop1, offset);
		case cJL_JPLEAF3: {
			Word_t lsb;
			JL_COPY3_PINDEX_TO_LONG(lsb, ((uint8_t *) Pjll) + (3 * offset));
			*PIndex = (*PIndex & (~JL_LEASTBYTESMASK(3))) | lsb;
			JL_RET_FOUND_LEAF3(Pjll, pop1, offset);
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
		      SM1Imm_01:JL_RET_FOUND_IMM_01(Pjp);

#define	PJI (Pjp->jp_LIndex)
		case cJL_JPIMMED_1_02:
		case cJL_JPIMMED_1_03:
			JL_SETDIGIT1(*PIndex, ((uint8_t *) PJI)[offset]);
			JL_RET_FOUND_IMM(Pjp, offset);
		}		// switch for not-found *PIndex
		JL_SET_ERRNO(PJError, JL_ERRNO_CORRUPT);	// impossible?
		return (PPJERR);
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
			JL_RET_FOUND_LEAF_B1(Pjlb, subexp, offset);
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
			JL_RET_FOUND_LEAF_B1(Pjlb, subexp, offset);
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
		JL_SET_ERRNO(PJError, JL_ERRNO_CORRUPT);
		return (PPJERR);

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
			if ((Pjp = P_JP(JL_JBB_PJP(Pjbb, subexp))) == (Pjp_t) NULL) {
				JL_SET_ERRNO(PJError, JL_ERRNO_CORRUPT);
				return (PPJERR);
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
		JL_SET_ERRNO(PJError, JL_ERRNO_CORRUPT);
		return (PPJERR);
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
			JL_SET_ERRNO(PJError, JL_ERRNO_CORRUPT);
			return (PPJERR);
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
				JL_SET_ERRNO(PJError, JL_ERRNO_CORRUPT);
				return (PPJERR);
			}
		}

		offset = SEARCHBITMAPMAXB(JL_JBB_BITMAP(Pjbb, subexp));
		assert((offset >= 0) && (offset < cJL_BITSPERSUBEXPB));
#else
		subexp = -1;
		while (!(JL_JBB_BITMAP(Pjbb, ++subexp))) {	// find non-empty subexp.
			if (subexp >= cJL_NUMSUBEXPB - 1) {	// didnt find one.
				JL_SET_ERRNO(PJError, JL_ERRNO_CORRUPT);
				return (PPJERR);
			}
		}
		offset = 0;
#endif
		JL_BITMAPDIGITB(digit, subexp, JL_JBB_BITMAP(Pjbb, subexp), offset);
		JL_SETDIGIT(*PIndex, digit, state);
		if ((Pjp = P_JP(JL_JBB_PJP(Pjbb, subexp))) == (Pjp_t) NULL) {
			JL_SET_ERRNO(PJError, JL_ERRNO_CORRUPT);
			return (PPJERR);
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
		JL_SET_ERRNO(PJError, JL_ERRNO_CORRUPT);
		return (PPJERR);

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
		JL_RET_FOUND_LEAF1(Pjll, pop1, offset);
	case cJL_JPLEAF2:
		SM3LEAFLDCD(2);
		*PIndex = (*PIndex & (~JL_LEASTBYTESMASK(2))) | ((uint16_t *) Pjll)[offset];
		JL_RET_FOUND_LEAF2(Pjll, pop1, offset);
	case cJL_JPLEAF3: {
			Word_t lsb;
			SM3LEAFLNODCD;
			JL_COPY3_PINDEX_TO_LONG(lsb, ((uint8_t *) Pjll) + (3 * offset));
			*PIndex = (*PIndex & (~JL_LEASTBYTESMASK(3))) | lsb;
			JL_RET_FOUND_LEAF3(Pjll, pop1, offset);
		}

	case cJL_JPLEAF_B1: {
		Pjlb_t Pjlb;
		JL_SETDCD(*PIndex, Pjp, 1);
		Pjlb = P_JLB(Pjp->jp_Addr);
#ifdef JUDYPREV
		subexp = cJL_NUMSUBEXPL;
		while (!JL_JLB_BITMAP(Pjlb, --subexp)) {	// find non-empty subexp.
			if (subexp <= 0) {	// wholly empty bitmap.
				JL_SET_ERRNO(PJError, JL_ERRNO_CORRUPT);
				return (PPJERR);
			}
		}
		offset = SEARCHBITMAPMAXL(JL_JLB_BITMAP(Pjlb, subexp));
		assert((offset >= 0) && (offset < cJL_BITSPERSUBEXPL));
#else
		subexp = -1;
		while (!JL_JLB_BITMAP(Pjlb, ++subexp)) {	// find non-empty subexp.
			if (subexp >= cJL_NUMSUBEXPL - 1) {	// didnt find one.
				JL_SET_ERRNO(PJError, JL_ERRNO_CORRUPT);
				return (PPJERR);
			}
		}
		offset = 0;
#endif
		JL_BITMAPDIGITL(digit, subexp, JL_JLB_BITMAP(Pjlb, subexp), offset);
		JL_SETDIGIT1(*PIndex, digit);
		JL_RET_FOUND_LEAF_B1(Pjlb, subexp, offset);
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
		JL_RET_FOUND_IMM_01(Pjp);
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
	      SM3Imm1:JL_SETDIGIT1(*PIndex, ((uint8_t *) PJI)[offset]);
		JL_RET_FOUND_IMM(Pjp, offset);
	default:
		JL_SET_ERRNO(PJError, JL_ERRNO_CORRUPT);
		return (PPJERR);
	}
}
