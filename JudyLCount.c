#include "JudyL.h"

#define cJL_LEAFW       cJL_JPIMMED_CAP
static Word_t judyLCountSM(const Pjp_t Pjp, const Word_t Index, const Pjpm_t Pjpm);
static int judyCountLeafB1(const Pjll_t Pjll, const Word_t Pop1, const Word_t Index);
Word_t judyLJPPop1(const Pjp_t Pjp);

size_t JudyLCount(const void *PArray, uint32_t Index1, uint32_t Index2)
{
	jLpm_t fakejpm;
	Pjpm_t Pjpm;
	jp_t fakejp;
	Pjp_t Pjp;
	Word_t pop1, pop1above1, pop1above2;
	int retcode;
	void **PPvalue;

	if (PArray == NULL || Index1 > Index2)
		return 0;

	if (Index1 == Index2) {
		PPvalue = JudyLGet(PArray, Index1);
		if (PPvalue == PPJERR)
			return 0;

		if (PPvalue == NULL)
			return 0;
		return 1;
	}

	if (JL_LEAFW_POP0(PArray) < cJL_LEAFW_MAXPOP1) {
		Pjlw_t Pjlw = P_JLW(PArray);
		Pjpm = &fakejpm;
		Pjp = &fakejp;
		Pjp->jp_Addr = (Word_t) Pjlw;
		Pjp->jp_Type = cJL_LEAFW;
		Pjpm->jpm_Pop0 = Pjlw[0];
		pop1 = Pjpm->jpm_Pop0 + 1;
	} else {
		Pjpm = P_JPM(PArray);
		Pjp = &(Pjpm->jpm_JP);
		pop1 = (Pjpm->jpm_Pop0) + 1;
		assert(pop1);
	}

	assert(pop1);
	if (Index1 == 0) {
		pop1above1 = pop1;
	} else {
		if ((PPvalue = JudyLFirst(PArray, &Index1)) == PPJERR)
			return 0;

		retcode = (PPvalue != NULL);
		if (retcode == 0)
			return 0;
		if ((pop1above1 = judyLCountSM(Pjp, Index1, Pjpm)) == 0)
			return 0;
	}

	assert(pop1above1);
	if (Index2++ == cJL_ALLONES)
		return pop1above1;
	if ((PPvalue = JudyLFirst(PArray, &Index2)) == PPJERR)
		return 0;
	retcode = (PPvalue != NULL);
	if (retcode == 0)
		return pop1above1;
	if ((pop1above2 = judyLCountSM(Pjp, Index2, Pjpm)) == 0)
		return 0;

	if (pop1above1 == pop1above2)
		return 0;

	return pop1above1 - pop1above2;
}

/**
 * @Pjp		top of Judy (sub)SM.
 * @Index	count at or above this Index.
 * @Pjpm	for returning error info.
 */
static Word_t judyLCountSM(const Pjp_t Pjp, const Word_t Index, const Pjpm_t Pjpm)
{
	Pjbl_t Pjbl;
	Pjbb_t Pjbb;
	Pjbu_t Pjbu;
	Pjll_t Pjll;
	Word_t digit, pop1, pop1above;
	long jpnum;
	int offset;

#define	CHECKDCD(Pjp,cState) \
	assert(! JL_DCDNOTMATCHINDEX(Index, Pjp, cState))
#define	PREPB_ROOT(Pjp,Next)				\
	digit = JL_DIGITATSTATE(Index, cJL_ROOTSTATE);	\
	pop1  = (Pjpm->jpm_Pop0) + 1;			\
	goto Next
#define	PREPB(Pjp,cState,Next)				\
	digit = JL_DIGITATSTATE(Index, cState);		\
	pop1  = JL_JPBRANCH_POP0(Pjp, (cState)) + 1;    \
	goto Next

	switch (JL_JPTYPE(Pjp)) {
	case cJL_LEAFW: {
		Pjlw_t Pjlw = P_JLW(Pjp->jp_Addr);

		assert((Pjpm->jpm_Pop0) + 1 == Pjlw[0] + 1);
		offset = judySearchLeafW(Pjlw + 1, Pjpm->jpm_Pop0 + 1, Index);
		assert(offset >= 0);
		assert(offset < (Pjpm->jpm_Pop0) + 1);
		return ((Pjpm->jpm_Pop0) + 1 - offset);
	}
	case cJL_JPBRANCH_L2:
		CHECKDCD(Pjp, 2);
		PREPB(Pjp, 2, BranchL);
	case cJL_JPBRANCH_L3:
		CHECKDCD(Pjp, 3);
		PREPB(Pjp, 3, BranchL);
	case cJL_JPBRANCH_L:
		PREPB_ROOT(Pjp, BranchL);
	/* Common code (state-independent) for all cases of linear branches: */
	BranchL:
		Pjbl = P_JBL(Pjp->jp_Addr);
		jpnum = Pjbl->jbl_NumJPs;
		pop1above = 0;

		while (digit < (Pjbl->jbl_Expanse[--jpnum])) {
			if ((pop1 = judyLJPPop1((Pjbl->jbl_jp) + jpnum)) == cJL_ALLONES) {
				JL_SET_ERRNO(JLE_CORRUPT);
				return 0;
			}

			pop1above += pop1;
			assert(jpnum > 0);
		}

		assert(digit == (Pjbl->jbl_Expanse[jpnum]));
		pop1 = judyLCountSM((Pjbl->jbl_jp) + jpnum, Index, Pjpm);
		if (pop1 == 0)
			return 0;

		assert(pop1above + pop1);
		return pop1above + pop1;
	case cJL_JPBRANCH_B2:
		CHECKDCD(Pjp, 2);
		PREPB(Pjp, 2, BranchB);
	case cJL_JPBRANCH_B3:
		CHECKDCD(Pjp, 3);
		PREPB(Pjp, 3, BranchB);
	case cJL_JPBRANCH_B:
		PREPB_ROOT(Pjp, BranchB);
	/* Common code (state-independent) for all cases of bitmap branches: */
	BranchB:{
		long subexp, findsub;
		Word_t findbit, lowermask, jpcount, clbelow, clabove;

		Pjbb = P_JBB(Pjp->jp_Addr);
		findsub = digit / cJL_BITSPERSUBEXPB;
		findbit = digit % cJL_BITSPERSUBEXPB;
		lowermask = JL_MASKLOWERINC(JL_BITPOSMASKB(findbit));
		clbelow = clabove = 0;

		assert(JL_BITMAPTESTB(Pjbb, digit));	/* digit must have a JP. */
		assert(findsub < cJL_NUMSUBEXPB);	/* falls in expected range. */

#define	BMPJP0(Subexp)       (P_JP(JL_JBB_PJP(Pjbb, Subexp)))
#define	BMPJP(Subexp,JPnum)  (BMPJP0(Subexp) + (JPnum))
#define	CLPERJPS(jpcount)    ((((jpcount) * cJL_WORDSPERJP) + cJL_WORDSPERCL - 1) / cJL_WORDSPERCL)
		for (subexp = 0; subexp < cJL_NUMSUBEXPB; ++subexp) {
			jpcount = judyCountBits(JL_JBB_BITMAP(Pjbb, subexp));
			if (subexp < findsub)
				clbelow += CLPERJPS(jpcount);
			else if (subexp > findsub)
				clabove += CLPERJPS(jpcount);
			else {
				Word_t clfind;
				clfind = CLPERJPS(judyCountBits(JL_JBB_BITMAP(Pjbb, subexp) &
					      lowermask));

				assert(clfind > 0);	/* digit itself should have 1 CL. */
				clbelow += clfind - 1;
				clabove += CLPERJPS(jpcount) - clfind;
			}
		}
		jpnum = 0;
		if (clbelow < clabove) {
			pop1above = pop1;
			for (subexp = 0; subexp <= findsub; ++subexp) {
				jpcount = judyCountBits((subexp < findsub) ?
							 JL_JBB_BITMAP(Pjbb, subexp) :
							 JL_JBB_BITMAP(Pjbb, subexp) &
							 lowermask);
				assert((subexp < findsub) || jpcount);
				assert(jpcount || (BMPJP0(subexp) == NULL));
				assert((!jpcount) || (BMPJP0(subexp) != NULL));

				for (jpnum = 0; jpnum < jpcount; ++jpnum) {
					if ((pop1 = judyLJPPop1(BMPJP(subexp, jpnum))) == cJL_ALLONES) {
						JL_SET_ERRNO(JLE_CORRUPT);
						return 0;
					}
					pop1above -= pop1;
				}
				jpnum = jpcount - 1;
			}
		} else {
			long jpcountbf;
			pop1above = 0;
			jpcountbf = 0;
			for (subexp = cJL_NUMSUBEXPB - 1; subexp >= findsub; --subexp) {
				jpcount = judyCountBits(JL_JBB_BITMAP(Pjbb, subexp));
				/* should always find findbit: */
				assert((subexp > findsub) || jpcount);
				if (!jpcount)
					continue;
				if (subexp == findsub)
					jpcountbf = judyCountBits(JL_JBB_BITMAP (Pjbb, subexp) & lowermask);
				assert((subexp > findsub) || jpcountbf);
				assert(jpcount >= jpcountbf);
				assert(BMPJP0(subexp) != NULL);
				for (jpnum = jpcount - 1; jpnum >= jpcountbf; --jpnum) {
					if ((pop1 = judyLJPPop1(BMPJP(subexp, jpnum))) == cJL_ALLONES) {
						JL_SET_ERRNO(JLE_CORRUPT);
						return 0;
					}
					pop1above += pop1;
				}
			}
		}

		pop1 = judyLCountSM(BMPJP(findsub, jpnum), Index, Pjpm);
		if (pop1 == 0)
			return 0;
		assert(pop1above + pop1);
		return pop1above + pop1;
	}
	case cJL_JPBRANCH_U2:
		CHECKDCD(Pjp, 2);
		PREPB(Pjp, 2, BranchU);
	case cJL_JPBRANCH_U3:
		CHECKDCD(Pjp, 3);
		PREPB(Pjp, 3, BranchU);
	case cJL_JPBRANCH_U:
		PREPB_ROOT(Pjp, BranchU);
      BranchU:
		Pjbu = P_JBU(Pjp->jp_Addr);
		if (digit < (cJL_BRANCHUNUMJPS / 2)) {
			pop1above = pop1;
			for (jpnum = 0; jpnum <= digit; ++jpnum) {
				if ((Pjbu->jbu_jp[jpnum].jp_Type) <= cJL_JPNULLMAX)
					continue;

				if ((pop1 = judyLJPPop1(Pjbu->jbu_jp + jpnum)) == cJL_ALLONES) {
					JL_SET_ERRNO(JLE_CORRUPT);
					return 0;
				}
				pop1above -= pop1;
			}
		} else {
			assert(digit < cJL_BRANCHUNUMJPS);
			pop1above = 0;

			for (jpnum = cJL_BRANCHUNUMJPS - 1; jpnum > digit; --jpnum) {
				if ((Pjbu->jbu_jp[jpnum].jp_Type) <= cJL_JPNULLMAX)
					continue;

				if ((pop1 = judyLJPPop1(Pjbu->jbu_jp + jpnum)) == cJL_ALLONES) {
					JL_SET_ERRNO(JLE_CORRUPT);
					return 0;
				}

				pop1above += pop1;
			}
		}

		if ((pop1 = judyLCountSM(Pjbu->jbu_jp + digit, Index, Pjpm)) == 0)
			return 0;

		assert(pop1above + pop1);
		return pop1above + pop1;

#define	LEAFLABOVE(Func)				\
	Pjll = P_JLL(Pjp->jp_Addr);			\
	pop1 = JL_JPLEAF_POP0(Pjp) + 1;	                \
	LEAFABOVE(Func, Pjll, pop1)
#define	LEAFABOVE(Func,Pjll,Pop1)		\
	offset = Func(Pjll, Pop1, Index);	\
	assert(offset >= 0);			\
	assert(offset < (Pop1));		\
	return ((Pop1) - offset)
#define	IMMABOVE_01						\
	assert((JL_JPDCDPOP0(Pjp)) == JL_TRIMTODCDSIZE(Index));	\
	return 1
	case cJL_JPLEAF1: LEAFLABOVE(judySearchLeaf1);
	case cJL_JPLEAF2: LEAFLABOVE(judySearchLeaf2);
	case cJL_JPLEAF3: LEAFLABOVE(judySearchLeaf3);
	case cJL_JPLEAF_B1: LEAFLABOVE(judyCountLeafB1);
	case cJL_JPIMMED_1_01: IMMABOVE_01;
	case cJL_JPIMMED_2_01: IMMABOVE_01;
	case cJL_JPIMMED_3_01: IMMABOVE_01;
	case cJL_JPIMMED_1_02: LEAFABOVE(judySearchLeaf1, (Pjll_t)(Pjp->jp_LIndex), 2);
	case cJL_JPIMMED_1_03: LEAFABOVE(judySearchLeaf1, (Pjll_t)(Pjp->jp_LIndex), 3);
	default:
		JL_SET_ERRNO(JLE_CORRUPT);
		return 0;
	}
}

/**
 * @Pjll	bitmap leaf, as Pjll_t for consistency.
 * @Pop1	Population of whole leaf.
 * @Index	to which to count. */
static int judyCountLeafB1(const Pjll_t Pjll, const Word_t Pop1, const Word_t Index)
{
	Pjlb_t Pjlb = (Pjlb_t) Pjll;
	Word_t digit = Index & cJL_MASKATSTATE(1);
	Word_t findsub = digit / cJL_BITSPERSUBEXPL;
	Word_t findbit = digit % cJL_BITSPERSUBEXPL;
	int count;
	long subexp;

	if (findsub < (cJL_NUMSUBEXPL / 2)) {
		count = 0;

		for (subexp = 0; subexp < findsub; ++subexp) {
			count += ((JL_JLB_BITMAP(Pjlb, subexp) == cJL_FULLBITMAPL) ?
				  cJL_BITSPERSUBEXPL : judyCountBits(JL_JLB_BITMAP(Pjlb, subexp)));
		}

		count += judyCountBits(JL_JLB_BITMAP(Pjlb, findsub)
					& JL_MASKLOWERINC(JL_BITPOSMASKL(findbit)));
		assert(count >= 1);
		return count - 1;
	}
	count = Pop1;
	for (subexp = cJL_NUMSUBEXPL - 1; subexp > findsub; --subexp) {
		count -= ((JL_JLB_BITMAP(Pjlb, subexp) == cJL_FULLBITMAPL) ?
			  cJL_BITSPERSUBEXPL : judyCountBits(JL_JLB_BITMAP(Pjlb, subexp)));
	}

	count -= judyCountBits(JL_JLB_BITMAP(Pjlb, findsub)
				&JL_MASKHIGHERINC(JL_BITPOSMASKL(findbit)));
	assert(count >= 0);
	return count;
}

Word_t judyLJPPop1(const Pjp_t Pjp)
{
	switch (JL_JPTYPE(Pjp)) {
	case cJL_JPBRANCH_L2:
	case cJL_JPBRANCH_B2:
	case cJL_JPBRANCH_U2:
		return (JL_JPBRANCH_POP0(Pjp, 2) + 1);
	case cJL_JPBRANCH_L3:
	case cJL_JPBRANCH_B3:
	case cJL_JPBRANCH_U3:
		return (JL_JPBRANCH_POP0(Pjp, 3) + 1);
	case cJL_JPLEAF1:
	case cJL_JPLEAF2:
	case cJL_JPLEAF3:
	case cJL_JPLEAF_B1:
		return (JL_JPLEAF_POP0(Pjp) + 1);
	case cJL_JPIMMED_1_01:
	case cJL_JPIMMED_2_01:
	case cJL_JPIMMED_3_01:
		return 1;
	case cJL_JPIMMED_1_02:
		return 2;
	case cJL_JPIMMED_1_03:
		return 3;
	default:
		return cJL_ALLONES;
	}
}
