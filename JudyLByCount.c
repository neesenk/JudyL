#include "JudyL.h"

extern Word_t judyLJPPop1(const Pjp_t Pjp);

void **JudyLByCount(const void *PArray, uint32_t Count, uint32_t *PIndex)
{
	Word_t Count0, state, pop1, pop1lower, digit, jpcount;
	long jpnum, subexp;
	int offset;

	Pjp_t Pjp;
	Pjll_t Pjll;

	if (PArray == NULL)
		return NULL;
	if (PIndex == NULL) {
		JL_SET_ERRNO(JLE_NULLPINDEX);
		return PPJERR;
	}

	Count0 = Count - 1;
	assert((Count || Count0 == ~0));	/* ensure CPU is sane about 0 - 1. */
	pop1lower = 0;

	if (JL_LEAFW_POP0(PArray) < cJL_LEAFW_MAXPOP1) {
		Pjlw_t Pjlw = P_JLW(PArray);
		if (Count0 > Pjlw[0])
			return NULL;
		*PIndex = Pjlw[Count];
		return (void **)(JL_LEAFWVALUEAREA(Pjlw, Pjlw[0] + 1) + Count0);
	} else {
		Pjpm_t Pjpm = P_JPM(PArray);
		if (Count0 > (Pjpm->jpm_Pop0))
			return NULL;
		Pjp = &(Pjpm->jpm_JP);
		pop1 = (Pjpm->jpm_Pop0) + 1;
	}

#define	PREPB_ROOT(Next) state = cJL_ROOTSTATE;	goto Next
#define	PREPB_DCD(Pjp,cState,Next)			\
	JL_SETDCD(*PIndex, Pjp, cState);	        \
	PREPB((Pjp), cState, Next)
#define	PREPB(Pjp,cState,Next)	\
	state = (cState);	\
	pop1  = JL_JPBRANCH_POP0(Pjp, (cState)) + 1; \
	goto Next
#define	LOWERHALF(Count0,Pop1lower,Pop1exp) \
	(((Count0) - (Pop1lower)) < ((Pop1exp) / 2))
#define	SETOFFSET(Offset,Count0,Pop1lower,Pjp)	\
	(Offset) = (Count0) - (Pop1lower);	\
	assert((Offset) >= 0);			\
	assert((Offset) <= JL_JPLEAF_POP0(Pjp))

      SMByCount:		/* return here for next branch/leaf. */
	switch (JL_JPTYPE(Pjp)) {
	case cJL_JPBRANCH_L2:
		PREPB_DCD(Pjp, 2, BranchL);
	case cJL_JPBRANCH_L3:
		PREPB(Pjp, 3, BranchL);
	case cJL_JPBRANCH_L:
		PREPB_ROOT(BranchL);
	{
		Pjbl_t Pjbl;
		/* Common code (state-independent) for all cases of linear branches: */
	      BranchL:
		Pjbl = P_JBL(Pjp->jp_Addr);
		for (jpnum = 0; jpnum < (Pjbl->jbl_NumJPs); ++jpnum) {
			if ((pop1 = judyLJPPop1((Pjbl->jbl_jp) + jpnum)) == cJL_ALLONES) {
				JL_SET_ERRNO(JLE_CORRUPT);
				return PPJERR;
			}
			assert(pop1 != 0);

			if (pop1lower + pop1 > Count0) {
				JL_SETDIGIT(*PIndex, Pjbl->jbl_Expanse[jpnum], state);
				Pjp = (Pjbl->jbl_jp) + jpnum;
				goto SMByCount;
			}

			pop1lower += pop1;
		}

		JL_SET_ERRNO(JLE_CORRUPT);
		return PPJERR;
	}
	case cJL_JPBRANCH_B2:
		PREPB_DCD(Pjp, 2, BranchB);
	case cJL_JPBRANCH_B3:
		PREPB(Pjp, 3, BranchB);
	case cJL_JPBRANCH_B:
		PREPB_ROOT(BranchB);
	{
		Pjbb_t Pjbb;
        BranchB:
		Pjbb = P_JBB(Pjp->jp_Addr);
#define	BMPJP0(Subexp)	     (P_JP(JL_JBB_PJP(Pjbb, Subexp)))
#define	BMPJP(Subexp,JPnum)  (BMPJP0(Subexp) + (JPnum))
#define	JBB_FOUNDEXPANSE	{				       \
    JL_BITMAPDIGITB(digit, subexp, JL_JBB_BITMAP(Pjbb,subexp), jpnum); \
    JL_SETDIGIT(*PIndex, digit, state);	\
    Pjp = BMPJP(subexp, jpnum);		\
    goto SMByCount;			\
}
		if (LOWERHALF(Count0, pop1lower, pop1)) {
			for (subexp = 0; subexp < cJL_NUMSUBEXPB; ++subexp) {
				if ((jpcount = judyCountBits(JL_JBB_BITMAP(Pjbb, subexp)))
				    && (BMPJP0(subexp) == NULL)) {
					JL_SET_ERRNO(JLE_CORRUPT);
					return PPJERR;
				}
				for (jpnum = 0; jpnum < jpcount; ++jpnum) {
					if ((pop1 = judyLJPPop1(BMPJP(subexp, jpnum))) == cJL_ALLONES) {
						JL_SET_ERRNO(JLE_CORRUPT);
						return PPJERR;
					}
					assert(pop1 != 0);

					if (pop1lower + pop1 > Count0)
						JBB_FOUNDEXPANSE;

					pop1lower += pop1;
				}
			}
		} else {
			pop1lower += pop1;
			for (subexp = cJL_NUMSUBEXPB - 1; subexp >= 0; --subexp) {
				if ((jpcount = judyCountBits(JL_JBB_BITMAP(Pjbb, subexp)))
				    && (BMPJP0(subexp) == NULL)) {
					JL_SET_ERRNO(JLE_CORRUPT);
					return PPJERR;
				}
				for (jpnum = jpcount - 1; jpnum >= 0; --jpnum) {
					if ((pop1 = judyLJPPop1(BMPJP(subexp, jpnum))) == cJL_ALLONES) {
						JL_SET_ERRNO(JLE_CORRUPT);
						return PPJERR;
					}
					assert(pop1 != 0);
					pop1lower -= pop1;
					if ((pop1lower == 0) || (pop1lower - 1 < Count0))
						JBB_FOUNDEXPANSE;
				}
			}
		}
		JL_SET_ERRNO(JLE_CORRUPT);
		return PPJERR;
	}
	case cJL_JPBRANCH_U2:
		PREPB_DCD(Pjp, 2, BranchU);
	case cJL_JPBRANCH_U3:
		PREPB(Pjp, 3, BranchU);
	case cJL_JPBRANCH_U:
		PREPB_ROOT(BranchU);
	{
		Pjbu_t Pjbu;
	/* Common code (state-independent) for all cases of uncompressed branches: */
	BranchU:
		Pjbu = P_JBU(Pjp->jp_Addr);
#define	JBU_FOUNDEXPANSE do {			\
	JL_SETDIGIT(*PIndex, jpnum, state);	\
	Pjp = (Pjbu->jbu_jp) + jpnum;		\
	goto SMByCount;				\
} while (0)
		if (LOWERHALF(Count0, pop1lower, pop1)) {
			for (jpnum = 0; jpnum < cJL_BRANCHUNUMJPS; ++jpnum) {
				if ((Pjbu->jbu_jp[jpnum].jp_Type) <= cJL_JPNULLMAX)
					continue;

				if ((pop1 = judyLJPPop1((Pjbu->jbu_jp) + jpnum)) == cJL_ALLONES) {
					JL_SET_ERRNO(JLE_CORRUPT);
					return PPJERR;
				}
				assert(pop1 != 0);
				if (pop1lower + pop1 > Count0)
					JBU_FOUNDEXPANSE;

				pop1lower += pop1;
			}
		} else {
			pop1lower += pop1;
			for (jpnum = cJL_BRANCHUNUMJPS - 1; jpnum >= 0; --jpnum) {
				if ((Pjbu->jbu_jp[jpnum].jp_Type) <= cJL_JPNULLMAX)
					continue;
				if ((pop1 = judyLJPPop1(Pjbu->jbu_jp + jpnum)) == cJL_ALLONES) {
					JL_SET_ERRNO(JLE_CORRUPT);
					return PPJERR;
				}
				assert(pop1 != 0);
				pop1lower -= pop1;
				if ((pop1lower == 0) || (pop1lower - 1 < Count0))
					JBU_FOUNDEXPANSE;
			}
		}
		JL_SET_ERRNO(JLE_CORRUPT);
		return PPJERR;
	}

#define	PREPL_DCD(cState) JL_SETDCD(*PIndex, Pjp, cState); PREPL
#define	PREPL				\
	Pjll = P_JLL(Pjp->jp_Addr);	\
	pop1 = JL_JPLEAF_POP0(Pjp) + 1;	\
	SETOFFSET(offset, Count0, pop1lower, Pjp)
	case cJL_JPLEAF1:
		PREPL_DCD(1);
		JL_SETDIGIT1(*PIndex, ((uint8_t *) Pjll)[offset]);
		return (void **)(JL_LEAF1VALUEAREA(Pjll, pop1) + offset);
	case cJL_JPLEAF2:
		PREPL_DCD(2);
		*PIndex = (*PIndex & (~JL_LEASTBYTESMASK(2)))
			  | ((uint16_t *) Pjll)[offset];
		return (void **)(JL_LEAF2VALUEAREA(Pjll, pop1) + offset);
	case cJL_JPLEAF3: {
		Word_t lsb;
		PREPL;
		JL_COPY3_PINDEX_TO_LONG(lsb, ((uint8_t *) Pjll) + (3 * offset));
		*PIndex = (*PIndex & (~JL_LEASTBYTESMASK(3))) | lsb;
		return (void **)(JL_LEAF3VALUEAREA(Pjll, pop1) + offset);
	}
	case cJL_JPLEAF_B1:{
		Pjlb_t Pjlb;

		JL_SETDCD(*PIndex, Pjp, 1);
		Pjlb = P_JLB(Pjp->jp_Addr);
		pop1 = JL_JPLEAF_POP0(Pjp) + 1;

		if (LOWERHALF(Count0, pop1lower, pop1)) {
			for (subexp = 0; subexp < cJL_NUMSUBEXPL; ++subexp) {
				pop1 = judyCountBits(JL_JLB_BITMAP(Pjlb, subexp));
				if (pop1lower + pop1 > Count0)
					goto LeafB1;
				pop1lower += pop1;
			}
		} else {
			pop1lower += pop1;
			for (subexp = cJL_NUMSUBEXPL - 1; subexp >= 0; --subexp) {
				pop1lower -= judyCountBits(JL_JLB_BITMAP(Pjlb, subexp));
				if ((pop1lower == 0) || (pop1lower - 1 < Count0))
					goto LeafB1;
			}
		}
		JL_SET_ERRNO(JLE_CORRUPT);
		return PPJERR;
	LeafB1:
		SETOFFSET(offset, Count0, pop1lower, Pjp);
		JL_BITMAPDIGITL(digit, subexp, JL_JLB_BITMAP(Pjlb, subexp),
				offset);
		JL_SETDIGIT1(*PIndex, digit);
		return (void **)(P_JV(JL_JLB_PVALUE(Pjlb, subexp)) + offset);
	}

	case cJL_JPIMMED_1_01:
		JL_SETDIGITS(*PIndex, JL_JPDCDPOP0(Pjp), 1);
		goto Imm_01;
	case cJL_JPIMMED_2_01:
		JL_SETDIGITS(*PIndex, JL_JPDCDPOP0(Pjp), 2);
		goto Imm_01;
	case cJL_JPIMMED_3_01:
		JL_SETDIGITS(*PIndex, JL_JPDCDPOP0(Pjp), 3);
		goto Imm_01;
	Imm_01:
		return (void **)(&Pjp->jp_Addr);
	case cJL_JPIMMED_1_02:
	case cJL_JPIMMED_1_03:
		offset = Count0 - pop1lower;
		JL_SETDIGIT1(*PIndex, ((uint8_t *) Pjp->jp_LIndex)[offset]);
		return (void **)(P_JV(Pjp->jp_Addr) + offset);
	default:
		JL_SET_ERRNO(JLE_CORRUPT);
		return PPJERR;
	}
}
