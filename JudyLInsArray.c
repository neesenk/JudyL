#include "JudyL.h"

static uint8_t immed_maxpop1[] = {
	0, cJL_IMMED1_MAXPOP1, cJL_IMMED2_MAXPOP1, cJL_IMMED3_MAXPOP1,
};
static uint8_t leaf_maxpop1[] = { 0, cJL_LEAF1_MAXPOP1, cJL_LEAF2_MAXPOP1, cJL_LEAF3_MAXPOP1 };
static uint8_t branchL_JPtype[] = { 0, 0, cJL_JPBRANCH_L2, cJL_JPBRANCH_L3, cJL_JPBRANCH_L };
static uint8_t branchB_JPtype[] = { 0, 0, cJL_JPBRANCH_B2, cJL_JPBRANCH_B3, cJL_JPBRANCH_B };
static uint8_t branchU_JPtype[] = { 0, 0, cJL_JPBRANCH_U2, cJL_JPBRANCH_U3, cJL_JPBRANCH_U };
static Word_t subexp_mask[] = { 0, ~cJL_POP0MASK(1), ~cJL_POP0MASK(2), ~cJL_POP0MASK(3) };

/**
 * @PjpParent	parent JP in/under which to store.
 * @Level	initial digits remaining to decode.
 * @PPop1	number of indexes to store.
 * @PIndex	list of indexes to store.
 * @PValue	list of corresponding values.
 * @Pjpm	for memory and errors.
 */
static int judyInsArray(Pjp_t PjpParent, int Level, PWord_t PPop1,
			const uint32_t *PIndex, Pjv_t PValue, Pjpm_t Pjpm)
{
	Pjp_t	Pjp;
	jp_t	JPnull;
	Pjbu_t	PjbuRaw, Pjbu;
	Pjv_t	PjvRaw, Pjv;
	Word_t	Pjbany, pop1sub, digitmask, digitshifted, digitshincr;
	Word_t	pop1 = *PPop1;
	int	digit, levelsub, offset, numJPs, retval;
	uint8_t JPtype, JPtype_null;

#define SAMESUBEXP(L_evel) (!((PIndex[0] ^ PIndex[pop1 - 1]) & subexp_mask[L_evel]))
#define SETJPNULL_PARENT JL_JPSETADT(PjpParent, 0, 0, cJL_JPNULL1 + Level - 1);
#define SETJPNULL(Pjp) *(Pjp) = JPnull
#define NOMEM do { SETJPNULL_PARENT; *PPop1 = 0; return 0; } while (0)
#define ALLOCLEAF(AllocLeaf) \
        if ((PjllRaw = AllocLeaf(pop1, Pjpm)) == NULL) NOMEM; \
        Pjll = P_JLL(PjllRaw);

#define COPYTOLEAF_EVEN_SUB(Pjll,LeafType) do {         \
        LeafType * P_leaf  = (LeafType *) (Pjll);	\
        Word_t p_op1   = pop1;				\
        const uint32_t *P_Index = PIndex;		\
        assert(pop1 > 0);				\
        do { *P_leaf++ = *P_Index++; /* truncates */	\
        } while (--(p_op1));				\
} while (0)

#define COPYTOLEAF_ODD_SUB(cLevel,Pjll,Copy) do {       \
        uint8_t * P_leaf  = (uint8_t *) (Pjll);		\
        Word_t p_op1   = pop1;				\
        const uint32_t *P_Index = PIndex;		\
        assert(pop1 > 0);				\
        do {						\
		Copy(P_leaf, *P_Index);                 \
                P_leaf += (cLevel); ++P_Index;          \
        } while (--(p_op1));				\
} while (0)

#define COPYTOLEAF_EVEN(Pjll,LeafType) do {             \
        COPYTOLEAF_EVEN_SUB(Pjll,LeafType);		\
        JL_COPYMEM(Pjv, PValue, pop1);			\
} while (0)

#define COPYTOLEAF_ODD(cLevel,Pjll,Copy) do {		\
        COPYTOLEAF_ODD_SUB(cLevel,Pjll,Copy);		\
        JL_COPYMEM(Pjv, PValue, pop1);			\
} while (0)

#define SETIMMTYPE(BaseJPType)  (PjpParent->jp_Type) = (BaseJPType) + pop1 - 2

#define MAKELEAF_SUB1(AllocLeaf,ValueArea,LeafType)     \
        ALLOCLEAF(AllocLeaf);                           \
        Pjv = ValueArea(Pjll, pop1)

#define MAKELEAF_SUB2(cLevel,JPType) do {				\
        Word_t D_cdP0;                                                  \
        assert(pop1 - 1 <= cJL_POP0MASK(cLevel));                       \
        D_cdP0 = (*PIndex & cJL_DCDMASK(cLevel)) | (pop1 - 1);          \
        JL_JPSETADT(PjpParent, (Word_t)PjllRaw, D_cdP0, JPType);        \
} while (0)

#define MAKELEAF_EVEN(cLevel,JPType,AllocLeaf,ValueArea,LeafType)       \
        MAKELEAF_SUB1(AllocLeaf,ValueArea,LeafType);                    \
        COPYTOLEAF_EVEN(Pjll, LeafType);                                \
        MAKELEAF_SUB2(cLevel, JPType)

#define MAKELEAF_ODD(cLevel,JPType,AllocLeaf,ValueArea,Copy)            \
        MAKELEAF_SUB1(AllocLeaf,ValueArea,LeafType);                    \
        COPYTOLEAF_ODD(cLevel, Pjll, Copy);                             \
        MAKELEAF_SUB2(cLevel, JPType)

#define CHECKLEAFORDER do {                                             \
        for (offset = 1; offset < pop1; ++offset) {			\
		if (PIndex[offset - 1] >= PIndex[offset]) {             \
			SETJPNULL_PARENT;                               \
			*PPop1 = 0;                                     \
			JL_SET_ERRNO(JLE_UNSORTED);		\
			return 0;					\
                }                                                       \
        }	                                                        \
} while (0)

	assert(Level >= 1);
	assert(Level <= cJL_ROOTSTATE);
	assert((Level < cJL_ROOTSTATE) || (pop1 > cJL_LEAFW_MAXPOP1));

	if (Level == cJL_ROOTSTATE) {
		levelsub = cJL_ROOTSTATE;
		goto BuildBranch2;
	}
	assert(Level < cJL_ROOTSTATE);
	assert(pop1 > 1);

	if (pop1 <= immed_maxpop1[Level]) {
		uint8_t *Pjll = (uint8_t *) (PjpParent->jp_LIndex);

		CHECKLEAFORDER;	/* indexes to be stored are sorted. */

		if ((PjvRaw = judyLAllocJV(pop1, Pjpm)) == NULL)
			NOMEM;
		(PjpParent->jp_Addr) = (Word_t) PjvRaw;
		Pjv = P_JV(PjvRaw);

		if (Level == 1) {
			COPYTOLEAF_EVEN(Pjll, uint8_t);
			SETIMMTYPE(cJL_JPIMMED_1_02);
		} else
			assert(0);

		return 1;
	}

	for (levelsub = Level; levelsub >= 1; --levelsub) {
		Pjll_t PjllRaw;
		Pjll_t Pjll;
		if (pop1 > leaf_maxpop1[levelsub])
			continue;
		if ((levelsub < Level) && (!SAMESUBEXP(levelsub)))
			goto BuildBranch;
		assert(pop1 <= cJL_POP0MASK(Level) + 1);
		assert(!((PIndex[0] ^ PIndex[pop1 - 1]) & cJL_DCDMASK(Level)));

		CHECKLEAFORDER;

		switch (levelsub) {
		case 1:
			MAKELEAF_EVEN(1, cJL_JPLEAF1, judyLAllocJLL1,
				      JL_LEAF1VALUEAREA, uint8_t);
			break;
		case 2:
			MAKELEAF_EVEN(2, cJL_JPLEAF2, judyLAllocJLL2,
				      JL_LEAF2VALUEAREA, uint16_t);
			break;
		case 3:
			MAKELEAF_ODD(3, cJL_JPLEAF3, judyLAllocJLL3,
				     JL_LEAF3VALUEAREA, JL_COPY3_LONG_TO_PINDEX);
			break;
		default:
			assert(0);
		}

		return 1;
	}

	if ((Level == 1) || SAMESUBEXP(1)) {
		Pjlb_t PjlbRaw, Pjlb;

		assert(pop1 <= cJL_JPFULLPOPU1_POP0 + 1);
		CHECKLEAFORDER;

		if ((PjlbRaw = judyLAllocJLB1(Pjpm)) == NULL)
			NOMEM;
		Pjlb = P_JLB(PjlbRaw);

		for (offset = 0; offset < pop1; ++offset)
			JL_BITMAPSETL(Pjlb, PIndex[offset]);

		retval = 1;

		for (offset = 0; offset < cJL_NUMSUBEXPL; ++offset) {
			if (!(pop1sub = judyCountBits(JL_JLB_BITMAP(Pjlb, offset))))
				continue;
			if ((PjvRaw = judyLAllocJV(pop1sub, Pjpm)) == NULL) {
				for ( ; offset < cJL_NUMSUBEXPL; ++offset) {
					*PPop1 -= judyCountBits(JL_JLB_BITMAP(Pjlb, offset));
					JL_JLB_BITMAP(Pjlb, offset) = 0;
				}

				retval = 0;
				break;
			}

			Pjv = P_JV(PjvRaw);
			JL_COPYMEM(Pjv, PValue, pop1sub);
			JL_JLB_PVALUE(Pjlb, offset) = PjvRaw;
			PValue += pop1sub;
		}

		JL_JPSETADT(PjpParent, (Word_t) PjlbRaw,
			    (*PIndex & cJL_DCDMASK(1)) | (*PPop1 - 1), cJL_JPLEAF_B1);

		return retval;
	}

      BuildBranch:
	assert(Level >= 2);
	assert(Level < cJL_ROOTSTATE);
	assert(!SAMESUBEXP(1));

	for (levelsub = Level; levelsub >= 3; --levelsub)
		if (!SAMESUBEXP(levelsub - 1))
			break;
      BuildBranch2: /* come here directly for Level = levelsub = cJL_ROOTSTATE. */
	assert(levelsub >= 2);
	assert(levelsub <= Level);

	if ((PjbuRaw = judyLAllocJBU(Pjpm)) == NULL)
		NOMEM;
	Pjbu = P_JBU(PjbuRaw);

	JPtype_null = cJL_JPNULL1 + levelsub - 2;
	JL_JPSETADT(&JPnull, 0, 0, JPtype_null);

	Pjp = Pjbu->jbu_jp;
	numJPs = 0;
	digitmask = cJL_MASKATSTATE(levelsub);
	digitshincr = 1UL << (cJL_BITSPERBYTE * (levelsub - 1));
	retval = 1;

	for (digit = digitshifted = 0;
	     digit < cJL_BRANCHUNUMJPS; ++digit, digitshifted += digitshincr, ++Pjp) {
		assert(pop1 != 0);
		for (pop1sub = 0; pop1sub < pop1; ++pop1sub)
			if (digitshifted != (PIndex[pop1sub] & digitmask))
				break;

		if (pop1sub == 0) {
			if (digitshifted < (PIndex[0] & digitmask)) {
				SETJPNULL(Pjp);
				continue;
			}

			assert(pop1 < *PPop1);
			JL_SET_ERRNO(JLE_UNSORTED);
			goto AbandonBranch;
		}

		if (pop1sub == 1) {
			Word_t Addr = 0;
			Addr = (Word_t) (*PValue++);
			JL_JPSETADT(Pjp, Addr, *PIndex, cJL_JPIMMED_1_01 + levelsub - 2);

			++numJPs;

			if (--pop1) {
				++PIndex;
				continue;
			}

			++digit;
			++Pjp;
			goto ClearBranch;
		}

		if (judyInsArray(Pjp, levelsub - 1, &pop1sub, PIndex, PValue, Pjpm)) {
			++numJPs;
			assert(pop1 >= pop1sub);

			if ((pop1 -= pop1sub) != 0) {
				PIndex += pop1sub;
				PValue += pop1sub;
				continue;
			}

			++digit;
			++Pjp;
			goto ClearBranch;
		}

		assert(pop1 > pop1sub);
		if (pop1sub) {
			++digit;
			++Pjp;
			++numJPs;
		}
		pop1 -= pop1sub;
	AbandonBranch:
		assert(pop1 != 0);
		assert(pop1 <= *PPop1);
		*PPop1 -= pop1;
		pop1 = 0;
		retval = 0;
	ClearBranch:
		for ( /* null */ ; digit < cJL_BRANCHUNUMJPS; ++digit, ++Pjp)
			SETJPNULL(Pjp);
		break;
	}

	Pjbany = (Word_t) PjbuRaw;
	JPtype = branchU_JPtype[levelsub];

	assert((!retval) || *PPop1);
	if ((!retval) && (*PPop1 == 0))	{
		judyLFreeJBU(PjbuRaw, Pjpm);
		SETJPNULL_PARENT;
		return 0;
	}

	if (pop1 != 0) {
		JL_SET_ERRNO(JLE_UNSORTED);
		*PPop1 -= pop1;
		retval = 0;
	}
	assert(*PPop1 != 0);

	if (numJPs <= cJL_BRANCHLMAXJPS) {
		Pjbl_t PjblRaw = NULL;
		Pjbl_t Pjbl;

		if (*PPop1 > JL_BRANCHL_MAX_POP || (PjblRaw = judyLAllocJBL(Pjpm)) == NULL)
			goto SetParent;

		Pjbl = P_JBL(PjblRaw);
		(Pjbl->jbl_NumJPs) = numJPs;
		offset = 0;

		for (digit = 0; digit < cJL_BRANCHUNUMJPS; ++digit) {
			if ((((Pjbu->jbu_jp) + digit)->jp_Type) == JPtype_null)
				continue;

			(Pjbl->jbl_Expanse[offset]) = digit;
			(Pjbl->jbl_jp[offset++]) = Pjbu->jbu_jp[digit];
		}
		assert(offset == numJPs);
		judyLFreeJBU(PjbuRaw, Pjpm);

		Pjbany = (Word_t) PjblRaw;
		JPtype = branchL_JPtype[levelsub];
	} else {
		Pjbb_t PjbbRaw = NULL;
		Pjbb_t Pjbb;
		Pjp_t Pjp2;

		if (*PPop1 > JL_BRANCHB_MAX_POP || (PjbbRaw = judyLAllocJBB(Pjpm)) == NULL)
			goto SetParent;

		Pjbb = P_JBB(PjbbRaw);

		Pjp2 = Pjbu->jbu_jp;
		for (digit = 0; digit < cJL_BRANCHUNUMJPS; ++digit)
			if ((((Pjbu->jbu_jp) + digit)->jp_Type) != JPtype_null)
				JL_BITMAPSETB(Pjbb, digit);

		for (offset = 0; offset < cJL_NUMSUBEXPB; ++offset) {
			Pjp_t PjparrayRaw;
			Pjp_t Pjparray;

			if (!(numJPs = judyCountBits(JL_JBB_BITMAP(Pjbb, offset))))
				continue;

			if ((PjparrayRaw = judyLAllocJBBJP(numJPs, Pjpm)) == NULL) {
				while (offset-- > 0) {
					if (JL_JBB_PJP(Pjbb, offset) == NULL)
						continue;
					judyLFreeJBBJP(JL_JBB_PJP(Pjbb, offset),
						      judyCountBits(JL_JBB_BITMAP(Pjbb, offset)),
						      Pjpm);
				}
				judyLFreeJBB(PjbbRaw, Pjpm);
				goto SetParent;
			}

			JL_JBB_PJP(Pjbb, offset) = PjparrayRaw;
			Pjparray = P_JP(PjparrayRaw);

			while (numJPs-- > 0) {
				while ((Pjp2->jp_Type) == JPtype_null) {
					++Pjp2;
					assert(Pjp2 < (Pjbu->jbu_jp) + cJL_BRANCHUNUMJPS);
				}
				*Pjparray++ = *Pjp2++;
			}
		}
		judyLFreeJBU(PjbuRaw, Pjpm);
		Pjbany = (Word_t) PjbbRaw;
		JPtype = branchB_JPtype[levelsub];
	}

      SetParent:
	(PjpParent->jp_Addr) = Pjbany;
	(PjpParent->jp_Type) = JPtype;
	if (Level < cJL_ROOTSTATE) {
		Word_t DcdP0 = (*PIndex & cJL_DCDMASK(levelsub)) | (*PPop1 - 1);
		JL_JPSETADT(PjpParent, Pjbany, DcdP0, JPtype);
	}

	return retval;
}

/**
 * @PPArray	in which to insert, initially empty.
 * @Count	number of indexes (and values) to insert.
 * @PIndex	list of indexes to insert.
 * @PValue	list of corresponding values.
 */
int JudyLInsArray(void **PPArray, size_t Count,
		  const uint32_t *PIndex, void * const *Value)
{
	Pjlw_t Pjlw, Pjlwindex;
	int offset;
	Pjv_t PValue = (Pjv_t)Value;

	if (!PPArray || !PIndex || !PValue) {
		JL_SET_ERRNO(JLE_NULLPPARRAY);
		return JERR;
	}

	if (*PPArray != NULL) {
		JL_SET_ERRNO(JLE_NONNULLPARRAY);
		return JERR;
	}

	if (Count > cJL_LEAFW_MAXPOP1) {
		Word_t tmp = Count;
		Pjpm_t Pjpm;
		Pjpm = judyLAllocJPM();
		JL_CHECKALLOC(Pjpm_t, Pjpm, JERR);
		*PPArray = (void *) Pjpm;
		Pjpm->jpm_Pop0 = Count - 1;
		if (!judyInsArray(&Pjpm->jpm_JP, cJL_ROOTSTATE, &tmp,
				  PIndex, PValue, Pjpm)) {
			if (Count) {
				(Pjpm->jpm_Pop0) = Count - 1;
			} else {
				judyLFreeJPM(Pjpm, NULL);
				*PPArray = NULL;
			}

			return JERR;
		}
		return 1;
	}

	for (offset = 1; offset < Count; ++offset) {
		if (PIndex[offset - 1] >= PIndex[offset]) {
			JL_SET_ERRNO(JLE_UNSORTED);
			return JERR;
		}
	}

	if (Count == 0)
		return 1;

	Pjlw = judyLAllocJLW(Count + 1);
	JL_CHECKALLOC(Pjlw_t, Pjlw, JERR);
	*PPArray = (void *) Pjlw;
	Pjlw[0] = Count - 1;
	Pjlwindex = Pjlw + 1;

	JL_COPYMEM(Pjlwindex, PIndex, Count);
	JL_COPYMEM(JL_LEAFWVALUEAREA(Pjlw, Count), PValue, Count);

	return 1;
}
