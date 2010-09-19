#include "JudyL.h"

static void judyCopy2to3(uint8_t *PDest, uint16_t *PSrc, Word_t Pop1, Word_t MSByte)
{
	Word_t Temp;
	assert(Pop1);
	do {
		Temp = MSByte | *PSrc++;
		JL_COPY3_LONG_TO_PINDEX(PDest, Temp);
		PDest += 3;
	} while (--Pop1);
}

static void judyCopy3toW(PWord_t PDest, uint8_t *PSrc, Word_t Pop1, Word_t MSByte)
{
	assert(Pop1);
	do {
		JL_COPY3_PINDEX_TO_LONG(*PDest, PSrc);
		*PDest++ |= MSByte;
		PSrc += 3;
	} while (--Pop1);
}

int judyBranchBToBranchL(Pjp_t Pjp, void *Pjpm)
{
	Pjbb_t PjbbRaw;		// old BranchB to shrink.
	Pjbb_t Pjbb;
	Pjbl_t PjblRaw;		// new BranchL to create.
	Pjbl_t Pjbl;
	Word_t Digit;		// in BranchB.
	Word_t NumJPs;		// non-null JPs in BranchB.
	uint8_t Expanse[cJL_BRANCHLMAXJPS];	// for building jbl_Expanse[].
	Pjp_t Pjpjbl;		// current JP in BranchL.
	Word_t SubExp;		// in BranchB.

	assert(JL_JPTYPE(Pjp) >= cJL_JPBRANCH_B2);
	assert(JL_JPTYPE(Pjp) <= cJL_JPBRANCH_B);

	PjbbRaw = (Pjbb_t) (Pjp->jp_Addr);
	Pjbb = P_JBB(PjbbRaw);

	for (NumJPs = Digit = 0; Digit < cJL_BRANCHUNUMJPS; ++Digit) {
		if (JL_BITMAPTESTB(Pjbb, Digit)) {
			Expanse[NumJPs++] = Digit;
			assert(NumJPs <= cJL_BRANCHLMAXJPS);	// required of caller.
		}
	}

	if ((PjblRaw = judyLAllocJBL(Pjpm)) == NULL)
		return -1;
	Pjbl = P_JBL(PjblRaw);

	JL_COPYMEM(Pjbl->jbl_Expanse, Expanse, NumJPs);

	Pjbl->jbl_NumJPs = NumJPs;
	Pjpjbl = P_JP(Pjbl->jbl_jp);	// start at first JP in array.

	for (SubExp = 0; SubExp < cJL_NUMSUBEXPB; ++SubExp) {
		Pjp_t PjpRaw = JL_JBB_PJP(Pjbb, SubExp);	// current Pjp.
		Pjp_t Pjp;

		if (PjpRaw == NULL)
			continue;	// skip empty subexpanse.
		Pjp = P_JP(PjpRaw);

		NumJPs = judyCountBits(JL_JBB_BITMAP(Pjbb, SubExp));
		assert(NumJPs);
		JL_COPYMEM(Pjpjbl, Pjp, NumJPs);	// one subarray at a time.

		Pjpjbl += NumJPs;
		judyLFreeJBBJP(PjpRaw, NumJPs, Pjpm);	// subarray.
	}
	judyLFreeJBB(PjbbRaw, Pjpm);	// BranchB itself.

	Pjp->jp_Type += cJL_JPBRANCH_L - cJL_JPBRANCH_B;
	Pjp->jp_Addr = (Word_t) PjblRaw;

	return 1;
}

int judyLeafB1ToLeaf1(Pjp_t Pjp, void *Pjpm)
{
	Pjlb_t PjlbRaw;		// bitmap in old leaf.
	Pjlb_t Pjlb;
	Pjll_t PjllRaw;		// new Leaf1.
	uint8_t *Pleaf1;	// Leaf1 pointer type.
	Word_t Digit;		// in LeafB1 bitmap.
	Pjv_t PjvNew;		// value area in new Leaf1.
	Word_t Pop1;
	Word_t SubExp;
	assert(JL_JPTYPE(Pjp) == cJL_JPLEAF_B1);
	assert(((JL_JPDCDPOP0(Pjp) & 0xFF) + 1) == cJL_LEAF1_MAXPOP1);

	if ((PjllRaw = judyLAllocJLL1(cJL_LEAF1_MAXPOP1, Pjpm)) == 0)
		return -1;

	Pleaf1 = (uint8_t *) P_JLL(PjllRaw);
	PjlbRaw = (Pjlb_t) (Pjp->jp_Addr);
	Pjlb = P_JLB(PjlbRaw);
	PjvNew = JL_LEAF1VALUEAREA(Pleaf1, cJL_LEAF1_MAXPOP1);

	for (Digit = 0; Digit < cJL_BRANCHUNUMJPS; ++Digit)
		if (JL_BITMAPTESTL(Pjlb, Digit))
			*Pleaf1++ = Digit;

	for (SubExp = 0; SubExp < cJL_NUMSUBEXPL; ++SubExp) {
		Pjv_t PjvRaw = JL_JLB_PVALUE(Pjlb, SubExp);
		Pjv_t Pjv = P_JV(PjvRaw);

		if (Pjv == NULL)
			continue;	// skip empty subarray.

		Pop1 = judyCountBits(JL_JLB_BITMAP(Pjlb, SubExp));	// subarray.
		assert(Pop1);

		JL_COPYMEM(PjvNew, Pjv, Pop1);	// copy value areas.
		judyLFreeJV(PjvRaw, Pop1, Pjpm);
		PjvNew += Pop1;	// advance through new.
	}

	assert((((Word_t) Pleaf1) - (Word_t) P_JLL(PjllRaw))
	       == (PjvNew - JL_LEAF1VALUEAREA(P_JLL(PjllRaw), cJL_LEAF1_MAXPOP1)));
	judyLFreeJLB1(PjlbRaw, Pjpm);

	Pjp->jp_Addr = (Word_t) PjllRaw;
	Pjp->jp_Type = cJL_JPLEAF1;

	return 1;
}

/**
 * @PLeaf2	destination uint16_t * Index portion of leaf.
 * @Pjv2	destination value part of leaf.
 * @Pjp		1-byte-index object from which to copy.
 * @MSByte	most-significant byte, prefix to each Index.
 * @Pjpm	for global accounting.
 */
Word_t judyLeaf1ToLeaf2(uint16_t *PLeaf2, Pjv_t Pjv2, Pjp_t Pjp, Word_t MSByte, void *Pjpm)
{
	Word_t Pop1;		// Indexes in leaf.
	Word_t Offset;		// in linear leaf list.
	Pjv_t Pjv1Raw;		// source object value area.
	Pjv_t Pjv1;

	switch (JL_JPTYPE(Pjp)) {
	case cJL_JPLEAF_B1: {
		Pjlb_t Pjlb = P_JLB(Pjp->jp_Addr);
		Word_t Digit;	// in LeafB1 bitmap.
		Word_t SubExp;	// in LeafB1.

		Pop1 = JL_JPBRANCH_POP0(Pjp, 1) + 1;
		assert(Pop1);

		for (Digit = 0; Digit < cJL_BRANCHUNUMJPS; ++Digit)
			if (JL_BITMAPTESTL(Pjlb, Digit))
				*PLeaf2++ = MSByte | Digit;
		for (SubExp = 0; SubExp < cJL_NUMSUBEXPL; ++SubExp) {
			Word_t SubExpPop1;

			Pjv1Raw = JL_JLB_PVALUE(Pjlb, SubExp);
			if (Pjv1Raw == NULL)
				continue;	// skip empty.
			Pjv1 = P_JV(Pjv1Raw);

			SubExpPop1 = judyCountBits(JL_JLB_BITMAP(Pjlb, SubExp));
			assert(SubExpPop1);

			JL_COPYMEM(Pjv2, Pjv1, SubExpPop1);	// copy value areas.
			judyLFreeJV(Pjv1Raw, SubExpPop1, Pjpm);
			Pjv2 += SubExpPop1;	// advance through new.
		}
		judyLFreeJLB1((Pjlb_t) (Pjp->jp_Addr), Pjpm);	// LeafB1 itself.
		return Pop1;
	}
	case cJL_JPLEAF1: {
		uint8_t *PLeaf1 = (uint8_t *) P_JLL(Pjp->jp_Addr);

		Pop1 = JL_JPBRANCH_POP0(Pjp, 1) + 1;
		assert(Pop1);
		Pjv1 = JL_LEAF1VALUEAREA(PLeaf1, Pop1);

		for (Offset = 0; Offset < Pop1; ++Offset) {
			PLeaf2[Offset] = MSByte | PLeaf1[Offset];
			Pjv2[Offset] = Pjv1[Offset];
		}
		judyLFreeJLL1((Pjll_t) (Pjp->jp_Addr), Pop1, Pjpm);
		return Pop1;
	}
	case cJL_JPIMMED_1_01: {
		PLeaf2[0] = JL_JPDCDPOP0(Pjp);	// see above.
		Pjv2[0] = Pjp->jp_Addr;
		return 1;
	}
	case cJL_JPIMMED_1_02:
	case cJL_JPIMMED_1_03: {
		Pop1 = JL_JPTYPE(Pjp) - cJL_JPIMMED_1_02 + 2;
		assert(Pop1);
		Pjv1Raw = (Pjv_t) (Pjp->jp_Addr);
		Pjv1 = P_JV(Pjv1Raw);

		for (Offset = 0; Offset < Pop1; ++Offset) {
			PLeaf2[Offset] = MSByte | Pjp->jp_LIndex[Offset];
			Pjv2[Offset] = Pjv1[Offset];
		}
		judyLFreeJV(Pjv1Raw, Pop1, Pjpm);
		return Pop1;
	}
	default:
		assert(0);
		break;
	}
	return 0;
}

/**
 * @PLeaf3	 destination "uint24_t *" Index part of leaf.
 * @Pjv3	 destination value part of leaf.
 * @Pjp		 2-byte-index object from which to copy.
 * @MSByte	 most-significant byte, prefix to each Index.
 * @Pjpm	 for global accounting.
 */
Word_t judyLeaf2ToLeaf3(uint8_t *PLeaf3, Pjv_t Pjv3, Pjp_t Pjp, Word_t MSByte,	void *Pjpm)
{
	int jptype = JL_JPTYPE(Pjp);

	if (jptype == cJL_JPLEAF2) {
		Pjv_t Pjv2;
		uint16_t *PLeaf2 = (uint16_t *) P_JLL(Pjp->jp_Addr);
		Word_t Pop1 = JL_JPLEAF_POP0(Pjp) + 1;
		assert(Pop1);
		judyCopy2to3(PLeaf3, PLeaf2, Pop1, MSByte);
		Pjv2 = JL_LEAF2VALUEAREA(PLeaf2, Pop1);
		JL_COPYMEM(Pjv3, Pjv2, Pop1);
		judyLFreeJLL2((Pjll_t) (Pjp->jp_Addr), Pop1, Pjpm);
		return Pop1;
	} else if (jptype == cJL_JPIMMED_2_01) {
		JL_COPY3_LONG_TO_PINDEX(PLeaf3, JL_JPDCDPOP0(Pjp));
		Pjv3[0] = Pjp->jp_Addr;
		return 1;
	} else {
		assert(0);
	}

	return 0;
}

/** 
 * @Pjlw	destination Index part of leaf.
 * @PjvW	destination value part of leaf.
 * @Pjp		3-byte-index object from which to copy.
 * @MSByt	emost-significant byte, prefix to each Index.
 * @Pjpm	for global accounting.
 */
Word_t judyLeaf3ToLeafW(Pjlw_t Pjlw, Pjv_t PjvW, Pjp_t Pjp, Word_t MSByte, void *Pjpm)
{
	Word_t Pop1;		// Indexes in leaf.
	Pjv_t Pjv3;		// source object value area.
	int jptype = JL_JPTYPE(Pjp);

	if (jptype == cJL_JPLEAF3) {
		uint8_t *PLeaf3 = (uint8_t *) P_JLL(Pjp->jp_Addr);

		Pop1 = JL_JPLEAF_POP0(Pjp) + 1;
		judyCopy3toW((PWord_t ) Pjlw, PLeaf3, Pop1, MSByte);
		Pjv3 = JL_LEAF3VALUEAREA(PLeaf3, Pop1);
		JL_COPYMEM(PjvW, Pjv3, Pop1);
		judyLFreeJLL3((Pjll_t) (Pjp->jp_Addr), Pop1, Pjpm);
		return Pop1;
	} else if (jptype == cJL_JPIMMED_3_01) {
		Pjlw[0] = MSByte | JL_JPDCDPOP0(Pjp);	// see above.
		PjvW[0] = Pjp->jp_Addr;
		return 1;
	} else {
		assert(0);
	}

	return 0;
}
