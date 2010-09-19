#include "JudyL.h"

extern int judyBranchBToBranchL(Pjp_t Pjp, void *Pjpm);
extern int judyLeafB1ToLeaf1(Pjp_t, void *);
extern Word_t judyLeaf1ToLeaf2(uint16_t *, Pjv_t, Pjp_t, Word_t , void *);
extern Word_t judyLeaf2ToLeaf3(uint8_t *, Pjv_t, Pjp_t, Word_t , void *);
extern Word_t judyLLeaf3ToLeafW(Pjlw_t, Pjv_t, Pjp_t, Word_t , void *);
extern Word_t judyLeaf3ToLeafW(Pjlw_t, Pjv_t, Pjp_t, Word_t , void *);

// For convenience in the calling code; "M1" means "minus one":
#define judyLeafM1ToLeafW judyLeaf3ToLeafW

/* Given a pointer to a JP, an Index known to be valid, the number of bytes
 * left to decode (== level in the tree), and a pointer to a global JPM, walk a
 * Judy (sub)tree to do an unset/delete of that index, and possibly modify the
 * JPM.  This function is only called internally, and recursively.  Unlike
 * Judy1Test() and JudyLGet(), the extra time required for recursion should be
 * negligible compared with the total.
 * Return values:
 * -1 error; details in JPM
 *  0 Index already deleted (should never happen, Index is known to be valid)
 *  1 previously valid Index deleted
 *  2 same as 1, but in addition the JP now points to a BranchL containing a
 *    single JP, which should be compressed into the parent branch (if there
 *    is one, which is not the case for a top-level branch under a JPM) */
static int judyDelWalk(Pjp_t Pjp, Word_t Index,	Word_t ParentLevel, Pjpm_t Pjpm)	
{
	Word_t pop1;		// of a leaf.
	Word_t level;		// of a leaf.
	uint8_t digit;		// from Index, in current branch.
	Pjll_t PjllnewRaw;	// address of newly allocated leaf.
	Pjll_t Pjllnew;
	int offset;		// within a branch.
	int retcode;		// return code: -1, 0, 1, 2.
	Pjv_t PjvRaw;		// value area.
	Pjv_t Pjv;

      ContinueDelWalk:		// for modifying state without recursing.
	switch (JL_JPTYPE(Pjp)) {

#define JL_BRANCH_KEEP(cLevel,MaxPop1,Next)             \
        if (pop1 > (MaxPop1)) {  /* hysteresis = 1 */    \
            assert((cLevel) >= 2);                      \
            level = (cLevel);                           \
            digit = JL_DIGITATSTATE(Index, cLevel);     \
            goto Next;                                  \
        }

#define JL_PVALUEPASS  Pjv,

#define JL_BRANCH_COPY_IMMED_EVEN(cLevel,Pjp,ignore)            \
        if (JL_JPTYPE(Pjp) == cJL_JPIMMED_1_01 + (cLevel) - 2) {\
            *Pleaf++ = JL_JPDCDPOP0(Pjp);                       \
	    *Pjv++   = (Pjp)->jp_Addr;                          \
            continue;   /* for-loop */                          \
        }

#define JL_BRANCH_COPY_IMMED_ODD(cLevel,Pjp,CopyIndex)          \
        if (JL_JPTYPE(Pjp) == cJL_JPIMMED_1_01 + (cLevel) - 2) {\
            CopyIndex(Pleaf, (Word_t ) (JL_JPDCDPOP0(Pjp)));     \
            Pleaf += (cLevel);  /* index size = level */        \
	    *Pjv++ = (Pjp)->jp_Addr;                            \
            continue;   /* for-loop */                          \
        }

#define JL_BRANCHL_COMPRESS(cLevel,LeafType,MaxPop1,NewJPType,          \
                            LeafToLeaf,Alloc,ValueArea,                 \
                            CopyImmed,CopyIndex)                        \
{                                                               \
    LeafType Pleaf;                                             \
    Pjbl_t   PjblRaw;                                           \
    Pjbl_t   Pjbl;                                              \
    Word_t numJPs;                                            \
								\
    if ((PjllnewRaw = Alloc(MaxPop1, Pjpm)) == 0) return -1;   \
    Pjllnew = P_JLL(PjllnewRaw);                                \
    Pleaf   = (LeafType) Pjllnew;                               \
    Pjv     = ValueArea(Pleaf, MaxPop1);                        \
								\
    PjblRaw = (Pjbl_t) (Pjp->jp_Addr);                          \
    Pjbl    = P_JBL(PjblRaw);                                   \
    numJPs  = Pjbl->jbl_NumJPs;                                 \
								\
    for (offset = 0; offset < numJPs; ++offset) {               \
	CopyImmed(cLevel, (Pjbl->jbl_jp) + offset, CopyIndex);  \
	pop1 = LeafToLeaf(Pleaf, JL_PVALUEPASS                  \
		  (Pjbl->jbl_jp) + offset,                      \
		  JL_DIGITTOSTATE(Pjbl->jbl_Expanse[offset],    \
		  cLevel), (void *) Pjpm);                     \
	Pleaf = (LeafType) (((Word_t ) Pleaf) + ((cLevel) * pop1)); \
	Pjv  += pop1;                                          \
    }                                                           \
    assert(((((Word_t ) Pleaf) - ((Word_t ) Pjllnew)) / (cLevel)) == (MaxPop1)); \
    assert((Pjv - ValueArea(Pjllnew, MaxPop1)) == (MaxPop1));  \
    judyLFreeJBL(PjblRaw, Pjpm);                               \
    Pjp->jp_Type = (NewJPType);                                 \
    Pjp->jp_Addr = (Word_t ) PjllnewRaw;                         \
    goto ContinueDelWalk;       /* delete from new leaf */      \
}

#define JL_BRANCHL(cLevel,MaxPop1,LeafType,NewJPType,                   \
                   LeafToLeaf,Alloc,ValueArea,CopyImmed,CopyIndex)      \
        assert(! JL_DCDNOTMATCHINDEX(Index, Pjp, cLevel));              \
        assert(ParentLevel > (cLevel));                                 \
        pop1 = JL_JPBRANCH_POP0(Pjp, cLevel) + 1;                       \
        JL_BRANCH_KEEP(cLevel, MaxPop1, BranchLKeep);                   \
        assert(pop1 == (MaxPop1));                                      \
        JL_BRANCHL_COMPRESS(cLevel, LeafType, MaxPop1, NewJPType,       \
                            LeafToLeaf, Alloc, ValueArea, CopyImmed, CopyIndex)

	case cJL_JPBRANCH_L2:
		JL_BRANCHL(2, cJL_LEAF2_MAXPOP1, uint16_t *, cJL_JPLEAF2,
			   judyLeaf1ToLeaf2, judyLAllocJLL2, JL_LEAF2VALUEAREA,
			   JL_BRANCH_COPY_IMMED_EVEN, ignore);
	case cJL_JPBRANCH_L3:
		JL_BRANCHL(3, cJL_LEAF3_MAXPOP1, uint8_t *, cJL_JPLEAF3,
			   judyLeaf2ToLeaf3, judyLAllocJLL3, JL_LEAF3VALUEAREA,
			   JL_BRANCH_COPY_IMMED_ODD, JL_COPY3_LONG_TO_PINDEX);
	case cJL_JPBRANCH_L: {
		Pjbl_t Pjbl;
		Word_t numJPs;
		level = cJL_ROOTSTATE;
		digit = JL_DIGITATSTATE(Index, cJL_ROOTSTATE);
	      BranchLKeep:
		Pjbl = P_JBL(Pjp->jp_Addr);
		numJPs = Pjbl->jbl_NumJPs;
		assert(numJPs > 0);
		for (offset = 0; (Pjbl->jbl_Expanse[offset]) != digit; ++offset)
			assert(offset < numJPs - 1);
		Pjp = (Pjbl->jbl_jp) + offset;
		assert(level >= 2);
		if ((JL_JPTYPE(Pjp)) != cJL_JPIMMED_1_01 + level - 2)
			break;
		assert(JL_JPDCDPOP0(Pjp) == JL_TRIMTODCDSIZE(Index));
		JL_DELETEINPLACE(Pjbl->jbl_Expanse, numJPs, offset, ignore);
		JL_DELETEINPLACE(Pjbl->jbl_jp, numJPs, offset, ignore);
		return ((--(Pjbl->jbl_NumJPs) <= 1) ? 2 : 1);
	}
#define JL_BRANCHB_COMPRESS(cLevel,LeafType,MaxPop1,NewJPType,          \
                            LeafToLeaf,Alloc,ValueArea,                 \
                            CopyImmed,CopyIndex)                        \
{                                                               \
    LeafType  Pleaf;                                            \
    Pjbb_t    PjbbRaw;  /* BranchB to compress */               \
    Pjbb_t    Pjbb;                                             \
    Word_t subexp;   /* current subexpanse number    */      \
    BITMAPB_t bitmap;   /* portion for this subexpanse  */      \
    Pjp_t     Pjp2Raw;  /* one subexpanses subarray     */      \
    Pjp_t     Pjp2;                                             \
								\
    if ((PjllnewRaw = Alloc(MaxPop1, Pjpm)) == 0) return -1;   \
    Pjllnew = P_JLL(PjllnewRaw);                                \
    Pleaf   = (LeafType) Pjllnew;                               \
    Pjv     = ValueArea(Pleaf, MaxPop1);                        \
    PjbbRaw = (Pjbb_t) (Pjp->jp_Addr);                          \
    Pjbb    = P_JBB(PjbbRaw);                                   \
								\
    for (subexp = 0; subexp < cJL_NUMSUBEXPB; ++subexp) {       \
	if ((bitmap = JL_JBB_BITMAP(Pjbb, subexp)) == 0)        \
	    continue;           /* empty subexpanse */          \
								\
	digit   = subexp * cJL_BITSPERSUBEXPB;                  \
	Pjp2Raw = JL_JBB_PJP(Pjbb, subexp);                     \
	Pjp2    = P_JP(Pjp2Raw);                                \
	assert(Pjp2 != NULL);                           \
								\
	for (offset = 0; bitmap != 0; bitmap >>= 1, ++digit) {  \
	    if (! (bitmap & 1))                                 \
		continue;       /* empty sub-subexpanse */      \
	    ++offset;           /* before any continue */       \
	    CopyImmed(cLevel, Pjp2 + offset - 1, CopyIndex);    \
	    pop1 = LeafToLeaf(Pleaf, JL_PVALUEPASS              \
			      Pjp2 + offset - 1,                \
			      JL_DIGITTOSTATE(digit, cLevel),   \
			      (void *) Pjpm);                  \
	    Pleaf = (LeafType) (((Word_t ) Pleaf) + ((cLevel) * pop1)); \
	    Pjv  += pop1;                                      \
	}                                                       \
	judyLFreeJBBJP(Pjp2Raw, /* pop1 = */ offset, Pjpm);    \
    }                                                           \
    assert(((((Word_t ) Pleaf) - ((Word_t ) Pjllnew)) / (cLevel)) == (MaxPop1)); \
    assert((Pjv - ValueArea(Pjllnew, MaxPop1)) == (MaxPop1));  \
    judyLFreeJBB(PjbbRaw, Pjpm);                               \
    Pjp->jp_Type = (NewJPType);                                 \
    Pjp->jp_Addr = (Word_t ) PjllnewRaw;                         \
    goto ContinueDelWalk;       /* delete from new leaf */      \
}

#define JL_BRANCHB(cLevel,MaxPop1,LeafType,NewJPType,                   \
                   LeafToLeaf,Alloc,ValueArea,CopyImmed,CopyIndex)      \
        assert(! JL_DCDNOTMATCHINDEX(Index, Pjp, cLevel));              \
        assert(ParentLevel > (cLevel));                                 \
        pop1 = JL_JPBRANCH_POP0(Pjp, cLevel) + 1;                       \
        JL_BRANCH_KEEP(cLevel, MaxPop1, BranchBKeep);                   \
        assert(pop1 == (MaxPop1));                                      \
        JL_BRANCHB_COMPRESS(cLevel, LeafType, MaxPop1, NewJPType,       \
                            LeafToLeaf, Alloc, ValueArea, CopyImmed, CopyIndex)

	case cJL_JPBRANCH_B2:
		JL_BRANCHB(2, cJL_LEAF2_MAXPOP1, uint16_t *, cJL_JPLEAF2,
			   judyLeaf1ToLeaf2, judyLAllocJLL2, JL_LEAF2VALUEAREA,
			   JL_BRANCH_COPY_IMMED_EVEN, ignore);
	case cJL_JPBRANCH_B3:
		JL_BRANCHB(3, cJL_LEAF3_MAXPOP1, uint8_t *, cJL_JPLEAF3,
			   judyLeaf2ToLeaf3, judyLAllocJLL3, JL_LEAF3VALUEAREA,
			   JL_BRANCH_COPY_IMMED_ODD, JL_COPY3_LONG_TO_PINDEX);
	case cJL_JPBRANCH_B: {
		Pjbb_t Pjbb;	// BranchB to modify.
		Word_t subexp;	// current subexpanse number.
		Word_t subexp2;	// in second-level loop.
		BITMAPB_t bitmap;	// portion for this subexpanse.
		BITMAPB_t bitmask;	// with digits bit set.
		Pjp_t Pjp2Raw;	// one subexpanses subarray.
		Pjp_t Pjp2;
		Word_t numJPs;	// in one subexpanse.

		level = cJL_ROOTSTATE;
		digit = JL_DIGITATSTATE(Index, cJL_ROOTSTATE);

	      BranchBKeep:
		Pjbb = P_JBB(Pjp->jp_Addr);
		subexp = digit / cJL_BITSPERSUBEXPB;
		bitmap = JL_JBB_BITMAP(Pjbb, subexp);
		bitmask = JL_BITPOSMASKB(digit);
		assert(bitmap & bitmask);	// Index valid => digits bit is set.

		offset = ((bitmap == (cJL_FULLBITMAPB)) ?
			  digit % cJL_BITSPERSUBEXPB :
			  judyCountBits(bitmap & JL_MASKLOWEREXC(bitmask)));
		Pjp2Raw = JL_JBB_PJP(Pjbb, subexp);
		Pjp2 = P_JP(Pjp2Raw);
		assert(Pjp2 != NULL);	// valid subexpanse pointer.

		if (JL_JPTYPE(Pjp2 + offset) != cJL_JPIMMED_1_01 + level - 2) {
			Pjp = Pjp2 + offset;
			break;
		}

		assert(JL_JPDCDPOP0(Pjp2 + offset) == JL_TRIMTODCDSIZE(Index));
		if ((numJPs = judyCountBits(bitmap)) == 1) {
			judyLFreeJBBJP(Pjp2Raw, /* pop1 = */ 1, Pjpm);
			JL_JBB_PJP(Pjbb, subexp) = NULL;
		} else if (JL_BRANCHBJPGROWINPLACE(numJPs - 1)) {
			assert(numJPs > 0);
			JL_DELETEINPLACE(Pjp2, numJPs, offset, ignore);
		} else {
			Pjp_t PjpnewRaw;
			Pjp_t Pjpnew;

			if ((PjpnewRaw = judyLAllocJBBJP(numJPs - 1, Pjpm)) == NULL)
				return -1;
			Pjpnew = P_JP(PjpnewRaw);

			JL_DELETECOPY(Pjpnew, Pjp2, numJPs, offset, ignore);
			judyLFreeJBBJP(Pjp2Raw, numJPs, Pjpm);	// old.

			JL_JBB_PJP(Pjbb, subexp) = PjpnewRaw;
		}
		JL_JBB_BITMAP(Pjbb, subexp) ^= bitmask;
		if (numJPs > cJL_BRANCHLMAXJPS)
			return 1;
		for (subexp2 = 0; subexp2 < cJL_NUMSUBEXPB; ++subexp2) {
			if (subexp2 == subexp)
				continue;	// skip current subexpanse.

			if ((numJPs == cJL_BRANCHLMAXJPS) ? JL_JBB_BITMAP(Pjbb, subexp2) :
			    ((numJPs += judyCountBits(JL_JBB_BITMAP(Pjbb, subexp2))) > cJL_BRANCHLMAXJPS)) {
				return 1;	// too many JPs, cannot shrink.
			}
		}

		(void)judyBranchBToBranchL(Pjp, Pjpm);
		return 1;
	} // case.

#define JL_BRANCHU_COMPRESS(cLevel,LeafType,MaxPop1,NullJPType,NewJPType,   \
                            LeafToLeaf,Alloc,ValueArea,CopyImmed,CopyIndex) \
        {                                                               \
            LeafType Pleaf;                                             \
            Pjbu_t PjbuRaw = (Pjbu_t) (Pjp->jp_Addr);                   \
            Pjp_t  Pjp2    = JL_JBU_PJP0(Pjp);                          \
            Word_t ldigit;      /* larger than uint8_t */               \
                                                                        \
            if ((PjllnewRaw = Alloc(MaxPop1, Pjpm)) == 0) return -1;   \
            Pjllnew = P_JLL(PjllnewRaw);                                \
            Pleaf   = (LeafType) Pjllnew;                               \
      	    Pjv     = ValueArea(Pleaf, MaxPop1);                       \
            for (ldigit = 0; ldigit < cJL_BRANCHUNUMJPS; ++ldigit, ++Pjp2) { \
                /* fast-process common types: */                        \
                if (JL_JPTYPE(Pjp2) == (NullJPType)) continue;          \
                CopyImmed(cLevel, Pjp2, CopyIndex);                     \
                pop1 = LeafToLeaf(Pleaf, JL_PVALUEPASS Pjp2,            \
                                  JL_DIGITTOSTATE(ldigit, cLevel),      \
                                  (void *) Pjpm);                      \
                Pleaf = (LeafType) (((Word_t ) Pleaf) + ((cLevel) * pop1)); \
                Pjv  += pop1;                                          \
            }                                                           \
            assert(((((Word_t ) Pleaf) - ((Word_t ) Pjllnew)) / (cLevel)) == (MaxPop1)); \
	    assert((Pjv - ValueArea(Pjllnew, MaxPop1)) == (MaxPop1));  \
            judyLFreeJBU(PjbuRaw, Pjpm);                               \
            Pjp->jp_Type = (NewJPType);                                 \
            Pjp->jp_Addr = (Word_t ) PjllnewRaw;                         \
            goto ContinueDelWalk;       /* delete from new leaf */      \
        }

#define JL_BRANCHU(cLevel,MaxPop1,LeafType,NullJPType,NewJPType,        \
                   LeafToLeaf,Alloc,ValueArea,CopyImmed,CopyIndex)      \
        assert(! JL_DCDNOTMATCHINDEX(Index, Pjp, cLevel));              \
        assert(ParentLevel > (cLevel));                                 \
        pop1 = JL_JPBRANCH_POP0(Pjp, cLevel) + 1;                       \
        if (pop1 > (MaxPop1)) {  /* hysteresis = 1 */                   \
            level = (cLevel);                                           \
            Pjp   = P_JP(Pjp->jp_Addr) + JL_DIGITATSTATE(Index, cLevel);\
            break;              /* descend to next level */             \
        }                                                               \
        assert(pop1 == (MaxPop1));                                      \
        JL_BRANCHU_COMPRESS(cLevel, LeafType, MaxPop1, NullJPType, NewJPType, \
                            LeafToLeaf, Alloc, ValueArea, CopyImmed, CopyIndex)

	case cJL_JPBRANCH_U2:
		JL_BRANCHU(2, cJL_LEAF2_MAXPOP1, uint16_t *,
			   cJL_JPNULL1, cJL_JPLEAF2,
			   judyLeaf1ToLeaf2, judyLAllocJLL2, JL_LEAF2VALUEAREA,
			   JL_BRANCH_COPY_IMMED_EVEN, ignore);
	case cJL_JPBRANCH_U3:
		JL_BRANCHU(3, cJL_LEAF3_MAXPOP1, uint8_t *,
			   cJL_JPNULL2, cJL_JPLEAF3,
			   judyLeaf2ToLeaf3, judyLAllocJLL3, JL_LEAF3VALUEAREA,
			   JL_BRANCH_COPY_IMMED_ODD, JL_COPY3_LONG_TO_PINDEX);
	case cJL_JPBRANCH_U:
		level = cJL_ROOTSTATE;
		Pjp = P_JP(Pjp->jp_Addr) + JL_DIGITATSTATE(Index, cJL_ROOTSTATE);
		break;
// State transitions while deleting an Index, the inverse of the similar table
// that appears in JudyIns.c:
// Note:  In JudyIns.c this table is not needed and does not appear until the
// Immed handling code; because once a Leaf is reached upon growing the tree,
// the situation remains simpler, but for deleting indexes, the complexity
// arises when leaves must compress to Immeds.
// Note:  There are other transitions possible too, not shown here, such as to
// a leaf one level higher.
// (Yes, this is very terse...  Study it and it will make sense.)
// (Note, parts of this diagram are repeated below for quick reference.)
//                      reformat JP here for Judy1 only, from word-1 to word-2
//           JUDY1 && JL_64BIT   JUDY1 || JL_64BIT                     |
// (*) Leaf1 [[ => 1_15..08 ] => 1_07 => ... => 1_04 ] => 1_03 => 1_02 => 1_01
//     Leaf2 [[ => 2_07..04 ] => 2_03 => 2_02        ]                 => 2_01
//     Leaf3 [[ => 3_05..03 ] => 3_02                ]                 => 3_01
// JL_64BIT only:
//     Leaf4 [[ => 4_03..02 ]]                                         => 4_01
//     Leaf5 [[ => 5_03..02 ]]                                         => 5_01
//     Leaf6 [[ => 6_02     ]]                                         => 6_01
//     Leaf7 [[ => 7_02     ]]                                         => 7_01
// (*) For Judy1 & 64-bit, go directly from a LeafB1 to cJL_JPIMMED_1_15; skip
//     Leaf1, as described in Judy1.h regarding cJ1_JPLEAF1.
// MACROS FOR COMMON CODE:
// (De)compress a LeafX into a LeafY one index size (cIS) larger (X+1 = Y):
// This is only possible when the current leaf is under a narrow pointer
// ((ParentLevel - 1) > cIS) and its population fits in a higher-level leaf.
// Variables ParentLevel, pop1, PjllnewRaw, Pjllnew, Pjpm, and Index are in the
// context.
// Note:  Doing an "uplevel" doesnt occur until the old leaf can be compressed
// up one level BEFORE deleting an index; that is, hysteresis = 1.
// Note:  LeafType, MaxPop1, NewJPType, and Alloc refer to the up-level leaf,
// not the current leaf.
// Note:  010327:  Fixed bug where the jp_DcdPopO next-uplevel digit (byte)
// above the current Pop0 value was not being cleared.  When upleveling, one
// digit in jp_DcdPopO "moves" from being part of the Dcd subfield to the Pop0
// subfield, but since a leaf maxpop1 is known to be <= 1 byte in size, the new
// Pop0 byte should always be zero.  This is easy to overlook because
// JL_JPLEAF_POP0() "knows" to only use the LSB of Pop0 (for efficiency) and
// ignore the other bytes...  Until someone uses cJL_POP0MASK() instead of
// JL_JPLEAF_POP0(), such as in JudyInsertBranch.c.
#define JL_LEAF_UPLEVEL(cIS,LeafType,MaxPop1,NewJPType,LeafToLeaf,      \
                        Alloc,ValueArea)                                \
        assert(((ParentLevel - 1) == (cIS)) || (pop1 >= (MaxPop1)));    \
        if (((ParentLevel - 1) > (cIS))  /* under narrow pointer */     \
         && (pop1 == (MaxPop1))) {         /* hysteresis = 1       */     \
            Word_t D_cdP0;                                              \
            if ((PjllnewRaw = Alloc(MaxPop1, Pjpm)) == 0) return -1;   \
            Pjllnew = P_JLL(PjllnewRaw);                                \
	    Pjv     = ValueArea((LeafType) Pjllnew, MaxPop1);          \
            (void) LeafToLeaf((LeafType) Pjllnew, JL_PVALUEPASS Pjp,    \
                              Index & cJL_DCDMASK(cIS), /* TBD, Doug says */ \
                              (void *) Pjpm);                          \
            D_cdP0 = (~cJL_MASKATSTATE((cIS) + 1)) & JL_JPDCDPOP0(Pjp); \
            JL_JPSETADT(Pjp, (Word_t )PjllnewRaw, D_cdP0, NewJPType);    \
            goto ContinueDelWalk;       /* delete from new leaf */      \
        }

#define JL_LEAF_UPLEVEL64(cIS,LeafType,MaxPop1,NewJPType,LeafToLeaf,    \
                          Alloc,ValueArea)	// null.
#define JL_TOIMMED_01_EVEN(cIS,ignore1,ignore2)                         \
{                                                                       \
        Word_t D_cdP0;                                                 \
        Word_t A_ddr = 0;                                              \
        uint8_t T_ype = JL_JPTYPE(Pjp);                                 \
        offset = (Pleaf[0] == JL_LEASTBYTES(Index, cIS)); /* undeleted Ind */ \
        assert(Pleaf[offset ? 0 : 1] == JL_LEASTBYTES(Index, cIS));     \
        D_cdP0 = (Index & cJL_DCDMASK(cIS)) | Pleaf[offset];            \
	A_ddr = Pjv[offset];                                         \
        JL_JPSETADT(Pjp, A_ddr, D_cdP0, T_ype);                         \
}

#define JL_TOIMMED_01_ODD(cIS,SearchLeaf,CopyPIndex)                    \
        {                                                               \
            Word_t D_cdP0;                                             \
            Word_t A_ddr = 0;                                          \
            uint8_t T_ype = JL_JPTYPE(Pjp);                             \
            offset = SearchLeaf(Pleaf, 2, Index);                       \
            assert(offset >= 0);        /* Index must be valid */       \
            CopyPIndex(D_cdP0, & (Pleaf[offset ? 0 : cIS]));            \
            D_cdP0 |= Index & cJL_DCDMASK(cIS);                         \
	    A_ddr = Pjv[offset ? 0 : 1];                               \
            JL_JPSETADT(Pjp, A_ddr, D_cdP0, T_ype);                     \
        }

#define JL_LEAF_TOIMMED(cIS,LeafType,MaxPop1,BaseJPType,ignore1,\
                        ignore2,ignore3,ignore4,                \
                        DeleteCopy,FreeLeaf)                    \
        assert(pop1 > (MaxPop1));                               \
        if ((pop1 - 1) == (MaxPop1)) {    /* hysteresis = 0 */    \
            Pjll_t PjllRaw = (Pjll_t) (Pjp->jp_Addr);           \
            Pjv_t  PjvnewRaw;                                   \
            Pjv_t  Pjvnew;                                      \
                                                                \
            if ((PjvnewRaw = judyLAllocJV(pop1 - 1, Pjpm))    \
                == NULL) return -1;                    \
	    Pjvnew = P_JV(PjvnewRaw);                         \
                                                                \
            DeleteCopy((LeafType) (Pjp->jp_LIndex), Pleaf, pop1, offset, cIS); \
            JL_DELETECOPY(Pjvnew, Pjv, pop1, offset, cIS);      \
            FreeLeaf(PjllRaw, pop1, Pjpm);                      \
            Pjp->jp_Addr = (Word_t ) PjvnewRaw;                  \
            Pjp->jp_Type = (BaseJPType) - 2 + (MaxPop1);        \
            return 1;                                          \
        }
#define JL_LEAF_TOIMMED_01(cIS,LeafType,MaxPop1,ignore,Immed01JPType,   \
                           ToImmed,SearchLeaf,CopyPIndex,               \
                           DeleteCopy,FreeLeaf)                         \
        assert(pop1 > (MaxPop1));                                       \
        if ((pop1 - 1) == (MaxPop1)) {    /* hysteresis = 0 */            \
            Pjll_t PjllRaw = (Pjll_t) (Pjp->jp_Addr);                   \
            ToImmed(cIS, SearchLeaf, CopyPIndex);                       \
            FreeLeaf(PjllRaw, pop1, Pjpm);                              \
            Pjp->jp_Type = (Immed01JPType);                             \
            return 1;                                                  \
        }
#define JL_LEAF_TOIMMED_23(cIS,LeafType,MaxPop1,BaseJPType,Immed01JPType, \
                           ToImmed,SearchLeaf,CopyPIndex,               \
                           DeleteCopy,FreeLeaf)                         \
        JL_LEAF_TOIMMED_01(cIS,LeafType,MaxPop1,ignore,Immed01JPType,   \
                           ToImmed,SearchLeaf,CopyPIndex,               \
                           DeleteCopy,FreeLeaf)
#define JL_LEAF_INPLACE(cIS,GrowInPlace,DeleteInPlace)          \
        if (GrowInPlace(pop1 - 1)) {      /* hysteresis = 0 */    \
            DeleteInPlace(Pleaf, pop1, offset, cIS);            \
	    JL_DELETEINPLACE(Pjv, pop1, offset, ignore);        \
            return 1;                                          \
        }

#define JL_LEAF_SHRINK(cIS,LeafType,DeleteCopy,Alloc,FreeLeaf,ValueArea) \
        {                                                               \
	    Pjv_t Pjvnew;                                               \
            if ((PjllnewRaw = Alloc(pop1 - 1, Pjpm)) == 0) return -1;  \
            Pjllnew = P_JLL(PjllnewRaw);                                \
	    Pjvnew  = ValueArea(Pjllnew, pop1 - 1);                     \
            DeleteCopy((LeafType) Pjllnew, Pleaf, pop1, offset, cIS);   \
            JL_DELETECOPY(Pjvnew, Pjv, pop1, offset, cIS);              \
            FreeLeaf(PleafRaw, pop1, Pjpm);                             \
            Pjp->jp_Addr = (Word_t ) PjllnewRaw;                         \
            return 1;                                                  \
        }

#define JL_LEAF(cIS, UpLevel, LeafTypeUp,MaxPop1Up,LeafJPTypeUp,LeafToLeaf, \
                  AllocUp,ValueAreaUp,                                  \
                LeafToImmed,ToImmed,CopyPIndex,                         \
                  LeafType,ImmedMaxPop1,ImmedBaseJPType,Immed01JPType,  \
                  SearchLeaf,GrowInPlace,DeleteInPlace,DeleteCopy,      \
                  Alloc,FreeLeaf,ValueArea)                             \
        {                                                               \
            Pjll_t   PleafRaw;                                          \
            LeafType Pleaf;                                             \
            assert(! JL_DCDNOTMATCHINDEX(Index, Pjp, cIS));             \
            assert(ParentLevel > (cIS));                                \
            PleafRaw = (Pjll_t) (Pjp->jp_Addr);                         \
            Pleaf    = (LeafType) P_JLL(PleafRaw);                      \
            pop1     = JL_JPLEAF_POP0(Pjp) + 1;                         \
                                                                        \
            UpLevel(cIS, LeafTypeUp, MaxPop1Up, LeafJPTypeUp,           \
                    LeafToLeaf, AllocUp, ValueAreaUp);                  \
            offset = SearchLeaf(Pleaf, pop1, Index);                    \
            assert(offset >= 0);        /* Index must be valid */       \
	    Pjv = ValueArea(Pleaf, pop1);                              \
            LeafToImmed(cIS, LeafType, ImmedMaxPop1,                    \
                        ImmedBaseJPType, Immed01JPType,                 \
                        ToImmed, SearchLeaf, CopyPIndex,                \
                        DeleteCopy, FreeLeaf);                          \
            JL_LEAF_INPLACE(cIS, GrowInPlace, DeleteInPlace);           \
            JL_LEAF_SHRINK(cIS, LeafType, DeleteCopy, Alloc, FreeLeaf,  \
                           ValueArea);                                  \
        }

	// END OF MACROS, START OF CASES:
	// (*) Leaf1 [[ => 1_15..08 ] => 1_07 => ... => 1_04 ] => 1_03 => 1_02 => 1_01
	case cJL_JPLEAF1:
		JL_LEAF(1, JL_LEAF_UPLEVEL, uint16_t *, cJL_LEAF2_MAXPOP1, cJL_JPLEAF2,
			judyLeaf1ToLeaf2, judyLAllocJLL2, JL_LEAF2VALUEAREA,
			JL_LEAF_TOIMMED, ignore, ignore,
			uint8_t *, cJL_IMMED1_MAXPOP1,
			cJL_JPIMMED_1_02, cJL_JPIMMED_1_01, judySearchLeaf1,
			JL_LEAF1GROWINPLACE, JL_DELETEINPLACE, JL_DELETECOPY,
			judyLAllocJLL1, judyLFreeJLL1, JL_LEAF1VALUEAREA);
	// A complicating factor is that for JudyL & 32-bit, a Leaf2 must go directly
	// to an Immed 2_01 and a Leaf3 must go directly to an Immed 3_01:
	// Leaf2 [[ => 2_07..04 ] => 2_03 => 2_02 ] => 2_01
	// Leaf3 [[ => 3_05..03 ] => 3_02         ] => 3_01
	// Hence use JL_LEAF_TOIMMED_23 instead of JL_LEAF_TOIMMED in the cases below,
	// and also the parameters ToImmed and, for odd index sizes, CopyPIndex, are
	// required.
	case cJL_JPLEAF2:
		JL_LEAF(2, JL_LEAF_UPLEVEL, uint8_t *, cJL_LEAF3_MAXPOP1, cJL_JPLEAF3,
			judyLeaf2ToLeaf3, judyLAllocJLL3, JL_LEAF3VALUEAREA,
			JL_LEAF_TOIMMED_23, JL_TOIMMED_01_EVEN, ignore,
			uint16_t *, cJL_IMMED2_MAXPOP1,
			cJL_JPIMMED_2_02, cJL_JPIMMED_2_01, judySearchLeaf2,
			JL_LEAF2GROWINPLACE, JL_DELETEINPLACE, JL_DELETECOPY,
			judyLAllocJLL2, judyLFreeJLL2, JL_LEAF2VALUEAREA);
	case cJL_JPLEAF3:
		JL_LEAF(3, JL_LEAF_UPLEVEL64, uint32_t *, cJL_LEAF4_MAXPOP1,
			cJL_JPLEAF4, judyLLeaf3ToLeaf4, judyLAllocJLL4, JL_LEAF4VALUEAREA,
			JL_LEAF_TOIMMED_23, JL_TOIMMED_01_ODD, JL_COPY3_PINDEX_TO_LONG,
			uint8_t *, cJL_IMMED3_MAXPOP1, cJL_JPIMMED_3_02, cJL_JPIMMED_3_01, 
			judySearchLeaf3, JL_LEAF3GROWINPLACE, JL_DELETEINPLACE_ODD,
			JL_DELETECOPY_ODD, judyLAllocJLL3, judyLFreeJLL3, JL_LEAF3VALUEAREA);
	case cJL_JPLEAF_B1: {
		Pjv_t PjvnewRaw;	// new value area.
		Pjv_t Pjvnew;
		Word_t subexp;	// 1 of 8 subexpanses in bitmap.
		Pjlb_t Pjlb;	// pointer to bitmap part of the leaf.
		BITMAPL_t bitmap;	// for one subexpanse.
		BITMAPL_t bitmask;	// bit set for Indexs digit.
		assert(!JL_DCDNOTMATCHINDEX(Index, Pjp, 1));
		assert(ParentLevel > 1);
		assert(JL_BITMAPTESTL(P_JLB(Pjp->jp_Addr), Index));
		pop1 = JL_JPLEAF_POP0(Pjp) + 1;

		JL_LEAF_UPLEVEL(1, uint16_t *, cJL_LEAF2_MAXPOP1, cJL_JPLEAF2,
				judyLeaf1ToLeaf2, judyLAllocJLL2, JL_LEAF2VALUEAREA);
		if (pop1 == cJL_LEAF1_MAXPOP1) {	// hysteresis = 1.
			if (judyLeafB1ToLeaf1(Pjp, Pjpm) == -1)
				return -1;
			goto ContinueDelWalk;	// delete Index in new Leaf1.
		}

		digit = JL_DIGITATSTATE(Index, 1);
		Pjlb = P_JLB(Pjp->jp_Addr);

		subexp = digit / cJL_BITSPERSUBEXPL;	// which subexpanse.
		bitmap = JL_JLB_BITMAP(Pjlb, subexp);	// subexps 32-bit map.
		PjvRaw = JL_JLB_PVALUE(Pjlb, subexp);	// corresponding values.
		Pjv = P_JV(PjvRaw);
		bitmask = JL_BITPOSMASKL(digit);	// mask for Index.

		assert(bitmap & bitmask);	// Index must be valid.
		if (bitmap == cJL_FULLBITMAPL) {	// full bitmap, take shortcut:
			pop1 = cJL_BITSPERSUBEXPL;
			offset = digit % cJL_BITSPERSUBEXPL;
		} else { // compute subexpanse pop1 and value area offset:
			pop1 = judyCountBits(bitmap);
			offset = judyCountBits(bitmap & (bitmask - 1));
		}

		if (pop1 == 1) {
			judyLFreeJV(PjvRaw, 1, Pjpm);
			JL_JLB_PVALUE(Pjlb, subexp) = NULL;
			JL_JLB_BITMAP(Pjlb, subexp) = 0;
			return 1;
		}
		// Shrink value area in place or move to a smaller value area:
		if (JL_LEAFVGROWINPLACE(pop1 - 1)) {	// hysteresis = 0.
			JL_DELETEINPLACE(Pjv, pop1, offset, ignore);
		} else {
			if ((PjvnewRaw = judyLAllocJV(pop1 - 1, Pjpm)) == NULL)
				return -1;
			Pjvnew = P_JV(PjvnewRaw);

			JL_DELETECOPY(Pjvnew, Pjv, pop1, offset, ignore);
			judyLFreeJV(PjvRaw, pop1, Pjpm);
			JL_JLB_PVALUE(Pjlb, subexp) = (Pjv_t) PjvnewRaw;
		}

		JL_JLB_BITMAP(Pjlb, subexp) ^= bitmask;	// clear Indexs bit.
		return 1;
	}

#define JL_IMMED_01(NewJPType,ParentJPType)                             \
            assert(JL_JPDCDPOP0(Pjp) == JL_TRIMTODCDSIZE(Index));       \
            JL_JPSETADT(Pjp, 0, 0, NewJPType);                          \
            return 1

#define JL_IMMED_02(cIS,LeafType,NewJPType)             \
        {                                               \
            LeafType Pleaf;                             \
            assert((ParentLevel - 1) == (cIS));         \
	    Pleaf  = (LeafType) (Pjp->jp_LIndex);      \
	    PjvRaw = (Pjv_t) (Pjp->jp_Addr);           \
	    Pjv    = P_JV(PjvRaw);                     \
            JL_TOIMMED_01_EVEN(cIS, ignore, ignore);    \
	    judyLFreeJV(PjvRaw, 2, Pjpm);            \
            Pjp->jp_Type = (NewJPType);                 \
            return 1;                                  \
        }

#define JL_IMMED_DEL(cIS,DeleteInPlace)                         \
        if (JL_LEAFVGROWINPLACE(pop1 - 1)) { /* hysteresis = 0 */ \
            DeleteInPlace(   Pleaf,  pop1, offset, cIS);        \
            JL_DELETEINPLACE(Pjv, pop1, offset, ignore);        \
	} else {                                                \
            Pjv_t PjvnewRaw;                                    \
            Pjv_t Pjvnew;                                       \
                                                                \
            if ((PjvnewRaw = judyLAllocJV(pop1 - 1, Pjpm))    \
                == NULL) return -1;                    \
            Pjvnew = P_JV(PjvnewRaw);                           \
            DeleteInPlace(Pleaf, pop1, offset, cIS);            \
            JL_DELETECOPY(Pjvnew, Pjv, pop1, offset, ignore);   \
            judyLFreeJV(PjvRaw, pop1, Pjpm);                  \
            (Pjp->jp_Addr) = (Word_t ) PjvnewRaw;                \
        }

#define JL_IMMED(cIS,LeafType,BaseJPType,SearchLeaf,DeleteInPlace)      \
        {                                                               \
            LeafType Pleaf;                                             \
            assert((ParentLevel - 1) == (cIS));                         \
	    Pleaf  = (LeafType) (Pjp->jp_LIndex);                      \
	    PjvRaw = (Pjv_t) (Pjp->jp_Addr);                           \
	    Pjv    = P_JV(PjvRaw);                                     \
            pop1   = (JL_JPTYPE(Pjp)) - (BaseJPType) + 2;               \
            offset = SearchLeaf(Pleaf, pop1, Index);                    \
            assert(offset >= 0);        /* Index must be valid */       \
            JL_IMMED_DEL(cIS, DeleteInPlace);                           \
            --(Pjp->jp_Type);                                           \
            return 1;                                                  \
        }

	case cJL_JPIMMED_1_01:
		JL_IMMED_01(cJL_JPNULL1, cJL_JPBRANCH_U2);
	case cJL_JPIMMED_2_01:
		JL_IMMED_01(cJL_JPNULL2, cJL_JPBRANCH_U3);
	case cJL_JPIMMED_3_01:
		JL_IMMED_01(cJL_JPNULL3, cJL_JPBRANCH_U);
	case cJL_JPIMMED_1_02:
		JL_IMMED_02(1, uint8_t *, cJL_JPIMMED_1_01);
	case cJL_JPIMMED_1_03:
		JL_IMMED(1, uint8_t *, cJL_JPIMMED_1_02,
			 judySearchLeaf1, JL_DELETEINPLACE);
	default:
		JL_SET_ERRNO(JL_ERRNO_CORRUPT);
		return -1;
	}			// switch

	assert(level);
	retcode = judyDelWalk(Pjp, Index, level, Pjpm);
	assert(retcode != 0);	// should never happen.
	if ((JL_JPTYPE(Pjp)) < cJL_JPIMMED_1_01) {	// not an Immed.
		switch (retcode) {
		case 1: {
			jp_t JP = *Pjp;
			Word_t DcdP0;

			DcdP0 = JL_JPDCDPOP0(Pjp) - 1;	// decrement count.
			JL_JPSETADT(Pjp, JP.jp_Addr, DcdP0, JL_JPTYPE(&JP));
			break;
		}
		case 2:	{// collapse BranchL to single JP; see above:
			Pjbl_t PjblRaw = (Pjbl_t) (Pjp->jp_Addr);
			Pjbl_t Pjbl = P_JBL(PjblRaw);

			*Pjp = Pjbl->jbl_jp[0];
			judyLFreeJBL(PjblRaw, Pjpm);
			retcode = 1;
		}
		}
	}

	return retcode;
}

int JudyLDel(void **PPArray, uint32_t Index) 
{
	Word_t pop1;		// population of leaf.
	int offset;		// at which to delete Index.
	void **PPvalue;	// pointer from JudyLGet().

	if (PPArray == NULL) {
		JL_SET_ERRNO(JL_ERRNO_NULLPPARRAY);
		return JERR;
	}

	if ((PPvalue = JudyLGet(*PPArray, Index)) == PPJERR)
		return JERR;
	if (PPvalue == NULL)
		return 0;

	if (JL_LEAFW_POP0(*PPArray) < cJL_LEAFW_MAXPOP1) {	// must be a LEAFW
		Pjv_t Pjv;	// current value area.
		Pjv_t Pjvnew;	// value area in new leaf.
		Pjlw_t Pjlw = P_JLW(*PPArray);	// first word of leaf.
		Pjlw_t Pjlwnew;	// replacement leaf.
		pop1 = Pjlw[0] + 1;	// first word of leaf is pop0.

		// Delete single (last) Index from array:
		if (pop1 == 1) {
			judyLFreeJLW(Pjlw, /* pop1 = */ 1, NULL);
			*PPArray = NULL;
			return 1;
		}

		// Locate Index in compressible leaf:
		offset = judySearchLeafW(Pjlw + 1, pop1, Index);
		assert(offset >= 0);	// Index must be valid.

		Pjv = JL_LEAFWVALUEAREA(Pjlw, pop1);

		if (JL_LEAFWGROWINPLACE(pop1 - 1)) {
			JL_DELETEINPLACE(Pjlw + 1, pop1, offset, ignore);
			JL_DELETEINPLACE(Pjv, pop1, offset, ignore);
			--(Pjlw[0]);	// decrement population.
			return 1;
		}
		// Allocate new leaf for use in either case below:
		Pjlwnew = judyLAllocJLW(pop1 - 1);
		JL_CHECKALLOC(Pjlw_t, Pjlwnew, JERR);

		// Shrink to smaller LEAFW:
		// Note:  Skip the first word = pop0 in each leaf.
		Pjlwnew[0] = (pop1 - 1) - 1;
		JL_DELETECOPY(Pjlwnew + 1, Pjlw + 1, pop1, offset, ignore);

		Pjvnew = JL_LEAFWVALUEAREA(Pjlwnew, pop1 - 1);
		JL_DELETECOPY(Pjvnew, Pjv, pop1, offset, ignore);

		judyLFreeJLW(Pjlw, pop1, NULL);

		*PPArray = (void *) Pjlwnew;
		return 1;
	} else {
		Pjpm_t Pjpm;
		Pjp_t Pjp;	// top-level JP to process.
		Word_t digit;	// in a branch.
		Pjv_t Pjv;	// to value area.
		Pjlw_t Pjlwnew;	// replacement leaf.
		Pjpm = P_JPM(*PPArray);	// top object in array (tree).
		Pjp = &(Pjpm->jpm_JP);	// next object (first branch or leaf).

		assert(((Pjpm->jpm_JP.jp_Type) == cJL_JPBRANCH_L)
		       || ((Pjpm->jpm_JP.jp_Type) == cJL_JPBRANCH_B)
		       || ((Pjpm->jpm_JP.jp_Type) == cJL_JPBRANCH_U));

		if (judyDelWalk(Pjp, Index, cJL_ROOTSTATE, Pjpm) == -1)
			return JERR;

		--(Pjpm->jpm_Pop0);	// success; decrement total population.

		if ((Pjpm->jpm_Pop0 + 1) != cJL_LEAFW_MAXPOP1)
			return 1;

		Pjlwnew = judyLAllocJLW(cJL_LEAFW_MAXPOP1);
		JL_CHECKALLOC(Pjlw_t, Pjlwnew, JERR);

		// Plug leaf into root pointer and set population count:
		*PPArray = (void *) Pjlwnew;
		Pjv = JL_LEAFWVALUEAREA(Pjlwnew, cJL_LEAFW_MAXPOP1);
		*Pjlwnew++ = cJL_LEAFW_MAXPOP1 - 1;	// set pop0.

		switch (JL_JPTYPE(Pjp)) {
		// JPBRANCH_L:  Copy each JPs indexes to the new LEAFW and free the old branch:
		case cJL_JPBRANCH_L: {
			Pjbl_t PjblRaw = (Pjbl_t) (Pjp->jp_Addr);
			Pjbl_t Pjbl = P_JBL(PjblRaw);

			for (offset = 0; offset < Pjbl->jbl_NumJPs; ++offset) {
				pop1 = judyLeafM1ToLeafW(Pjlwnew, JL_PVALUEPASS
					(Pjbl->jbl_jp) + offset, 
					JL_DIGITTOSTATE(Pjbl->jbl_Expanse [offset],
					cJL_BYTESPERWORD), (void *) Pjpm);
				Pjlwnew += pop1;	// advance through indexes.
				Pjv += pop1;	// advance through values.
			}
			judyLFreeJBL(PjblRaw, Pjpm);

			break;	// delete Index from new LEAFW.
		}

		case cJL_JPBRANCH_B: {
			Pjbb_t PjbbRaw = (Pjbb_t) (Pjp->jp_Addr);
			Pjbb_t Pjbb = P_JBB(PjbbRaw);
			Word_t subexp;	// current subexpanse number.
			BITMAPB_t bitmap;	// portion for this subexpanse.
			Pjp_t Pjp2Raw;	// one subexpanses subarray.
			Pjp_t Pjp2;

			for (subexp = 0; subexp < cJL_NUMSUBEXPB; ++subexp) {
				if ((bitmap = JL_JBB_BITMAP(Pjbb, subexp)) == 0)
					continue;	// skip empty subexpanse.

				digit = subexp * cJL_BITSPERSUBEXPB;
				Pjp2Raw = JL_JBB_PJP(Pjbb, subexp);
				Pjp2 = P_JP(Pjp2Raw);
				assert(Pjp2 != NULL);
			// Walk through bits for all possible sub-subexpanses (digits); increment
			// offset for each populated subexpanse; until no more set bits:
				for (offset = 0; bitmap != 0;
				     bitmap >>= 1, ++digit) {
					if (!(bitmap & 1))	// skip empty sub-subexpanse.
						continue;
					pop1 = judyLeafM1ToLeafW(Pjlwnew, JL_PVALUEPASS Pjp2 + offset,
						JL_DIGITTOSTATE(digit, cJL_BYTESPERWORD), (void *) Pjpm);
					Pjlwnew += pop1;	// advance through indexes.
					Pjv += pop1;	// advance through values.
					++offset;
				}
				judyLFreeJBBJP(Pjp2Raw, /* pop1 = */ offset, Pjpm);
			}
			judyLFreeJBB(PjbbRaw, Pjpm);

			break;	// delete Index from new LEAFW.
		}
		case cJL_JPBRANCH_U: {
			Pjbu_t PjbuRaw = (Pjbu_t) (Pjp->jp_Addr);
			Pjbu_t Pjbu = P_JBU(PjbuRaw);
			Word_t ldigit;	// larger than uint8_t.

			for (Pjp = Pjbu->jbu_jp, ldigit = 0; ldigit < cJL_BRANCHUNUMJPS; ++Pjp, ++ldigit) {
				// Shortcuts, to save a little time for possibly big branches:
				if ((JL_JPTYPE(Pjp)) == cJL_JPNULLMAX)	// skip null JP.
					continue;
				if ((JL_JPTYPE(Pjp)) == cJL_JPIMMED_3_01) {	// single Immed:
					*Pjlwnew++ = JL_DIGITTOSTATE(ldigit, cJL_BYTESPERWORD)
							| JL_JPDCDPOP0(Pjp);	// rebuild Index.
					*Pjv++ = Pjp->jp_Addr;	// copy value area.
					continue;
				}

				pop1 = judyLeafM1ToLeafW(Pjlwnew, JL_PVALUEPASS Pjp,
							 JL_DIGITTOSTATE(ldigit, cJL_BYTESPERWORD),
							 (void *) Pjpm);
				Pjlwnew += pop1;	// advance through indexes.
				Pjv += pop1;	// advance through values.
			}
			judyLFreeJBU(PjbuRaw, Pjpm);
			break;	// delete Index from new LEAFW.
		}
		default:
			JL_SET_ERRNO(JL_ERRNO_CORRUPT);
			return JERR;
		}		// end switch on sub-JP type.
		judyLFreeJPM(Pjpm, NULL);
		return 1;
	}
}
