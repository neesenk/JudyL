#include "JudyL.h"

extern int judyCreateBranchB(Pjp_t, Pjp_t, uint8_t *, Word_t, void *);
extern int judyCreateBranchU(Pjp_t, void *);
extern int judyCascade1(Pjp_t, void *);
extern int judyCascade2(Pjp_t, void *);
extern int judyCascade3(Pjp_t, void *);
extern int judyCascadeL(Pjp_t, void *);
extern int judyInsertBranch(Pjp_t Pjp, Word_t Index, Word_t Btype, Pjpm_t);

#define JL_CHECK_IF_OUTLIER(Pjp, Index, cLevel, Pjpm)                   \
        if (JL_DCDNOTMATCHINDEX(Index, Pjp, cLevel))                    \
            return (judyInsertBranch(Pjp, Index, cLevel, Pjpm))

#define JL_CHECK_IF_EXISTS(Offset,Pjv,Pjpm) do {        \
        if ((Offset) >= 0) {				\
                (Pjpm)->jpm_PValue = (Pjv) + (Offset);  \
                return 0;                              \
        }						\
        (Offset) = ~(Offset);				\
} while (0)

static int judyInsWalk(Pjp_t Pjp, Word_t Index, Pjpm_t Pjpm)
{
	uint8_t digit;		// from Index, current offset into a branch.
	jp_t newJP;		// for creating a new Immed JP.
	Word_t exppop1;		// expanse (leaf) population.
	int retcode;		// return codes:  -1, 0, 1.

ContinueInsWalk:		// for modifying state without recursing.
	switch (JL_JPTYPE(Pjp))	{// entry:  Pjp, Index.
	case cJL_JPNULL1:
	case cJL_JPNULL2:
	case cJL_JPNULL3:
		assert((Pjp->jp_Addr) == 0);
		JL_JPSETADT(Pjp, 0, Index, JL_JPTYPE(Pjp) + cJL_JPIMMED_1_01 - cJL_JPNULL1);
		Pjpm->jpm_PValue = (Pjv_t)(&Pjp->jp_Addr);
		return 1;
#define JL_BRANCH_OUTLIER(DIGIT,POP1,cLEVEL,PJP,INDEX,PJPM)  \
        JL_CHECK_IF_OUTLIER(PJP, INDEX, cLEVEL, PJPM);       \
        (DIGIT) = JL_DIGITATSTATE(INDEX, cLEVEL);            \
        (POP1)  = JL_JPBRANCH_POP0(PJP, cLEVEL)
	case cJL_JPBRANCH_L2:
		JL_BRANCH_OUTLIER(digit, exppop1, 2, Pjp, Index, Pjpm);
		goto JudyBranchL;
	case cJL_JPBRANCH_L3:
		JL_BRANCH_OUTLIER(digit, exppop1, 3, Pjp, Index, Pjpm);
		goto JudyBranchL;
	case cJL_JPBRANCH_L: {
		Pjbl_t PjblRaw;	// pointer to old linear branch.
		Pjbl_t Pjbl;
		Pjbu_t PjbuRaw;	// pointer to new uncompressed branch.
		Pjbu_t Pjbu;
		Word_t numJPs;	// number of JPs = populated expanses.
		int offset;	// in branch.

		digit = JL_DIGITATSTATE(Index, cJL_ROOTSTATE);
		exppop1 = Pjpm->jpm_Pop0;

	      JudyBranchL:
		PjblRaw = (Pjbl_t) (Pjp->jp_Addr);
		Pjbl = P_JBL(PjblRaw);
		if (exppop1 > JL_BRANCHL_MAX_POP)
			goto ConvertBranchLtoU;
		numJPs = Pjbl->jbl_NumJPs;
		if ((numJPs == 0) || (numJPs > cJL_BRANCHLMAXJPS)) {
			JL_SET_ERRNO(JL_ERRNO_CORRUPT);
			return -1;
		}
		offset = judySearchLeaf1((Pjll_t) (Pjbl->jbl_Expanse), numJPs, digit);
		if (offset >= 0) {
			Pjp = (Pjbl->jbl_jp) + offset;
			break;
		}
		if (numJPs < cJL_BRANCHLMAXJPS) {
			offset = ~offset;
			JL_JPSETADT(&newJP, 0, Index, 
				    JL_JPTYPE(Pjp) + cJL_JPIMMED_1_01 - cJL_JPBRANCH_L2);

			JL_INSERTINPLACE(Pjbl->jbl_Expanse, numJPs, offset, digit);
			JL_INSERTINPLACE(Pjbl->jbl_jp, numJPs, offset, newJP);
			++(Pjbl->jbl_NumJPs);
			// value area is first word of new Immed 01 JP:
			Pjpm->jpm_PValue = (Pjv_t) ((Pjbl->jbl_jp) + offset);
			return 1;
		}

		assert((numJPs) <= cJL_BRANCHLMAXJPS);
		if (judyCreateBranchB(Pjp, Pjbl->jbl_jp, Pjbl->jbl_Expanse, numJPs, Pjpm) == -1)
			return -1;
		Pjp->jp_Type += cJL_JPBRANCH_B - cJL_JPBRANCH_L;
		judyLFreeJBL(PjblRaw, Pjpm);	// free old BranchL.
		goto ContinueInsWalk;
	      ConvertBranchLtoU:
		if ((PjbuRaw = judyLAllocJBU(Pjpm)) == NULL)
			return -1;
		Pjbu = P_JBU(PjbuRaw);

		JL_JPSETADT(&newJP, 0, 0, JL_JPTYPE(Pjp) - cJL_JPBRANCH_L2 + cJL_JPNULL1);

		for (numJPs = 0; numJPs < cJL_BRANCHUNUMJPS; ++numJPs)
			Pjbu->jbu_jp[numJPs] = newJP;

		for (numJPs = 0; numJPs < Pjbl->jbl_NumJPs; ++numJPs) {
			Pjp_t Pjp1 = &(Pjbl->jbl_jp[numJPs]);
			offset = Pjbl->jbl_Expanse[numJPs];
			Pjbu->jbu_jp[offset] = *Pjp1;
		}
		judyLFreeJBL(PjblRaw, Pjpm);
		Pjp->jp_Addr = (Word_t) PjbuRaw;
		Pjp->jp_Type += cJL_JPBRANCH_U - cJL_JPBRANCH_L;
		Pjpm->jpm_LastUPop0 = Pjpm->jpm_Pop0;
		goto ContinueInsWalk;
	}

	case cJL_JPBRANCH_B2:
		JL_BRANCH_OUTLIER(digit, exppop1, 2, Pjp, Index, Pjpm);
		goto JudyBranchB;
	case cJL_JPBRANCH_B3:
		JL_BRANCH_OUTLIER(digit, exppop1, 3, Pjp, Index, Pjpm);
		goto JudyBranchB;
	case cJL_JPBRANCH_B: {
		Pjbb_t Pjbb;	// pointer to bitmap branch.
		Pjbb_t PjbbRaw;	// pointer to bitmap branch.
		Pjp_t Pjp2Raw;	// 1 of N arrays of JPs.
		Pjp_t Pjp2;	// 1 of N arrays of JPs.
		Word_t subexp;	// 1 of N subexpanses in bitmap.
		BITMAPB_t bitmap;	// for one subexpanse.
		BITMAPB_t bitmask;	// bit set for Indexs digit.
		Word_t numJPs;	// number of JPs = populated expanses.
		int offset;	// in bitmap branch.

		digit = JL_DIGITATSTATE(Index, cJL_ROOTSTATE);
		exppop1 = Pjpm->jpm_Pop0;
	      JudyBranchB:
		if ((Pjpm->jpm_Pop0 - Pjpm->jpm_LastUPop0) > JL_BTOU_POP_INCREMENT) {
			if (Pjpm->jpm_Pop0 > JL_BRANCHB_MAX_POP) {
				if (exppop1 > JL_BRANCHB_MIN_POP) {
					if (judyCreateBranchU(Pjp, Pjpm) == -1)
						return -1;
					Pjpm->jpm_LastUPop0 = Pjpm->jpm_Pop0;
					goto ContinueInsWalk;
				}
			}
		}

		PjbbRaw = (Pjbb_t) (Pjp->jp_Addr);
		Pjbb = P_JBB(PjbbRaw);

		// Form the Int32 offset, and Bit offset values:
		// 8 bit Decode | 7 | 6 | 5 | 4 | 3 | 2 | 1 | 0 |
		//              |SubExpanse |    Bit offset     |
		// Get the 1 of 8 expanses from digit, Bits 5..7 = 1 of 8, and get the 32-bit
		// word that may have a bit set:
		subexp = digit / cJL_BITSPERSUBEXPB;
		bitmap = JL_JBB_BITMAP(Pjbb, subexp);
		Pjp2Raw = JL_JBB_PJP(Pjbb, subexp);
		Pjp2 = P_JP(Pjp2Raw);

		bitmask = JL_BITPOSMASKB(digit);
		offset = judyCountBits(bitmap & (bitmask - 1));
		if (bitmap & bitmask) {
			Pjp = Pjp2 + offset;
			break;
		}

		JL_JPSETADT(&newJP, 0, Index, JL_JPTYPE(Pjp) + cJL_JPIMMED_1_01 - cJL_JPBRANCH_B2);
		Pjp2Raw = JL_JBB_PJP(Pjbb, subexp);
		Pjp2 = P_JP(Pjp2Raw);
		numJPs = judyCountBits(bitmap);

		if (JL_BRANCHBJPGROWINPLACE(numJPs)) {
			assert(numJPs > 0);
			JL_INSERTINPLACE(Pjp2, numJPs, offset, newJP);
			Pjpm->jpm_PValue = (Pjv_t) (Pjp2 + offset);
		} else {
			Pjp_t PjpnewRaw;
			Pjp_t Pjpnew;
			if ((PjpnewRaw = judyLAllocJBBJP(numJPs + 1, Pjpm)) == 0)
				return -1;
			Pjpnew = P_JP(PjpnewRaw);
			if (numJPs) {
				JL_INSERTCOPY(Pjpnew, Pjp2, numJPs, offset, newJP);
				judyLFreeJBBJP(Pjp2Raw, numJPs, Pjpm);
				Pjpm->jpm_PValue = (Pjv_t) (Pjpnew + offset);
			} else {
				assert(JL_JBB_PJP(Pjbb, subexp) == NULL);
				Pjp = Pjpnew;
				*Pjp = newJP;
				Pjpm->jpm_PValue = (Pjv_t) (&(Pjp->jp_Addr));
			}
			JL_JBB_PJP(Pjbb, subexp) = PjpnewRaw;
		}

		JL_JBB_BITMAP(Pjbb, subexp) |= bitmask;
		return 1;
	}
#define JL_JBU_PJP_SUBEXP(Pjp,PSubExp,Index,Level) {            \
            uint8_t digit = JL_DIGITATSTATE(Index, Level);      \
            Pjbu_t  P_jbu  = P_JBU((Pjp)->jp_Addr);             \
            (Pjp) = &(P_jbu->jbu_jp[digit]);                    \
        }
	case cJL_JPBRANCH_U2:
		JL_CHECK_IF_OUTLIER(Pjp, Index, 2, Pjpm);
		JL_JBU_PJP_SUBEXP(Pjp, PSubExp, Index, 2);
		break;
	case cJL_JPBRANCH_U3:
		JL_JBU_PJP_SUBEXP(Pjp, PSubExp, Index, 3);
		break;
	case cJL_JPBRANCH_U:
		JL_JBU_PJP_SUBEXP(Pjp, PSubExp, Index, cJL_ROOTSTATE);
		break;
#define JL_LEAFPREP(cIS,Type,MaxPop1,ValueArea)         \
        Pjll_t  PjllRaw;                                \
        Type    Pleaf;  /* specific type */             \
        int     offset;                                 \
	Pjv_t	Pjv;					\
                                                        \
        JL_CHECK_IF_OUTLIER(Pjp, Index, cIS, Pjpm);     \
                                                        \
        exppop1 = JL_JPLEAF_POP0(Pjp) + 1;              \
        assert(exppop1 <= (MaxPop1));                   \
        PjllRaw = (Pjll_t) (Pjp->jp_Addr);              \
        Pleaf   = (Type) P_JLL(PjllRaw);                \
	Pjv	= ValueArea(Pleaf, exppop1)

#define JL_LEAFGROWVALUEADD(Pjv,ExpPop1,Offset)         \
        JL_INSERTINPLACE(Pjv, ExpPop1, Offset, 0);      \
        Pjpm->jpm_PValue = (Pjv) + (Offset)

#define JL_LEAFGROWVALUENEW(ValueArea,Pjv,ExpPop1,Offset) {             \
            Pjv_t Pjvnew = ValueArea(Pleafnew, (ExpPop1) + 1);          \
            JL_INSERTCOPY(Pjvnew, Pjv, ExpPop1, Offset, 0);             \
            Pjpm->jpm_PValue = (Pjvnew) + (Offset);                     \
        }

#define JL_LEAFGROW(cIS,Type,MaxPop1,Search,ValueArea,GrowInPlace,      \
                    InsertInPlace,InsertCopy,Alloc,Free)                \
                                                                        \
        offset = Search(Pleaf, exppop1, Index);                         \
        JL_CHECK_IF_EXISTS(offset, Pjv, Pjpm);                          \
                                                                        \
        if (GrowInPlace(exppop1)) {      /* add to current leaf */      \
            InsertInPlace(Pleaf, exppop1, offset, Index);               \
            JL_LEAFGROWVALUEADD(Pjv, exppop1, offset);                  \
            return 1;                                                  \
        }                                                               \
                                                                        \
        if (exppop1 < (MaxPop1)) {        /* grow to new leaf */        \
            Pjll_t PjllnewRaw;                                          \
            Type   Pleafnew;                                            \
            if ((PjllnewRaw = Alloc(exppop1 + 1, Pjpm)) == 0)		\
		    return -1;						\
            Pleafnew = (Type) P_JLL(PjllnewRaw);                        \
            InsertCopy(Pleafnew, Pleaf, exppop1, offset, Index);        \
            JL_LEAFGROWVALUENEW(ValueArea, Pjv, exppop1, offset);       \
            Free(PjllRaw, exppop1, Pjpm);                               \
            (Pjp->jp_Addr) = (Word_t) PjllnewRaw;                     \
            return 1;                                                  \
        }                                                               \
        assert(exppop1 == (MaxPop1))

#define JL_LEAFSET(cIS,Type,MaxPop1,Search,GrowInPlace,InsertInPlace,   \
                   InsertCopy,Cascade,Alloc,Free,ValueArea) {           \
            JL_LEAFPREP(cIS,Type,MaxPop1,ValueArea);                    \
            JL_LEAFGROW(cIS,Type,MaxPop1,Search,ValueArea,GrowInPlace,  \
                        InsertInPlace,InsertCopy,Alloc,Free);           \
	    if (Cascade(Pjp, Pjpm) == -1) return -1;			\
            Free(PjllRaw, MaxPop1, Pjpm);				\
            goto ContinueInsWalk;					\
        }

	case cJL_JPLEAF1:
		JL_LEAFSET(1, uint8_t *, cJL_LEAF1_MAXPOP1, judySearchLeaf1,
			   JL_LEAF1GROWINPLACE, JL_INSERTINPLACE, JL_INSERTCOPY,
			   judyCascade1, judyLAllocJLL1, judyLFreeJLL1, JL_LEAF1VALUEAREA);
	case cJL_JPLEAF2:
		JL_LEAFSET(2, uint16_t *, cJL_LEAF2_MAXPOP1, judySearchLeaf2,
			   JL_LEAF2GROWINPLACE, JL_INSERTINPLACE, JL_INSERTCOPY,
			   judyCascade2, judyLAllocJLL2, judyLFreeJLL2, JL_LEAF2VALUEAREA);
	case cJL_JPLEAF3:
		JL_LEAFSET(3, uint8_t *, cJL_LEAF3_MAXPOP1, judySearchLeaf3,
			   JL_LEAF3GROWINPLACE, JL_INSERTINPLACE3, JL_INSERTCOPY3,
			   judyCascade3, judyLAllocJLL3, judyLFreeJLL3, JL_LEAF3VALUEAREA);
	// JPLEAF_B1:
	// 8 bit Decode | 7 | 6 | 5 | 4 | 3 | 2 | 1 | 0 |
	//              |SubExpanse |    Bit offset     |
	// Note:  For JudyL, values are stored in 8 subexpanses, each a linear word
	// array of up to 32 values each.
	case cJL_JPLEAF_B1: {
		Pjv_t PjvRaw;	// pointer to value part of the leaf.
		Pjv_t Pjv;	// pointer to value part of the leaf.
		Pjv_t PjvnewRaw;	// new value area.
		Pjv_t Pjvnew;	// new value area.
		Word_t subexp;	// 1 of 8 subexpanses in bitmap.
		Pjlb_t Pjlb;	// pointer to bitmap part of the leaf.
		BITMAPL_t bitmap;	// for one subexpanse.
		BITMAPL_t bitmask;	// bit set for Indexs digit.
		int offset;	// of index in value area.
		JL_CHECK_IF_OUTLIER(Pjp, Index, 1, Pjpm);

		digit = JL_DIGITATSTATE(Index, 1);
		Pjlb = P_JLB(Pjp->jp_Addr);
		subexp = digit / cJL_BITSPERSUBEXPL;	// which subexpanse.
		bitmap = JL_JLB_BITMAP(Pjlb, subexp);	// subexps 32-bit map.
		PjvRaw = JL_JLB_PVALUE(Pjlb, subexp);	// corresponding values.
		Pjv = P_JV(PjvRaw);	// corresponding values.
		bitmask = JL_BITPOSMASKL(digit);	// mask for Index.
		offset = judyCountBits(bitmap & (bitmask - 1));	// of Index.
		if (bitmap & bitmask) {
			assert(Pjv);
			Pjpm->jpm_PValue = Pjv + offset;	// existing value.
			return 0;
		}
		exppop1 = judyCountBits(bitmap);

		if (JL_LEAFVGROWINPLACE(exppop1)) {
			JL_INSERTINPLACE(Pjv, exppop1, offset, 0);
			JL_JLB_BITMAP(Pjlb, subexp) |= bitmask;	// set Indexs bit.
			Pjpm->jpm_PValue = Pjv + offset;	// new value area.
			return 1;
		}

		if ((PjvnewRaw = judyLAllocJV(exppop1 + 1, Pjpm)) == NULL)
			return -1;
		Pjvnew = P_JV(PjvnewRaw);

		if (exppop1) {
			assert(Pjv);
			JL_INSERTCOPY(Pjvnew, Pjv, exppop1, offset, 0);
			Pjpm->jpm_PValue = Pjvnew + offset;
			judyLFreeJV(PjvRaw, exppop1, Pjpm);	// free old values.
		} else {
			Pjpm->jpm_PValue = Pjvnew;
			*(Pjpm->jpm_PValue) = 0;
		}

		JL_JLB_BITMAP(Pjlb, subexp) |= bitmask;
		JL_JLB_PVALUE(Pjlb, subexp) = PjvnewRaw;
		return 1;
	}
// This is some of the most complex code in Judy considering Judy1 versus JudyL
// and 32-bit versus 64-bit variations.  The following comments attempt to make
// this clearer.
// Of the 2 words in a JP, for immediate indexes Judy1 can use 2 words - 1 byte
// = 7 [15] bytes, but JudyL can only use 1 word - 1 byte = 3 [7] bytes because
// the other word is needed for a value area or a pointer to a value area.
// For both Judy1 and JudyL, cJL_JPIMMED_*_01 indexes are in word 2; otherwise
// for Judy1 only, a list of 2 or more indexes starts in word 1.  JudyL keeps
// the list in word 2 because word 1 is a pointer (to a LeafV, that is, a leaf
// containing only values).  Furthermore, cJL_JPIMMED_*_01 indexes are stored
// all-but-first-byte in jp_DcdPopO, not just the Index Sizes bytes.
// TBD:  This can be confusing because Doug didnt use data structures for it.
// Instead he often directly accesses Pjp for the first word and jp_DcdPopO for
// the second word.  It would be nice to use data structs, starting with
// jp_LIndex where possible.
// Maximum Immed JP types for Judy1/JudyL, depending on Index Size (cIS):
//          32-bit  64-bit
//    bytes:  7/ 3   15/ 7   (Judy1/JudyL)
//    cIS
//    1_     07/03   15/07   (as in: cJ1_JPIMMED_1_07)
//    2_     03/01   07/03
//    3_     02/01   05/02
//    4_             03/01
//    5_             03/01
//    6_             02/01
//    7_             02/01
// State transitions while inserting an Index, matching the above table:
// (Yes, this is very terse...  Study it and it will make sense.)
// (Note, parts of this diagram are repeated below for quick reference.)
//      +-- reformat JP here for Judy1 only, from word-2 to word-1
//      |
//      |                  JUDY1 || JL_64BIT        JUDY1 && JL_64BIT
//      V
// 1_01 => 1_02 => 1_03 => [ 1_04 => ... => 1_07 => [ 1_08..15 => ]] Leaf1 (*)
// 2_01 =>                 [ 2_02 => 2_03 =>        [ 2_04..07 => ]] Leaf2
// 3_01 =>                 [ 3_02 =>                [ 3_03..05 => ]] Leaf3
// JL_64BIT only:
// 4_01 =>                                         [[ 4_02..03 => ]] Leaf4
// 5_01 =>                                         [[ 5_02..03 => ]] Leaf5
// 6_01 =>                                         [[ 6_02     => ]] Leaf6
// 7_01 =>                                         [[ 7_02     => ]] Leaf7
// (*) For Judy1 & 64-bit, go directly from cJL_JPIMMED_1_15 to a LeafB1; skip
//     Leaf1, as described in Judy1.h regarding cJ1_JPLEAF1.
// COMMON CODE FRAGMENTS TO MINIMIZE REDUNDANCY BELOW:
// These are necessary to support performance by function and loop unrolling
// while avoiding huge amounts of nearly identical code.
// The differences between Judy1 and JudyL with respect to value area handling
// are just too large for completely common code between them...  Oh well, some
// big ifdefs follow.  However, even in the following ifdefd code, use cJL_*,
// JL_*, and Judy*() instead of cJ1_* / cJL_*, J1_* / JL_*, and
// Judy1*()/JudyL*(), for minimum diffs.
// Handle growth of cJL_JPIMMED_*_01 to cJL_JPIMMED_*_02, for an even or odd
// Index Size (cIS), given oldIndex, Index, and Pjll in the context:
// Put oldIndex and Index in their proper order.  For odd indexes, must copy
// bytes.
// Variations to also handle value areas; see comments above:
// For JudyL, Pjv (start of value area) and oldValue are also in the context;
// leave Pjv set to the value area for Index.
#define JL_IMMSET_01_COPY_EVEN(cIS,CopyWord)    \
        if (oldIndex < Index) {                 \
            Pjll[0] = oldIndex;                 \
            Pjv [0] = oldValue;                 \
            Pjll[1] = Index;                    \
            ++Pjv;                              \
        } else {                                \
            Pjll[0] = Index;                    \
            Pjll[1] = oldIndex;                 \
            Pjv [1] = oldValue;                 \
        }

#define JL_IMMSET_01_COPY_ODD(cIS,CopyWord)     \
        if (oldIndex < Index) {                 \
            CopyWord(Pjll + 0,     oldIndex);   \
            CopyWord(Pjll + (cIS), Index);      \
            Pjv[0] = oldValue;                  \
            ++Pjv;                              \
        } else {                                \
            CopyWord(Pjll + 0,    Index);       \
            CopyWord(Pjll + (cIS), oldIndex);   \
            Pjv[1] = oldValue;                  \
        }

#define JL_IMMSET_01_COPY(cIS,LeafType,NewJPType,Copy,CopyWord)	\
{								\
	LeafType Pjll;						\
	Word_t oldIndex = JL_JPDCDPOP0(Pjp);		\
	Word_t oldValue;					\
	Pjv_t    PjvRaw;					\
	Pjv_t    Pjv;						\
								\
	Index = JL_TRIMTODCDSIZE(Index);			\
								\
	if (oldIndex == Index) {				\
		Pjpm->jpm_PValue = (Pjv_t) Pjp;                 \
		return 0;                                      \
	}							\
								\
	if ((PjvRaw = judyLAllocJV(2, Pjpm)) == NULL)	\
		return -1;                                     \
	Pjv = P_JV(PjvRaw);					\
								\
	oldValue       = Pjp->jp_Addr;				\
	(Pjp->jp_Addr) = (Word_t) PjvRaw;			\
	Pjll           = (LeafType) (Pjp->jp_LIndex);		\
								\
	Copy(cIS,CopyWord);					\
								\
	Pjp->jp_Type   = (NewJPType);				\
	*Pjv             = 0;					\
	Pjpm->jpm_PValue = Pjv;					\
	return 1;						\
}

#define JL_IMMSET_01_CASCADE(cIS,LeafType,NewJPType,ValueArea,  \
                             Copy,CopyWord,Alloc)               \
{								\
    Word_t D_P0;						\
    LeafType PjllRaw;						\
    LeafType Pjll;						\
    Word_t oldIndex = JL_JPDCDPOP0(Pjp);			\
    Word_t oldValue;					\
    Pjv_t    Pjv;						\
    Index = JL_TRIMTODCDSIZE(Index);				\
    if (oldIndex == Index) {					\
	Pjpm->jpm_PValue = (Pjv_t) (&(Pjp->jp_Addr));		\
	return 0;						\
    }								\
								\
    if ((PjllRaw = (LeafType) Alloc(2, Pjpm)) == NULL)		\
	return -1;						\
    Pjll = (LeafType) P_JLL(PjllRaw);				\
    Pjv  = ValueArea(Pjll, 2);					\
    oldValue = Pjp->jp_Addr;					\
    Copy(cIS,CopyWord);						\
    *Pjv = 0;							\
    Pjpm->jpm_PValue  = Pjv;					\
    D_P0 = Index & cJL_DCDMASK(cIS); /* pop0 = 0 */		\
    JL_JPSETADT(Pjp, (Word_t)PjllRaw, D_P0, NewJPType);	\
    return 1;							\
}

#define JL_IMMSETINPLACE(cIS,LeafType,BaseJPType_02,Search,InsertInPlace) \
{                                                                 \
    LeafType Pleaf;                                               \
    int      offset;                                              \
    Pjv_t    PjvRaw;                                              \
    Pjv_t    Pjv;                                                 \
    Pjv_t    PjvnewRaw;                                           \
    Pjv_t    Pjvnew;                                              \
								  \
    exppop1 = JL_JPTYPE(Pjp) - (BaseJPType_02) + 2;               \
    offset  = Search((Pjll_t) (Pjp->jp_LIndex), exppop1, Index);  \
    PjvRaw  = (Pjv_t) (Pjp->jp_Addr);                             \
    Pjv     = P_JV(PjvRaw);                                       \
								  \
    JL_CHECK_IF_EXISTS(offset, Pjv, Pjpm);                        \
								  \
    if ((PjvnewRaw = judyLAllocJV(exppop1 + 1, Pjpm)) == NULL)    \
	     return -1;					  \
    Pjvnew = P_JV(PjvnewRaw);                                     \
								  \
    Pleaf = (LeafType) (Pjp->jp_LIndex);                          \
								  \
    InsertInPlace(Pleaf, exppop1, offset, Index);                 \
    /* see TBD above about this: */                               \
    JL_INSERTCOPY(Pjvnew, Pjv, exppop1, offset, 0);               \
    judyLFreeJV(PjvRaw, exppop1, Pjpm);				  \
    Pjp->jp_Addr     = (Word_t) PjvnewRaw;                      \
    Pjpm->jpm_PValue = Pjvnew + offset;                           \
    ++(Pjp->jp_Type);                                             \
    return 1;                                                    \
}

#define JL_IMMSETCASCADE(cIS,OldPop1,LeafType,NewJPType,                \
                         ValueArea,Search,InsertCopy,Alloc)             \
        {                                                               \
            Word_t D_P0;                                      \
            Pjll_t PjllRaw;                                             \
            Pjll_t Pjll;                                                \
            int    offset;                                              \
            Pjv_t  PjvRaw;                                              \
            Pjv_t  Pjv;                                                 \
            Pjv_t  Pjvnew;                                              \
                                                                        \
            PjvRaw = (Pjv_t) (Pjp->jp_Addr);                            \
            Pjv    = P_JV(PjvRaw);                                      \
            offset = Search((Pjll_t) (Pjp->jp_LIndex), (OldPop1), Index); \
            JL_CHECK_IF_EXISTS(offset, Pjv, Pjpm);                      \
                                                                        \
            if ((PjllRaw = Alloc((OldPop1) + 1, Pjpm)) == 0)            \
                return -1;                                             \
            Pjll = P_JLL(PjllRaw);                                      \
            InsertCopy((LeafType) Pjll, (LeafType) (Pjp->jp_LIndex),    \
                       OldPop1, offset, Index);                         \
                                                                        \
            Pjvnew = ValueArea(Pjll, (OldPop1) + 1);                    \
            JL_INSERTCOPY(Pjvnew, Pjv, OldPop1, offset, 0);             \
            judyLFreeJV(PjvRaw, (OldPop1), Pjpm);                     \
            Pjpm->jpm_PValue = Pjvnew + offset;                         \
                                                                        \
            D_P0 = (Index & cJL_DCDMASK(cIS)) + (OldPop1) - 1;          \
            JL_JPSETADT(Pjp, (Word_t)PjllRaw, D_P0, NewJPType);         \
            return 1;                                                  \
        }

	// (1_01 => 1_02 => 1_03 => [ 1_04 => ... => 1_07 => [ 1_08..15 => ]] LeafL)
	case cJL_JPIMMED_1_01:
		JL_IMMSET_01_COPY(1, uint8_t *, cJL_JPIMMED_1_02, 
				  JL_IMMSET_01_COPY_EVEN, ignore);
	// 2_01 leads to 2_02, and 3_01 leads to 3_02, except for JudyL 32-bit, where
	// they lead to a leaf:
	// (2_01 => [ 2_02 => 2_03 => [ 2_04..07 => ]] LeafL)
	// (3_01 => [ 3_02 =>         [ 3_03..05 => ]] LeafL)
	case cJL_JPIMMED_2_01:
		JL_IMMSET_01_CASCADE(2, uint16_t *, cJL_JPLEAF2, JL_LEAF2VALUEAREA,
				     JL_IMMSET_01_COPY_EVEN, ignore, judyLAllocJLL2);
	case cJL_JPIMMED_3_01:
		JL_IMMSET_01_CASCADE(3, uint8_t *, cJL_JPLEAF3, JL_LEAF3VALUEAREA,
				     JL_IMMSET_01_COPY_ODD,
				     JL_COPY3_LONG_TO_PINDEX, judyLAllocJLL3);
	// cJL_JPIMMED_1_* cases that can grow in place:
	// (1_01 => 1_02 => 1_03 => [ 1_04 => ... => 1_07 => [ 1_08..15 => ]] LeafL)
	case cJL_JPIMMED_1_02:
		JL_IMMSETINPLACE(1, uint8_t *, cJL_JPIMMED_1_02, judySearchLeaf1,
				 JL_INSERTINPLACE);
	// cJL_JPIMMED_1_* cases that must cascade:
	// (1_01 => 1_02 => 1_03 => [ 1_04 => ... => 1_07 => [ 1_08..15 => ]] LeafL)
	case cJL_JPIMMED_1_03:
		JL_IMMSETCASCADE(1, 3, uint8_t *, cJL_JPLEAF1, JL_LEAF1VALUEAREA,
				 judySearchLeaf1, JL_INSERTCOPY, judyLAllocJLL1);
	// cJL_JPIMMED_[2..7]_[02..15] cases that grow in place or cascade:
	// (2_01 => [ 2_02 => 2_03 => [ 2_04..07 => ]] LeafL)
	// (3_01 => [ 3_02 => [ 3_03..05 => ]] LeafL)
	default:
		JL_SET_ERRNO(JL_ERRNO_CORRUPT);
		return -1;
	}			// switch on JP type
	retcode = judyInsWalk(Pjp, Index, Pjpm);

	if ((JL_JPTYPE(Pjp) < cJL_JPIMMED_1_01) && (retcode == 1)) {
		jp_t JP;
		Word_t DcdP0;
		JP = *Pjp;
		DcdP0 = JL_JPDCDPOP0(Pjp) + 1;
		JL_JPSETADT(Pjp, JP.jp_Addr, DcdP0, JL_JPTYPE(&JP));
	}
	return retcode;
}

void **JudyLIns(void **PPArray, uint32_t Index)
{
	Pjv_t Pjv;		// value area in old leaf.
	Pjv_t Pjvnew;		// value area in new leaf.
	Pjpm_t Pjpm;		// array-global info.
	int offset;		// position in which to store new Index.
	Pjlw_t Pjlw;

	if (PPArray == NULL) {
		JL_SET_ERRNO(JL_ERRNO_NULLPPARRAY);
		return PPJERR;
	}

	Pjlw = P_JLW(*PPArray);
	if (Pjlw == NULL) {
		Pjlw_t Pjlwnew;

		Pjlwnew = judyLAllocJLW(1);
		JL_CHECKALLOC(Pjlw_t, Pjlwnew, PPJERR);

		Pjlwnew[0] = 1 - 1;
		Pjlwnew[1] = Index;

		*PPArray = (void *) Pjlwnew;
		Pjlwnew[2] = 0;	
		return (void **)(Pjlwnew + 2);
	}

	if (JL_LEAFW_POP0(*PPArray) < cJL_LEAFW_MAXPOP1) {
		Pjlw_t Pjlwnew;
		Word_t pop1;

		Pjlw = P_JLW(*PPArray);	
		pop1 = Pjlw[0] + 1;

		Pjv = JL_LEAFWVALUEAREA(Pjlw, pop1);
		offset = judySearchLeafW(Pjlw + 1, pop1, Index);

		if (offset >= 0)
			return (void **)(Pjv + offset);
		offset = ~offset;

		if (JL_LEAFWGROWINPLACE(pop1)) {
			++Pjlw[0];

			JL_INSERTINPLACE(Pjlw + 1, pop1, offset, Index);
			JL_INSERTINPLACE(Pjv, pop1, offset, 0);

			return (void **)(Pjv + offset);
		}

		if (pop1 < cJL_LEAFW_MAXPOP1) {
			Pjlwnew = judyLAllocJLW(pop1 + 1);
			JL_CHECKALLOC(Pjlw_t, Pjlwnew, PPJERR);

			Pjlwnew[0] = pop1;

			JL_INSERTCOPY(Pjlwnew + 1, Pjlw + 1, pop1, offset, Index);
			Pjvnew = JL_LEAFWVALUEAREA(Pjlwnew, pop1 + 1);
			JL_INSERTCOPY(Pjvnew, Pjv, pop1, offset, 0);
			judyLFreeJLW(Pjlw, pop1, NULL);
			*PPArray = (void *) Pjlwnew;
			return (void **)(Pjvnew + offset);
		}
		assert(pop1 == cJL_LEAFW_MAXPOP1);

		Pjpm = judyLAllocJPM();
		JL_CHECKALLOC(Pjpm_t, Pjpm, PPJERR);

		Pjpm->jpm_Pop0 = cJL_LEAFW_MAXPOP1 - 1;
		Pjpm->jpm_JP.jp_Addr = (Word_t) Pjlw;

		if (judyCascadeL(&Pjpm->jpm_JP, Pjpm) == -1)
			return PPJERR;

		judyLFreeJLW(Pjlw, cJL_LEAFW_MAXPOP1, NULL);
		*PPArray = (void *)Pjpm;
	}
	{
		int retcode;
		Pjpm = P_JPM(*PPArray);
		retcode = judyInsWalk(&Pjpm->jpm_JP, Index, Pjpm);
		if (retcode == -1)
			return PPJERR;
		if (retcode == 1)
			++(Pjpm->jpm_Pop0);	// incr total array popu.
		assert(((Pjpm->jpm_JP.jp_Type) == cJL_JPBRANCH_L)
		       || ((Pjpm->jpm_JP.jp_Type) == cJL_JPBRANCH_B)
		       || ((Pjpm->jpm_JP.jp_Type) == cJL_JPBRANCH_U));
		assert(Pjpm->jpm_PValue != NULL);
		return ((void **) Pjpm->jpm_PValue);
	}
}
