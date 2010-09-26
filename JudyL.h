#ifndef _JUDYL_INCLUDED
#define _JUDYL_INCLUDED

#include "JudyPrivate.h"	// includes Judy.h in turn.
#include "JudyPrivateBranch.h"	// support for branches.
#include <assert.h>

/* uint8_t */
typedef enum {
#define cJL_JPNULLMAX cJL_JPNULL3
	cJL_JPNULL1 = 1,	// Index Size 1[1] byte  when 1 Index inserted.
	cJL_JPNULL2,		// Index Size 2[2] bytes when 1 Index inserted.
	cJL_JPNULL3,		// Index Size 3[3] bytes when 1 Index inserted.
	cJL_JPBRANCH_L2,	// 2[2] bytes Pop0, 1[5] bytes Dcd.
	cJL_JPBRANCH_L3,	// 3[3] bytes Pop0, 0[4] bytes Dcd.
	cJL_JPBRANCH_L,		// note:  DcdPopO field not used.
	cJL_JPBRANCH_B2,	// 2[2] bytes Pop0, 1[5] bytes Dcd.
	cJL_JPBRANCH_B3,	// 3[3] bytes Pop0, 0[4] bytes Dcd.
	cJL_JPBRANCH_B,		// note:  DcdPopO field not used.
	cJL_JPBRANCH_U2,	// 2[2] bytes Pop0, 1[5] bytes Dcd.
	cJL_JPBRANCH_U3,	// 3[3] bytes Pop0, 0[4] bytes Dcd.
	cJL_JPBRANCH_U,		// note:  DcdPopO field not used.
	cJL_JPLEAF1,		// 1[1] byte  Pop0, 2    bytes Dcd.
	cJL_JPLEAF2,		// 2[2] bytes Pop0, 1[5] bytes Dcd.
	cJL_JPLEAF3,		// 3[3] bytes Pop0, 0[4] bytes Dcd.
	cJL_JPLEAF_B1,		// 1[1] byte Pop0, 2[6] bytes Dcd.
	cJL_JPIMMED_1_01,	// Index Size = 1, Pop1 = 1.
	cJL_JPIMMED_2_01,	// Index Size = 2, Pop1 = 1.
	cJL_JPIMMED_3_01,	// Index Size = 3, Pop1 = 1.
	cJL_JPIMMED_1_02,	// Index Size = 1, Pop1 = 2.
	cJL_JPIMMED_1_03,	// Index Size = 1, Pop1 = 3.
	cJL_JPIMMED_CAP
} jpL_Type_t;

// RELATED VALUES: Index Size (state) for leaf JP, and JP type based on Index Size (state)
#define JL_LEAFINDEXSIZE(jpType) ((jpType)    - cJL_JPLEAF1 + 1)
#define JL_LEAFTYPE(IndexSize)   ((IndexSize) + cJL_JPLEAF1 - 1)

// MAXIMUM POPULATIONS OF LINEAR LEAVES:
#define J_L_MAXB                (sizeof(uint32_t) * 64)
#define ALLOCSIZES { 3, 5, 7, 11, 15, 23, 32, 47, 64, TERMINATOR }	// in words.
#define cJL_LEAF1_MAXWORDS      (32)	// max Leaf1 size in words.

// Note:  cJL_LEAF1_MAXPOP1 is chosen such that the index portion is less than
// 32 bytes -- the number of bytes the index takes in a bitmap leaf.
#define cJL_LEAF1_MAXPOP1 ((cJL_LEAF1_MAXWORDS * cJL_BYTESPERWORD)/(1 + cJL_BYTESPERWORD))
#define cJL_LEAF2_MAXPOP1 (J_L_MAXB / (2 + cJL_BYTESPERWORD))
#define cJL_LEAF3_MAXPOP1 (J_L_MAXB / (3 + cJL_BYTESPERWORD))
#define cJL_LEAFW_MAXPOP1 ((J_L_MAXB - cJL_BYTESPERWORD) / (2 * cJL_BYTESPERWORD))

// MAXIMUM POPULATIONS OF IMMEDIATE JPs:
// These specify the maximum Population of immediate JPs with various Index
// Sizes (== sizes of remaining undecoded Index bits).  Since the JP Types enum
// already lists all the immediates in order by state and size, calculate these
// values from it to avoid redundancy.
#define cJL_IMMED1_MAXPOP1  ((cJL_BYTESPERWORD - 1) / 1)	// 3 [7].
#define cJL_IMMED2_MAXPOP1  ((cJL_BYTESPERWORD - 1) / 2)	// 1 [3].
#define cJL_IMMED3_MAXPOP1  ((cJL_BYTESPERWORD - 1) / 3)	// 1 [2].

// Assemble bitmap leaves out of smaller units that put bitmap subexpanses
// close to their associated pointers.  Why not just use a bitmap followed by a
// series of pointers?  (See 4.27.)  Turns out this wastes a cache fill on
// systems with smaller cache lines than the assumed value cJL_WORDSPERCL.
#define JL_JLB_BITMAP(Pjlb, Subexp)  ((Pjlb)->jLlb_jLlbs[Subexp].jLlbs_Bitmap)
#define JL_JLB_PVALUE(Pjlb, Subexp)  ((Pjlb)->jLlb_jLlbs[Subexp].jLlbs_PValue)

typedef struct JUDYL_LEAF_BITMAP_SUBEXPANSE {
	BITMAPL_t jLlbs_Bitmap;
	Pjv_t jLlbs_PValue;
} jLlbs_t;

typedef struct JUDYL_LEAF_BITMAP {
	jLlbs_t jLlb_jLlbs[cJL_NUMSUBEXPL];
} jlb_t, *Pjlb_t;

/**
 * ARRAY-GLOBAL INFORMATION:
 * At the cost of an occasional additional cache fill, this object, which is
 * pointed at by a JRP and in turn points to a JP_BRANCH*, carries array-global
 * information about a JudyL array that has sufficient population to amortize
 * the cost.  The jpm_Pop0 field prevents having to add up the total population
 * for the array in insert, delete, and count code.  The jpm_JP field prevents
 * having to build a fake JP for entry to a state machine; however, the
 * jp_DcdPopO field in jpm_JP, being one byte too small, is not used.
 * Note:  Struct fields are ordered to keep "hot" data in the first 8 words
 * (see left-margin comments) for machines with 8-word cache lines, and to keep
 * sub-word fields together for efficient packing.
 */
typedef struct JUDYL_POPULATION_AND_MEMORY {
	Word_t jpm_Pop0;
	jp_t jpm_JP;
	Word_t jpm_LastUPop0;
	Pjv_t jpm_PValue;
	Word_t jpm_TotalMemWords;
} jLpm_t, *Pjpm_t;

// TABLES FOR DETERMINING IF LEAVES HAVE ROOM TO GROW:
// These tables indicate if a given memory chunk can support growth of a given
// object into wasted (rounded-up) memory in the chunk.  Note:  This violates
// the hiddenness of the JudyMalloc code.
extern const uint8_t jL_Leaf1PopToWords[cJL_LEAF1_MAXPOP1 + 1];
extern const uint8_t jL_Leaf2PopToWords[cJL_LEAF2_MAXPOP1 + 1];
extern const uint8_t jL_Leaf3PopToWords[cJL_LEAF3_MAXPOP1 + 1];
extern const uint8_t jL_LeafWPopToWords[cJL_LEAFW_MAXPOP1 + 1];
extern const uint8_t jL_LeafVPopToWords[];

// These tables indicate where value areas start:
extern const uint8_t jL_Leaf1Offset[cJL_LEAF1_MAXPOP1 + 1];
extern const uint8_t jL_Leaf2Offset[cJL_LEAF2_MAXPOP1 + 1];
extern const uint8_t jL_Leaf3Offset[cJL_LEAF3_MAXPOP1 + 1];
extern const uint8_t jL_LeafWOffset[cJL_LEAFW_MAXPOP1 + 1];

// Also define macros to hide the details in the code using these tables.
#define JL_LEAF1GROWINPLACE(Pop1) J__U_GROWCK(Pop1, cJL_LEAF1_MAXPOP1, jL_Leaf1PopToWords)
#define JL_LEAF2GROWINPLACE(Pop1) J__U_GROWCK(Pop1, cJL_LEAF2_MAXPOP1, jL_Leaf2PopToWords)
#define JL_LEAF3GROWINPLACE(Pop1) J__U_GROWCK(Pop1, cJL_LEAF3_MAXPOP1, jL_Leaf3PopToWords)
#define JL_LEAFWGROWINPLACE(Pop1) J__U_GROWCK(Pop1, cJL_LEAFW_MAXPOP1, jL_LeafWPopToWords)
#define JL_LEAFVGROWINPLACE(Pop1) J__U_GROWCK(Pop1, cJL_BITSPERSUBEXPL,  jL_LeafVPopToWords)

#define JL_LEAF1VALUEAREA(Pjv, Pop1) (((PWord_t )(Pjv)) + jL_Leaf1Offset[Pop1])
#define JL_LEAF2VALUEAREA(Pjv, Pop1) (((PWord_t )(Pjv)) + jL_Leaf2Offset[Pop1])
#define JL_LEAF3VALUEAREA(Pjv, Pop1) (((PWord_t )(Pjv)) + jL_Leaf3Offset[Pop1])
#define JL_LEAFWVALUEAREA(Pjv, Pop1) (((PWord_t )(Pjv)) + jL_LeafWOffset[Pop1])

#define JL_LEAF1POPTOWORDS(Pop1) (jL_Leaf1PopToWords[Pop1])
#define JL_LEAF2POPTOWORDS(Pop1) (jL_Leaf2PopToWords[Pop1])
#define JL_LEAF3POPTOWORDS(Pop1) (jL_Leaf3PopToWords[Pop1])
#define JL_LEAFWPOPTOWORDS(Pop1) (jL_LeafWPopToWords[Pop1])
#define JL_LEAFVPOPTOWORDS(Pop1) (jL_LeafVPopToWords[Pop1])

Pjpm_t judyLAllocJLPM(void);
Pjbl_t judyLAllocJBL(Pjpm_t);
void   judyLFreeJBL(Pjbl_t, Pjpm_t);
Pjbb_t judyLAllocJBB(Pjpm_t);
void   judyLFreeJBB(Pjbb_t, Pjpm_t);
Pjp_t  judyLAllocJBBJP(Word_t, Pjpm_t);
void   judyLFreeJBBJP(Pjp_t, Word_t, Pjpm_t);
Pjbu_t judyLAllocJBU(Pjpm_t);
void   judyLFreeJBU(Pjbu_t, Pjpm_t);
Pjll_t judyLAllocJLL1(Word_t, Pjpm_t);
void   judyLFreeJLL1(Pjll_t, Word_t, Pjpm_t);
Pjll_t judyLAllocJLL2(Word_t, Pjpm_t);
void   judyLFreeJLL2(Pjll_t, Word_t, Pjpm_t);
Pjll_t judyLAllocJLL3(Word_t, Pjpm_t);
void   judyLFreeJLL3(Pjll_t, Word_t, Pjpm_t);
Pjlw_t judyLAllocJLW(Word_t);
void   judyLFreeJLW(Pjlw_t, Word_t, Pjpm_t);
Pjlb_t judyLAllocJLB1(Pjpm_t);
void   judyLFreeJLB1(Pjlb_t, Pjpm_t);
Pjv_t  judyLAllocJV(Word_t, Pjpm_t);
void   judyLFreeJV(Pjv_t, Word_t, Pjpm_t);
Pjpm_t judyLAllocJPM(void);
void   judyLFreeJPM(Pjpm_t PjpmFree, Pjpm_t PjpmStats);
void   judyLFreeSM(Pjp_t, Pjpm_t);	// everything below Pjp.

#endif // ! _JUDYL_INCLUDED
