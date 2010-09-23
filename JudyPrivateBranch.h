#ifndef _JUDY_PRIVATE_BRANCH_INCLUDED
#define _JUDY_PRIVATE_BRANCH_INCLUDED

/**
 * JUDY POINTER (JP) SUPPORT
 * This "rich pointer" object is pivotal to Judy execution.
 * JP CONTAINING OTHER THAN IMMEDIATE INDEXES:
 * If the JP points to a linear or bitmap leaf, jp_DcdPopO contains the
 * Population-1 in LSbs and Decode (Dcd) bytes in the MSBs.  (In practice the
 * Decode bits are masked off while accessing the Pop0 bits.)
 * The Decode Size, the number of Dcd bytes available, is encoded in jpo_Type.
 * It can also be thought of as the number of states "skipped" in the SM, where
 * each state decodes 8 bits = 1 byte.
 * Note:  The jpo_u union is not required by HP-UX or Linux but Win32 because
 * the cl.exe compiler otherwise refuses to pack a bitfield (DcdPopO) with
 * anything else, even with the -Zp option.  This is pretty ugly, but
 * fortunately portable, and its all hide-able by macros (see below). 
 */
typedef struct JUDY_POINTER_OTHERS {
	Word_t j_po_Addr;				// first word:  Pjp_t, Word_t, etc.
	union {
		uint8_t j_po_DcdP0[sizeof(Word_t) - 1];
		uint8_t j_po_Bytes[sizeof(Word_t)];	// last byte = jp_Type.
	} jpo_u;
} jpo_t;

/**
 * JP CONTAINING IMMEDIATE INDEXES:
 * j_pi_1Index[] plus j_pi_LIndex[] together hold as many N-byte (1..3-byte
 * [1..7-byte]) Indexes as will fit in sizeof(jpi_t) less 1 byte for j_pi_Type
 * (that is, 7..1 [15..1] Indexes).
 * For Judy1, j_pi_1Index[] is used and j_pi_LIndex[] is not used.
 * For JudyL, j_pi_LIndex[] is used and j_pi_1Index[] is not used.
 * Note:  Actually when Pop1 = 1, jpi_t is not used, and the least bytes of the
 * single Index are stored in j_po_DcdPopO, for both Judy1 and JudyL, so for
 * JudyL the j_po_Addr field can hold the target value.
 */
typedef struct JUDY_POINTER_IMMED {
	uint8_t j_pi_1Index[sizeof(Word_t)];
	uint8_t j_pi_LIndex[sizeof(Word_t) - 1];
	uint8_t j_pi_Type; // JP type, 1 of cJ*_JPIMMED*.
} jpi_t;

/* UNION OF JP TYPES:
 * A branch is an array of cJL_BRANCHUNUMJPS (256) of this object, or an
 * alternate data type such as:  A linear branch which is a list of 2..7 JPs,
 * or a bitmap branch which contains 8 lists of 0..32 JPs.  JPs reside only in
 * branches of a Judy SM.
 */
typedef union JUDY_POINTER {	// JP.
	jpo_t j_po;		// other than immediate indexes.
	jpi_t j_pi;		// immediate indexes.
} jp_t, *Pjp_t;

#define jp_LIndex  j_pi.j_pi_LIndex	// for storing Indexes in second word.
#define jp_Addr    j_po.j_po_Addr
#define jp_Type    j_po.jpo_u.j_po_Bytes[sizeof(Word_t) - 1]
#define jp_DcdP0   j_po.jpo_u.j_po_DcdP0

/** 
 * EXTRACT VALUES FROM JP:
 * Masks for the bytes in the Dcd and Pop0 parts of jp_DcdPopO:
 * cJL_DCDMASK() consists of a mask that excludes the (LSb) Pop0 bytes and
 * also, just to be safe, the top byte of the word, since jp_DcdPopO is 1 byte
 * less than a full word.
 * Note:  These are constant macros (cJU) because cPopBytes should be a
 * constant.  Also note cPopBytes == state in the SM.
 */
#define cJL_POP0MASK(cPopBytes) JL_LEASTBYTESMASK(cPopBytes)
#define cJL_DCDMASK(cPopBytes) \
        ((cJL_ALLONES >> cJL_BITSPERBYTE) & (~cJL_POP0MASK(cPopBytes)))

// Mask off the high byte from INDEX to it can be compared to DcdPopO:
#define JL_TRIMTODCDSIZE(INDEX) ((cJL_ALLONES >> cJL_BITSPERBYTE) & (INDEX))

// Get from jp_DcdPopO the Pop0 for various branch JP Types:
// Note:  There are no simple macros for cJL_BRANCH* Types because their
// populations must be added up and dont reside in an already-calculated
// place.
#define JL_JPBRANCH_POP0(PJP,cPopBytes) \
        (JL_JPDCDPOP0(PJP) & cJL_POP0MASK(cPopBytes))

// METHOD FOR DETERMINING IF OBJECTS HAVE ROOM TO GROW:
// J__U_GROWCK() is a generic method to determine if an object can grow in
// place, based on whether the next population size (one more) would use the
// same space.
#define J__U_GROWCK(POP1,MAXPOP1,POPTOWORDS) \
        (((POP1) != (MAXPOP1)) && (POPTOWORDS[POP1] == POPTOWORDS[(POP1) + 1]))
#define JL_BRANCHBJPGROWINPLACE(NumJPs) \
        J__U_GROWCK(NumJPs, cJL_BITSPERSUBEXPB, jL_BranchBJPPopToWords)

// DETERMINE IF AN INDEX IS (NOT) IN A JPS EXPANSE:
#define JL_DCDNOTMATCHINDEX(INDEX,PJP,POP0BYTES) \
        (((INDEX) ^ JL_JPDCDPOP0(PJP)) & cJL_DCDMASK(POP0BYTES))

// NUMBER OF JPs IN AN UNCOMPRESSED BRANCH:
// An uncompressed branch is simply an array of 256 Judy Pointers (JPs).  It is
// a minimum cacheline fill object.  Define it here before its first needed.
#define cJL_BRANCHUNUMJPS  cJL_SUBEXPPERSTATE

/**
 * A linear branch is a way of compressing empty expanses (null JPs) out of an
 * uncompressed 256-way branch, when the number of populated expanses is so
 * small that even a bitmap branch is excessive.
 * The maximum number of JPs in a Judy linear branch:
 * Note:  This number results in a 1-cacheline sized structure.  Previous
 * versions had a larger struct so a linear branch didnt become a bitmap
 * branch until the memory consumed was even, but for speed, its better to
 * switch "sooner" and keep a linear branch fast.
 */
#define cJL_BRANCHLMAXJPS 7

// LINEAR BRANCH STRUCT:
// 1-byte count, followed by array of byte-sized expanses, followed by JPs.
typedef struct JUDY_BRANCH_LINEAR {
	uint8_t jbl_NumJPs;	// num of JPs (Pjp_t), 1..N.
	uint8_t jbl_Expanse[cJL_BRANCHLMAXJPS];	// 1..7 MSbs of pop exps.
	jp_t jbl_jp[cJL_BRANCHLMAXJPS];	// JPs for populated exps.
} jbl_t, *Pjbl_t;

// A bitmap branch is a way of compressing empty expanses (null JPs) out of
// uncompressed 256-way branch.  This costs 1 additional cache line fill, but
// can save a lot of memory when it matters most, near the leaves, and
// typically there will be only one at most in the path to any Index (leaf).
// The bitmap indicates which of the cJL_BRANCHUNUMJPS (256) JPs in the branch
// are NOT null, that is, their expanses are populated.  The jbb_t also
// contains N pointers to "mini" Judy branches ("subexpanses") of up to M JPs
// each (see BITMAP_BRANCHMxN, for example, BITMAP_BRANCH32x8), where M x N =
// cJL_BRANCHUNUMJPS.  These are dynamically allocated and never contain
// cJ*_JPNULL* jp_Types.  An empty subexpanse is represented by no bit sets in
// the corresponding subexpanse bitmap, in which case the corresponding
// jbbs_Pjp pointers value is unused.
// Note that the number of valid JPs in each 1-of-N subexpanses is determined
// by POPULATION rather than by EXPANSE -- the desired outcome to save memory
// when near the leaves.  Note that the memory required for 185 JPs is about as
// much as an uncompressed 256-way branch, therefore 184 is set as the maximum.
// However, it is expected that a conversion to an uncompressed 256-way branch
// will normally take place before this limit is reached for other reasons,
// such as improving performance when the "wasted" memory is well amortized by
// the population under the branch, preserving an acceptable overall
// bytes/Index in the Judy array.
// The number of pointers to arrays of JPs in the Judy bitmap branch:
// Note:  The numbers below are the same in both 32 and 64 bit systems.
#define cJL_BRANCHBMAXJPS  184	// maximum JPs for bitmap branches.

// Convenience wrappers for referencing BranchB bitmaps or JP subarray
// pointers:
// Note: JL_JBB_PJP produces a "raw" memory address that must pass through
// P_JP before use, except when freeing memory:
#define JL_JBB_BITMAP(Pjbb, SubExp)  ((Pjbb)->jbb_jbbs[SubExp].jbbs_Bitmap)
#define JL_JBB_PJP(Pjbb, SubExp)  ((Pjbb)->jbb_jbbs[SubExp].jbbs_Pjp)
#define JL_SUBEXPB(Digit) (((Digit) / cJL_BITSPERSUBEXPB) & (cJL_NUMSUBEXPB-1))
#define JL_BITMAPTESTB(Pjbb, Index) \
        (JL_JBB_BITMAP(Pjbb, JL_SUBEXPB(Index)) &  JL_BITPOSMASKB(Index))
#define JL_BITMAPSETB(Pjbb, Index)  \
        (JL_JBB_BITMAP(Pjbb, JL_SUBEXPB(Index)) |= JL_BITPOSMASKB(Index))

typedef struct JUDY_BRANCH_BITMAP_SUBEXPANSE {
	BITMAPB_t jbbs_Bitmap;
	Pjp_t jbbs_Pjp;
} jbbs_t;

typedef struct JUDY_BRANCH_BITMAP {
	jbbs_t jbb_jbbs[cJL_NUMSUBEXPB];
} jbb_t, *Pjbb_t;

#define JL_BRANCHJP_NUMJPSTOWORDS(NumJPs) (jL_BranchBJPPopToWords[NumJPs])

// Convenience wrapper for referencing BranchU JPs:
// Note:  This produces a non-"raw" address already passed through P_JBU().
#define JL_JBU_PJP(Pjp,Index,Level) \
        (&((P_JBU((Pjp)->jp_Addr))->jbu_jp[JL_DIGITATSTATE(Index, Level)]))
#define JL_JBU_PJP0(Pjp) (&((P_JBU((Pjp)->jp_Addr))->jbu_jp[0]))

typedef struct JUDY_BRANCH_UNCOMPRESSED {
	jp_t jbu_jp[cJL_BRANCHUNUMJPS];	// JPs for populated exp.
} jbu_t, *Pjbu_t;

// OBJECT SIZES IN WORDS:
// Word_ts per various JudyL structures that have constant sizes.
// cJL_WORDSPERJP should always be 2; this is fundamental to the Judy
// structures.
#define cJL_WORDSPERJP (sizeof(jp_t)   / cJL_BYTESPERWORD)
#define cJL_WORDSPERCL (cJL_BYTESPERCL / cJL_BYTESPERWORD)

// OPPORTUNISTIC UNCOMPRESSION:
// Define populations at which a BranchL or BranchB must convert to BranchU.
// Earlier conversion is possible with good memory efficiency -- see below.
// Max population below BranchL, then convert to BranchU:
#define JL_BRANCHL_MAX_POP      1000

// Minimum global population increment before next conversion of a BranchB to a
// BranchU: This is was done to allow malloc() to coalesce memory before the 
// next big (~512 words) allocation.
#define JL_BTOU_POP_INCREMENT    300

// Min/max population below BranchB, then convert to BranchU:
#define JL_BRANCHB_MIN_POP       135
#define JL_BRANCHB_MAX_POP       750

// Get N most significant bits from the shifted Index word:
// As Index words are decoded, they are shifted left so only relevant,
// undecoded Index bits remain.
#define JL_BITSFROMSFTIDX(SFTIDX, N)  ((SFTIDX) >> (cJL_BITSPERWORD - (N)))

#define cJL_MASKATSTATE(State)  (0xffL << (((State) - 1) * cJL_BITSPERBYTE))

// Get byte (digit) from Index at the specified state, right justified:
// Note:  State must be 1..cJL_ROOTSTATE, and Digits must be 1..(cJL_ROOTSTATE
// - 1), but theres no way to assert these within an expression.
#define JL_DIGITATSTATE(Index,cState) \
         ((uint8_t)((Index) >> (((cState) - 1) * cJL_BITSPERBYTE)))

// Similarly, place byte (digit) at correct position for the specified state:
// Note:  Cast digit to a Word_t first so there are no complaints or problems
// about shifting it more than 32 bits on a 64-bit system, say, when it is a
// uint8_t from jbl_Expanse[].  (Believe it or not, the C standard says to
// promote an unsigned char to a signed int; -Ac does not do this, but -Ae
// does.)
// Also, to make lint happy, cast the whole result again because apparently
// shifting a Word_t does not result in a Word_t !
#define JL_DIGITTOSTATE(Digit,cState) \
        ((Word_t) (((Word_t) (Digit)) << (((cState) - 1) * cJL_BITSPERBYTE)))

#endif // ! _JUDY_PRIVATE_BRANCH_INCLUDED
