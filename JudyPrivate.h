#ifndef _JUDYPRIVATE_INCLUDED
#define _JUDYPRIVATE_INCLUDED

#include "Judy.h"
// A VERY BRIEF EXPLANATION OF A JUDY ARRAY
//
// A Judy array is, effectively, a digital tree (or Trie) with 256 element
// branches (nodes), and with "compression tricks" applied to low-population
// branches or leaves to save a lot of memory at the cost of relatively little
// CPU time or cache fills.
//
// In the actual implementation, a Judy array is level-less, and traversing the
// "tree" actually means following the states in a state machine (SM) as
// directed by the Index.  A Judy array is referred to here as an "SM", rather
// than as a "tree"; having "states", rather than "levels".
//
// Each branch or leaf in the SM decodes a portion ("digit") of the original
// Index; with 256-way branches there are 8 bits per digit.  There are 3 kinds
// of branches, called:  Linear, Bitmap and Uncompressed, of which the first 2
// are compressed to contain no NULL entries.
//
// An Uncompressed branch has a 1.0 cache line fill cost to decode 8 bits of
// (digit, part of an Index), but it might contain many NULL entries, and is
// therefore inefficient with memory if lightly populated.
//
// A Linear branch has a ~1.75 cache line fill cost when at maximum population.
// A Bitmap branch has ~2.0 cache line fills.  Linear and Bitmap branches are
// converted to Uncompressed branches when the additional memory can be
// amortized with larger populations.  Higher-state branches have higher
// priority to be converted.
//
// Linear branches can hold 28 elements (based on detailed analysis) -- thus 28
// expanses.  A Linear branch is converted to a Bitmap branch when the 29th
// expanse is required.
//
// A Bitmap branch could hold 256 expanses, but is forced to convert to an
// Uncompressed branch when 185 expanses are required.  Hopefully, it is
// converted before that because of population growth (again, based on detailed
// analysis and heuristics in the code).
//
// A path through the SM terminates to a leaf when the Index (or key)
// population in the expanse below a pointer will fit into 1 or 2 cache lines
// (~31..255 Indexes).  A maximum-population Leaf has ~1.5 cache line fill
// cost.
//
// Leaves are sorted arrays of Indexes, where the Index Sizes (IS) are:  0, 1,
// 8, 16, 24, 32, [40, 48, 56, 64] bits.  The IS depends on the "density"
// (population/expanse) of the values in the Leaf.  Zero bits are possible if
// population == expanse in the SM (that is, a full small expanse).
//
// Elements of a branches are called Judy Pointers (JPs).  Each JP object
// points to the next object in the SM, plus, a JP can decode an additional
// 2[6] bytes of an Index, but at the cost of "narrowing" the expanse
// represented by the next object in the SM.  A "narrow" JP (one which has
// decode bytes/digits) is a way of skipping states in the SM.
//
// Although counterintuitive, we think a Judy SM is optimal when the Leaves are
// stored at MINIMUM compression (narrowing, or use of Decode bytes).  If more
// aggressive compression was used, decompression of a leaf be required to
// insert an index.  Additional compression would save a little memory but not
// help performance significantly.

#ifdef A_PICTURE_IS_WORTH_1000_WORDS
*******************************************************************************
    JUDY 32 - BIT STATE MACHINE(SM) EXAMPLE, FOR INDEX = 0x02040103
    The Index used in this example is purposely chosen to allow small, simple
    examples below;
each 1 - byte "digit" from the Index has a small numeric value that fits in one column.In the drawing below:

JRP ==
    Judy Root Pointer;

C == 1 byte of a 1. .3 byte Population(count of Indexes)
below this
    pointer.Since this is shared with the Decode field, the combined
    sizes must be 3[7], that is,
    1 word less 1 byte for the JP Type.The 1 -
    byte field jp_Type is represented as:1. .3 == Number
      of bytes in the population(Pop0) word of the Branch or Leaf below the pointer(note:1. .7 on 64 - bit);
indicates:
-number of bytes in Decode field == 3 - this number;
-number of bytes remaining to decode.Note:The maximum is 3, not 4, because the 1 st byte of the Index is
    always decoded digitally in the top branch.
    - B - == JP points to a Branch(there are many kinds of Branches).
    - L - == JP points to a Leaf(there are many kinds of Leaves).
    (2) == Digit of Index decoded by position offset in branch(really
							       0. .0 xff).4 * ==
    Digit of Index necessary for decoding
	a "narrow" pointer, in a Decode field;
replaces 1 missing branch(really 0. .0 xff).4 + ==
    Digit of Index NOT necessary for decoding
	a "narrow" pointer, but used for fast
traversal of the SM by Judy1Test()and JudyLGet()
(see the code) (really 0. .0 xff).0 == Byte in a JPs Pop0 field that is always ignored,
    because a leaf can never contain more than 256 Indexes(Pop0 <= 255). + ----- ==
    A Branch or Leaf;
drawn open - ended to remind you that it could | have up to 256 columns. + -----|| ==
    Pointer to next Branch or Leaf.V | O ==
    A state is skipped by using a "narrow" pointer. | <1 > ==
Digit(Index) shown as an example is not necessarily in the position shown;
is sorted in order with neighbor Indexes.(Really 0. .0 xff.)

Note that this example shows every possibly topology to reach a leaf in a
    32 - bit Judy SM, although this is a very subtle point !
    STATE or `
    LEVEL
    +
    ---++---++---++---++---++---++---++---+|RJP | |RJP | |RJP | |RJP | |RJP | |RJP | |RJP
    | |RJP | L-- - +B-- - +B-- - +B-- - +B-- - +B-- - +B-- - +B-- -
    +||||||||||||||||V V(2) V(2) V(2) V(2) V(2) V(2) V(2)
+ ------+------+------+------+------+------+------+------Four | <2 >
    |0 | 4 * |C | 4 * |4 * |C | C byte | <4 > |0 | 0 | C | 1 * |C | C | C 4 Index | <1 >
    |C | C | C | C | C | C | C Leaf | <3 >
    |3 | 2 | 3 | 1 | 2 | 3 | 3 + ------+--L-- - +--L-- - +--B-- - +--L-- - +--B-- -
    +--B-- - +--B-- - |||||||/|/||///|/||//|||||||V | V(4) | |V(4) V(4)
+ ------|+------||+------+------Three | <4 > ||4 + |||4 + |4 + byte Index | <1 >
    O | 0 O O | 1 * |C 3 Leaf | <3 >
    ||C | ||C | C + ------||2 | ||1 | 2 / +----L - ||+----L - +----B -
    /||||||/|///|/|///|/||//|/||//||||||V V | V(1) | V(1)
+ ------+------|+------|+------Two byte | <1 > |<1 > ||4 + ||4 + Index Leaf | <3 > |<3 >
    O | 1 + O | 1 + 2 + ------+------/|C | |C / |1 | |1 | +-L-- -- |+-L-- --
    |||||/|/||||V V V V + ------+------+------+------One byte Index Leaf | <3 > |<3 >
    |<3 > |<3 > 1 + ------+------+------+------
#endif				// A_PICTURE_IS_WORTH_1000_WORDS
#ifndef _BOOL_T
#define _BOOL_T
typedef int bool_t;
#endif

#ifndef TRUE
#define TRUE 1
#endif

#ifndef FALSE
#define FALSE 0
#endif

// #ifndef DEBUG
// #define NDEBUG 1		// must be 1 for "#if".
// #endif

#include <assert.h>
// Machine (CPU) cache line size:
// NOTE:  A leaf size of 2 cache lines maximum is the target (optimal) for
// Judy.  Its hard to obtain a machines cache line size at compile time, but
// if the machine has an unexpected cache line size, its not devastating if
// the following constants end up causing leaves that are 1 cache line in size,
// or even 4 cache lines in size.  The assumed 32-bit system has 16-word =
// 64-byte cache lines, and the assumed 64-bit system has 16-word = 128-byte
// cache lines.
#define cJL_BYTESPERCL  64	// cache line size in bytes.

// Bits Per Byte:
#define cJL_BITSPERBYTE 0x8

// Bytes Per Word and Bits Per Word, latter assuming sizeof(byte) is 8 bits:
// Expect 32 [64] bits per word.

#define cJL_BYTESPERWORD (sizeof(Word_t))
#define cJL_BITSPERWORD  (sizeof(Word_t) * cJL_BITSPERBYTE)
#define JL_BYTESTOWORDS(BYTES) \
        (((BYTES) + cJL_BYTESPERWORD - 1) / cJL_BYTESPERWORD)

// A word that is all-ones, normally equal to -1UL, but safer with ~0:
#define cJL_ALLONES  (~0UL)

// Note, these are forward references, but thats OK:
#define cJL_FULLBITMAPB ((BITMAPB_t) cJL_ALLONES)
#define cJL_FULLBITMAPL ((BITMAPL_t) cJL_ALLONES)

// ROOT STATE:
// State at the start of the Judy SM, based on 1 byte decoded per state; equal
// to the number of bytes per Index to decode.
#define cJL_ROOTSTATE (sizeof(Word_t))

// SUBEXPANSES PER STATE:
// Number of subexpanses per state traversed, which is the number of JPs in a
// branch (actual or theoretical) and the number of bits in a bitmap.
#define cJL_SUBEXPPERSTATE  256

// LEAF AND VALUE POINTERS:
// Some other basic object types are in declared in JudyPrivateBranch.h
// (Pjbl_t, Pjbb_t, Pjbu_t, Pjp_t) or are Judy1/L-specific (Pjlb_t).  The
// few remaining types are declared below.
// Note:  Leaf pointers are cast to different-sized objects depending on the
// leafs level, but are at least addresses (not just numbers), so use void *
// (Pvoid_t), not PWord_t or Word_t for them, except use Pjlw_t for whole-word
// (top-level, root-level) leaves.  Value areas, however, are always whole
// words.
// Furthermore, use Pjll_t only for generic leaf pointers (for various size
// LeafLs).  Use Pjlw_t for LeafWs.  Use Pleaf (with type uint8_t *, uint16_t
// *, etc) when the leaf index size is known.
typedef PWord_t Pjlw_t;		// pointer to root-level leaf (whole-word indexes).
typedef Pvoid_t Pjll_t;		// pointer to lower-level linear leaf.
typedef PWord_t Pjv_t;		// pointer to JudyL value area.

// POINTER PREPARATION MACROS:
// These macros are used to strip malloc-namespace-type bits from a pointer +
// malloc-type word (which references any Judy mallocd object that might be
// obtained from other than a direct call of malloc()), prior to dereferencing
// the pointer as an address.  The malloc-type bits allow Judy mallocd objects
// to come from different "malloc() namespaces".
//    (root pointer)    (JRP, see above)
//    jp.jp_Addr        generic pointer to next-level node, except when used
//                      as a JudyL Immed01 value area
//    JL_JBB_PJP        macro hides jbbs_Pjp (pointer to JP subarray)
//    JL_JLB_PVALUE     macro hides jLlbs_PValue (pointer to value subarray)
// When setting one of these fields or passing an address to judyFree*(), the
// "raw" memory address is used; otherwise the memory address must be passed
// through one of the macros below before its dereferenced.
// Note:  After much study, the typecasts below appear in the macros rather
// than at the point of use, which is both simpler and allows the compiler to
// do type-checking.
#define P_JLW(  ADDR) ((Pjlw_t) (ADDR))	// root leaf.
#define P_JPM(  ADDR) ((Pjpm_t) (ADDR))	// root JPM.
#define P_JBL(  ADDR) ((Pjbl_t) (ADDR))	// BranchL.
#define P_JBB(  ADDR) ((Pjbb_t) (ADDR))	// BranchB.
#define P_JBU(  ADDR) ((Pjbu_t) (ADDR))	// BranchU.
#define P_JLL(  ADDR) ((Pjll_t) (ADDR))	// LeafL.
#define P_JLB(  ADDR) ((Pjlb_t) (ADDR))	// LeafB1.
#define P_JP(   ADDR) ((Pjp_t)  (ADDR))	// JP.
#define P_JV(   ADDR) ((Pjv_t)  (ADDR))	// &value.

// LEAST BYTES:
// Mask for least bytes of a word, and a macro to perform this mask on an
// Index.
// Note:  This macro has been problematic in the past to get right and to make
// portable.  Its not OK on all systems to shift by the full word size.  This
// macro should allow shifting by 1..N bytes, where N is the word size, but
// should produce a compiler warning if the macro is called with Bytes == 0.
// Warning:  JL_LEASTBYTESMASK() is not a constant macro unless Bytes is a
// constant; otherwise it is a variable shift, which is expensive on some
// processors.
#define JL_LEASTBYTESMASK(BYTES) \
        ((0x100UL << (cJL_BITSPERBYTE * ((BYTES) - 1))) - 1)
#define JL_LEASTBYTES(INDEX,BYTES)  ((INDEX) & JL_LEASTBYTESMASK(BYTES))

// BITS IN EACH BITMAP SUBEXPANSE FOR BITMAP BRANCH AND LEAF:
// The bits per bitmap subexpanse times the number of subexpanses equals a
// constant (cJL_SUBEXPPERSTATE).  You can also think of this as a compile-time
// choice of "aspect ratio" for bitmap branches and leaves (which can be set
// independently for each).
// A default aspect ratio is hardwired here if not overridden at compile time,
// such as by "EXTCCOPTS=-DBITMAP_BRANCH16x16 make".
#define BITMAPB_t uint32_t

// Note:  For bitmap leaves
#define BITMAPL_t uint32_t

// EXPORTED DATA AND FUNCTIONS:
extern const uint8_t jL_BranchBJPPopToWords[];
#define SEARCHLEAFNATIVE(LEAFTYPE,ADDR,POP1,INDEX)              \
    LEAFTYPE *P_leaf = (LEAFTYPE *)(ADDR);                      \
    LEAFTYPE I_ndex = (LEAFTYPE)INDEX; /* truncate hi bits */   \
    Word_t   l_ow   = cJL_ALLONES;                              \
    Word_t   m_id;                                              \
    Word_t   h_igh  = POP1;                                     \
                                                                \
    while ((h_igh - l_ow) > 1UL) {                              \
        m_id = (h_igh + l_ow) / 2;                              \
        if (P_leaf[m_id] > I_ndex)                              \
            h_igh = m_id;                                       \
        else                                                    \
            l_ow = m_id;                                        \
    }                                                           \
    if (l_ow == cJL_ALLONES || P_leaf[l_ow] != I_ndex)          \
        return(~h_igh);                                         \
    return(l_ow)

#define SEARCHLEAFNONNAT(ADDR,POP1,INDEX,LFBTS,COPYINDEX)       \
    uint8_t *P_leaf = (uint8_t *)(ADDR);                        \
    Word_t   l_ow   = cJL_ALLONES;                              \
    Word_t   m_id;                                              \
    Word_t   h_igh  = POP1;                                     \
    Word_t   I_ndex = JL_LEASTBYTES((INDEX), (LFBTS));          \
    Word_t   i_ndex;                                            \
                                                                \
    I_ndex = JL_LEASTBYTES((INDEX), (LFBTS));                   \
    while ((h_igh - l_ow) > 1UL) {                              \
        m_id = (h_igh + l_ow) / 2;                              \
        COPYINDEX(i_ndex, &P_leaf[m_id * (LFBTS)]);             \
        if (i_ndex > I_ndex)                                    \
            h_igh = m_id;                                       \
        else                                                    \
            l_ow = m_id;                                        \
    }                                                           \
    if (l_ow == cJL_ALLONES) return(~h_igh);                    \
                                                                \
    COPYINDEX(i_ndex, &P_leaf[l_ow * (LFBTS)]);                 \
    if (i_ndex != I_ndex) return(~h_igh);                       \
    return(l_ow)

#define judyCountBitsB judyCountBitsL
static inline BITMAPL_t judyCountBitsL(BITMAPL_t word)
{
	word = (word & 0x55555555) + ((word & 0xAAAAAAAA) >> 1);
	word = (word & 0x33333333) + ((word & 0xCCCCCCCC) >> 2);
	word = (word & 0x0F0F0F0F) + ((word & 0xF0F0F0F0) >> 4);	// >= 8 bits.
	word = (word & 0x00FF00FF) + ((word & 0xFF00FF00) >> 8);	// >= 16 bits.
	word = (word & 0x0000FFFF) + ((word & 0xFFFF0000) >> 16);	// >= 32 bits.
	return (word);
}				// judyCountBitsL()

// Get from jp_DcdPopO the Pop0 for various JP Types.
// Notes:
// - Different macros require different parameters...
// - There are no simple macros for cJL_BRANCH* Types because their
//   populations must be added up and dont reside in an already-calculated
//   place.  (TBD:  This is no longer true, now its in the JPM.)
// - cJL_JPIMM_POP0() is not defined because it would be redundant because the
//   Pop1 is already encoded in each enum name.
// - A linear or bitmap leaf Pop0 cannot exceed cJL_SUBEXPPERSTATE - 1 (Pop0 =
//   0..255), so use a simpler, faster macro for it than for other JP Types.
// - Avoid any complex calculations that would slow down the compiled code.
//   Assume these macros are only called for the appropriate JP Types.
//   Unfortunately theres no way to trigger an assertion here if the JP type
//   is incorrect for the macro, because these are merely expressions, not
//   statements.
#define  JL_LEAFW_POP0(JRP)                  (*P_JLW(JRP))
#define cJL_JPFULLPOPU1_POP0                 (cJL_SUBEXPPERSTATE - 1)

// GET JP Type:
// Since bit fields greater than 32 bits are not supported in some compilers
// the jp_DcdPopO field is expanded to include the jp_Type in the high 8 bits
// of the Word_t.
// First the read macro:
#define JL_JPTYPE(PJP)          ((PJP)->jp_Type)
#define JL_JPLEAF_POP0(PJP)     ((PJP)->jp_DcdP0[sizeof(Word_t) - 2])
#define JL_JPDCDPOP0(PJP)               \
    ((Word_t)(PJP)->jp_DcdP0[0] << 16 | \
     (Word_t)(PJP)->jp_DcdP0[1] <<  8 | \
     (Word_t)(PJP)->jp_DcdP0[2])

#define JL_JPSETADT(PJP,ADDR,DCDPOP0,TYPE) {                    \
    (PJP)->jp_Addr     = (ADDR);                                \
    (PJP)->jp_DcdP0[0] = (uint8_t)((Word_t)(DCDPOP0) >> 16);    \
    (PJP)->jp_DcdP0[1] = (uint8_t)((Word_t)(DCDPOP0) >>  8);    \
    (PJP)->jp_DcdP0[2] = (uint8_t)((Word_t)(DCDPOP0));          \
    (PJP)->jp_Type     = (TYPE);                                \
}

// NUMBER OF BITS IN A BRANCH OR LEAF BITMAP AND SUBEXPANSE:
// Note:  cJL_BITSPERBITMAP must be the same as the number of JPs in a branch.
#define cJL_BITSPERBITMAP cJL_SUBEXPPERSTATE

// Bitmaps are accessed in units of "subexpanses":
#define cJL_BITSPERSUBEXPB  (sizeof(BITMAPB_t) * cJL_BITSPERBYTE)
#define cJL_NUMSUBEXPB      (cJL_BITSPERBITMAP / cJL_BITSPERSUBEXPB)
#define cJL_BITSPERSUBEXPL  (sizeof(BITMAPL_t) * cJL_BITSPERBYTE)
#define cJL_NUMSUBEXPL      (cJL_BITSPERBITMAP / cJL_BITSPERSUBEXPL)

// MASK FOR A SPECIFIED BIT IN A BITMAP:
// Warning:  If BitNum is a variable, this results in a variable shift that is
// expensive, at least on some processors.  Use with caution.
// Warning:  BitNum must be less than cJL_BITSPERWORD, that is, 0 ..
// cJL_BITSPERWORD - 1, to avoid a truncated shift on some machines.
#define JL_BITPOSMASKB(BITNUM) (1L << ((BITNUM) % cJL_BITSPERSUBEXPB))
#define JL_BITPOSMASKL(BITNUM) (1L << ((BITNUM) % cJL_BITSPERSUBEXPL))

// TEST/SET/CLEAR A BIT IN A BITMAP LEAF:
// Test if a byte-sized Digit (portion of Index) has a corresponding bit set in
// a bitmap, or set a byte-sized Digits bit into a bitmap, by looking up the
// correct subexpanse and then checking/setting the correct bit.
// Note:  Mask higher bits, if any, for the convenience of the user of this
// macro, in case they pass a full Index, not just a digit.  If the caller has
// a true 8-bit digit, make it of type uint8_t and the compiler should skip the
// unnecessary mask step.
#define JL_SUBEXPL(DIGIT) (((DIGIT) / cJL_BITSPERSUBEXPL) & (cJL_NUMSUBEXPL-1))
#define JL_BITMAPTESTL(PJLB, INDEX)  \
    (JL_JLB_BITMAP(PJLB, JL_SUBEXPL(INDEX)) &  JL_BITPOSMASKL(INDEX))
#define JL_BITMAPSETL(PJLB, INDEX)   \
    (JL_JLB_BITMAP(PJLB, JL_SUBEXPL(INDEX)) |= JL_BITPOSMASKL(INDEX))
#define JL_BITMAPCLEARL(PJLB, INDEX) \
    (JL_JLB_BITMAP(PJLB, JL_SUBEXPL(INDEX)) ^= JL_BITPOSMASKL(INDEX))

// MAP BITMAP BIT OFFSET TO DIGIT:
// Given a digit variable to set, a bitmap branch or leaf subexpanse (base 0),
// the bitmap (BITMAP*_t) for that subexpanse, and an offset (Nth set bit in
// the bitmap, base 0), compute the digit (also base 0) corresponding to the
// subexpanse and offset by counting all bits in the bitmap until offset+1 set
// bits are seen.  Avoid expensive variable shifts.  Offset should be less than
// the number of set bits in the bitmap; assert this.
// If theres a better way to do this, I dont know what it is.
#define JL_BITMAPDIGITB(DIGIT,SUBEXP,BITMAP,OFFSET) {           \
            BITMAPB_t bitmap = (BITMAP); int remain = (OFFSET); \
            (DIGIT) = (SUBEXP) * cJL_BITSPERSUBEXPB;            \
            while ((remain -= (bitmap & 1)) >= 0) {             \
                bitmap >>= 1; ++(DIGIT);                        \
                assert((DIGIT) < ((SUBEXP) + 1) * cJL_BITSPERSUBEXPB); \
            }                                                   \
        }

#define JL_BITMAPDIGITL(DIGIT,SUBEXP,BITMAP,OFFSET) {           \
            BITMAPL_t bitmap = (BITMAP); int remain = (OFFSET); \
            (DIGIT) = (SUBEXP) * cJL_BITSPERSUBEXPL;            \
            while ((remain -= (bitmap & 1)) >= 0) {             \
                bitmap >>= 1; ++(DIGIT);                        \
                assert((DIGIT) < ((SUBEXP) + 1) * cJL_BITSPERSUBEXPL); \
            }                                                   \
        }

// MASKS FOR PORTIONS OF 32-BIT WORDS:
// These are useful for bitmap subexpanses.
// "LOWER"/"HIGHER" means bits representing lower/higher-valued Indexes.  The
// exact order of bits in the word is explicit here but is hidden from the
// caller.
// "EXC" means exclusive of the specified bit; "INC" means inclusive.
// In each case, BitPos is either "JL_BITPOSMASK*(BitNum)", or a variable saved
// from an earlier call of that macro; either way, it must be a 32-bit word
// with a single bit set.  In the first case, assume the compiler is smart
// enough to optimize out common subexpressions.
// The expressions depend on unsigned decimal math that should be universal.
#define JL_MASKLOWEREXC( BITPOS)  ((BITPOS) - 1)
#define JL_MASKLOWERINC( BITPOS)  (JL_MASKLOWEREXC(BITPOS) | (BITPOS))
#define JL_MASKHIGHERINC(BITPOS)  (-(BITPOS))
#define JL_MASKHIGHEREXC(BITPOS)  (JL_MASKHIGHERINC(BITPOS) ^ (BITPOS))

// Copy a series of generic objects (uint8_t, uint16_t, uint32_t, Word_t) from
// one place to another.
#define JL_COPYMEM(PDST,PSRC,POP1)                      \
    {                                                   \
        Word_t i_ndex = 0;                              \
        assert((POP1) > 0);                             \
        do { (PDST)[i_ndex] = (PSRC)[i_ndex]; } \
        while (++i_ndex < (POP1));                      \
    }

// Copy a 3-byte Index pointed by a uint8_t * to a Word_t:
#define JL_COPY3_PINDEX_TO_LONG(DESTLONG,PINDEX)        \
    DESTLONG  = (Word_t)(PINDEX)[0] << 16;              \
    DESTLONG += (Word_t)(PINDEX)[1] <<  8;              \
    DESTLONG += (Word_t)(PINDEX)[2]

// Copy a Word_t to a 3-byte Index pointed at by a uint8_t *:
#define JL_COPY3_LONG_TO_PINDEX(PINDEX,SOURCELONG)      \
    (PINDEX)[0] = (uint8_t)((SOURCELONG) >> 16);        \
    (PINDEX)[1] = (uint8_t)((SOURCELONG) >>  8);        \
    (PINDEX)[2] = (uint8_t)((SOURCELONG))

// These code chunks are shared between various source files.
// SET (REPLACE) ONE DIGIT IN AN INDEX:
// To avoid endian issues, use masking and ORing, which operates in a
// big-endian register, rather than treating the Index as an array of bytes,
// though that would be simpler, but would operate in endian-specific memory.
#define JL_SETDIGIT(INDEX,DIGIT,STATE)                  \
        (INDEX) = ((INDEX) & (~cJL_MASKATSTATE(STATE))) \
                | (((Word_t) (DIGIT)) << (((STATE) - 1) * cJL_BITSPERBYTE))
// Fast version for single LSB:
#define JL_SETDIGIT1(INDEX,DIGIT) (INDEX) = ((INDEX) & ~0xff) | (DIGIT)

// SET (REPLACE) "N" LEAST DIGITS IN AN INDEX:
#define JL_SETDIGITS(INDEX,INDEX2,cSTATE) \
        (INDEX) = ((INDEX ) & (~JL_LEASTBYTESMASK(cSTATE))) \
                | ((INDEX2) & ( JL_LEASTBYTESMASK(cSTATE)))

// COPY DECODE BYTES FROM JP TO INDEX:
// Modify Index digit(s) to match the bytes in jp_DcdPopO in case one or more
// branches are skipped and the digits are significant.  Its probably faster
// to just do this unconditionally than to check if its necessary.
// To avoid endian issues, use masking and ORing, which operates in a
// big-endian register, rather than treating the Index as an array of bytes,
// though that would be simpler, but would operate in endian-specific memory.
// WARNING:  Must not call JL_LEASTBYTESMASK (via cJL_DCDMASK) with Bytes =
// cJL_ROOTSTATE or a bad mask is generated, but there are no Dcd bytes to copy
// in this case anyway.  In fact there are no Dcd bytes unless State <
// cJL_ROOTSTATE - 1, so dont call this macro except in those cases.
#define JL_SETDCD(INDEX,PJP,cSTATE)                             \
    (INDEX) = ((INDEX) & ~cJL_DCDMASK(cSTATE)) | (JL_JPDCDPOP0(PJP) & cJL_DCDMASK(cSTATE))

// INSERT/DELETE AN INDEX IN-PLACE IN MEMORY:
// Given a pointer to an array of "even" (native), same-sized objects
// (indexes), the current population of the array, an offset in the array, and
// a new Index to insert, "shift up" the array elements (Indexes) above the
// insertion point and insert the new Index.  Assume there is sufficient memory
// to do this.
// In these macros, "i_offset" is an index offset, and "b_off" is a byte
// offset for odd Index sizes.
// Note:  Endian issues only arise fro insertion, not deletion, and even for
// insertion, they are transparent when native (even) objects are used, and
// handled explicitly for odd (non-native) Index sizes.
// Note:  The following macros are tricky enough that there is some test code
// for them appended to this file.
#define JL_INSERTINPLACE(PARRAY,POP1,OFFSET,INDEX)              \
        assert((long) (POP1) > 0);                              \
        assert((Word_t) (OFFSET) <= (Word_t) (POP1));           \
        {                                                       \
            Word_t i_offset = (POP1);                           \
            while (i_offset-- > (OFFSET))                       \
                (PARRAY)[i_offset + 1] = (PARRAY)[i_offset];    \
            (PARRAY)[OFFSET] = (INDEX);                         \
        }

// Variation for non-native Indexes, where cIS = Index Size
// and PByte must point to a uint8_t (byte); shift byte-by-byte:
#define JL_INSERTINPLACE3(PBYTE,POP1,OFFSET,INDEX)              \
{                                                               \
    Word_t i_off = POP1;                                        \
    while (i_off-- > (OFFSET)) {                                \
        Word_t  i_dx = i_off * 3;                               \
        (PBYTE)[i_dx + 0 + 3] = (PBYTE)[i_dx + 0];              \
        (PBYTE)[i_dx + 1 + 3] = (PBYTE)[i_dx + 1];              \
        (PBYTE)[i_dx + 2 + 3] = (PBYTE)[i_dx + 2];              \
    }                                                           \
    JL_COPY3_LONG_TO_PINDEX(&((PBYTE)[(OFFSET) * 3]), INDEX);   \
}

// Counterparts to the above for deleting an Index:
// "Shift down" the array elements starting at the Index to be deleted.
#define JL_DELETEINPLACE(PARRAY,POP1,OFFSET,IGNORE)             \
        assert((long) (POP1) > 0);                              \
        assert((Word_t) (OFFSET) < (Word_t) (POP1));            \
        {                                                       \
            Word_t i_offset = (OFFSET);                         \
            while (++i_offset < (POP1))                         \
                (PARRAY)[i_offset - 1] = (PARRAY)[i_offset];    \
        }

// Variation for odd-byte-sized (non-native) Indexes, where cIS = Index Size
// and PByte must point to a uint8_t (byte); copy byte-by-byte:
// Note:  If cIS == 1, JL_DELETEINPLACE_ODD == JL_DELETEINPLACE.
// Note:  There are no endian issues here because bytes are just shifted as-is,
// not converted to/from an Index.
#define JL_DELETEINPLACE_ODD(PBYTE,POP1,OFFSET,cIS)             \
        assert((long) (POP1) > 0);                              \
        assert((Word_t) (OFFSET) < (Word_t) (POP1));            \
        {                                                       \
            Word_t b_off = (((OFFSET) + 1) * (cIS)) - 1;        \
            while (++b_off < ((POP1) * (cIS)))                  \
                (PBYTE)[b_off - (cIS)] = (PBYTE)[b_off];        \
        }

// INSERT/DELETE AN INDEX WHILE COPYING OTHERS:
// Copy PSource[] to PDest[], where PSource[] has Pop1 elements (Indexes),
// inserting Index at PDest[Offset].  Unlike JL_*INPLACE*() above, these macros
// are used when moving Indexes from one memory object to another.
#define JL_INSERTCOPY(PDEST,PSOURCE,POP1,OFFSET,INDEX)          \
        assert((long) (POP1) > 0);                              \
        assert((Word_t) (OFFSET) <= (Word_t) (POP1));           \
        {                                                       \
            Word_t i_offset;                                    \
            for (i_offset = 0; i_offset < (OFFSET); ++i_offset) \
                (PDEST)[i_offset] = (PSOURCE)[i_offset];        \
            (PDEST)[i_offset] = (INDEX);                        \
            for (/* null */; i_offset < (POP1); ++i_offset)     \
                (PDEST)[i_offset + 1] = (PSOURCE)[i_offset];    \
        }

#define JL_INSERTCOPY3(PDEST,PSOURCE,POP1,OFFSET,INDEX)         \
assert((long) (POP1) > 0);                                      \
assert((Word_t) (OFFSET) <= (Word_t) (POP1));                   \
{                                                               \
    Word_t o_ff;                                                \
    for (o_ff = 0; o_ff < (OFFSET); o_ff++) {                   \
        Word_t  i_dx = o_ff * 3;                                \
        (PDEST)[i_dx + 0] = (PSOURCE)[i_dx + 0];                \
        (PDEST)[i_dx + 1] = (PSOURCE)[i_dx + 1];                \
        (PDEST)[i_dx + 2] = (PSOURCE)[i_dx + 2];                \
    }                                                           \
    JL_COPY3_LONG_TO_PINDEX(&((PDEST)[(OFFSET) * 3]), INDEX);   \
    for (; o_ff < (POP1); o_ff++) {		                \
        Word_t  i_dx = o_ff * 3;                                \
        (PDEST)[i_dx + 0 + 3] = (PSOURCE)[i_dx + 0];            \
        (PDEST)[i_dx + 1 + 3] = (PSOURCE)[i_dx + 1];            \
        (PDEST)[i_dx + 2 + 3] = (PSOURCE)[i_dx + 2];            \
    }                                                           \
}

// Counterparts to the above for deleting an Index:
#define JL_DELETECOPY(PDEST,PSOURCE,POP1,OFFSET,IGNORE)         \
        assert((long) (POP1) > 0);                              \
        assert((Word_t) (OFFSET) < (Word_t) (POP1));            \
        {                                                       \
            Word_t i_offset;                                    \
            for (i_offset = 0; i_offset < (OFFSET); ++i_offset) \
                (PDEST)[i_offset] = (PSOURCE)[i_offset];        \
            for (++i_offset; i_offset < (POP1); ++i_offset)     \
                (PDEST)[i_offset - 1] = (PSOURCE)[i_offset];    \
        }

// Variation for odd-byte-sized (non-native) Indexes, where cIS = Index Size;
// copy byte-by-byte:
// Note:  There are no endian issues here because bytes are just shifted as-is,
// not converted to/from an Index.
// Note:  If cIS == 1, JL_DELETECOPY_ODD == JL_DELETECOPY, at least in concept.
#define JL_DELETECOPY_ODD(PDEST,PSOURCE,POP1,OFFSET,cIS)  {		\
        uint8_t *_Pdest   = (uint8_t *) (PDEST);			\
        uint8_t *_Psource = (uint8_t *) (PSOURCE);			\
        Word_t   b_off;							\
	assert((long) (POP1) > 0);					\
	assert((Word_t) (OFFSET) < (Word_t) (POP1));			\
        for (b_off = 0; b_off < ((OFFSET) * (cIS)); ++b_off)		\
		*_Pdest++ = *_Psource++;				\
        _Psource += (cIS);						\
        for (b_off += (cIS); b_off < ((POP1) * (cIS)); ++b_off)		\
		*_Pdest++ = *_Psource++;				\
}

#define JERRI ((int) ~0)

#define JL_RET_NOTFOUND				\
	return((PPvoid_t) NULL)
#define JL_RET_FOUND_JPM(Pjpm)			\
	return((PPvoid_t) ((Pjpm)->jpm_PValue))
#define JL_RET_FOUND_PVALUE(Pjv,OFFSET)		\
	return((PPvoid_t) ((Pjv) + OFFSET))
#define JL_RET_FOUND_LEAFW(PJLW,POP1,OFFSET)	\
        return((PPvoid_t) (JL_LEAFWVALUEAREA(PJLW, POP1) + (OFFSET)))
#define JL_RET_FOUND_LEAF1(Pjll,POP1,OFFSET)	\
        return((PPvoid_t) (JL_LEAF1VALUEAREA(Pjll, POP1) + (OFFSET)))
#define JL_RET_FOUND_LEAF2(Pjll,POP1,OFFSET)	\
        return((PPvoid_t) (JL_LEAF2VALUEAREA(Pjll, POP1) + (OFFSET)))
#define JL_RET_FOUND_LEAF3(Pjll,POP1,OFFSET)	\
        return((PPvoid_t) (JL_LEAF3VALUEAREA(Pjll, POP1) + (OFFSET)))
#define JL_RET_FOUND_IMM_01(PJP)		\
	return((PPvoid_t) (&((PJP)->jp_Addr)))
#define JL_RET_FOUND_IMM(PJP,OFFSET)		\
	return ((PPvoid_t)(P_JV((PJP)->jp_Addr) + (OFFSET)))
#define JL_RET_FOUND_LEAF_B1(PJLB,SUBEXP,OFFSET) \
        return((PPvoid_t) (P_JV(JL_JLB_PVALUE(PJLB, SUBEXP)) + (OFFSET)))

#define JL_SET_ERRNO(PJError, JErrno)  {          \
      assert((JErrno) != JL_ERRNO_OVERRUN);       \
      assert((JErrno) != JL_ERRNO_CORRUPT);       \
      if (PJError != (PJError_t) NULL) {          \
             JL_ERRNO(PJError) = (JErrno);        \
             JL_ERRID(PJError) = __LINE__;        \
      }                                           \
}
#define JL_SET_ERRNO_NONNULL(PJError, JErrno)   {       \
        assert((JErrno) != JL_ERRNO_OVERRUN);		\
        assert((JErrno) != JL_ERRNO_CORRUPT);		\
        assert(PJError);				\
        JL_ERRNO(PJError) = (JErrno);			\
        JL_ERRID(PJError) = __LINE__;			\
}
#define JL_COPY_ERRNO(PJError, Pjpm)                            \
       if (PJError) {                                           \
		JL_ERRNO(PJError) = (uint8_t)JL_ERRNO(Pjpm);    \
                JL_ERRID(PJError) = JL_ERRID(Pjpm);             \
       }
#define JL_ALLOC_ERRNO(ADDR) \
        (((void *) (ADDR) != (void *) NULL) ? JL_ERRNO_OVERRUN : JL_ERRNO_NOMEM)

#define JL_CHECKALLOC(Type,Ptr,Retval)                  \
        if ((Ptr) < (Type) sizeof(Word_t)) {            \
            JL_SET_ERRNO(PJError, JL_ALLOC_ERRNO(Ptr)); \
            return(Retval);                             \
        }
// Leaf search routines
static inline int judySearchLeaf1(Pjll_t Pjll, Word_t LeafPop1, Word_t Index)
{
	SEARCHLEAFNATIVE(uint8_t, Pjll, LeafPop1, Index);
}

static inline int judySearchLeaf2(Pjll_t Pjll, Word_t LeafPop1, Word_t Index)
{
	SEARCHLEAFNATIVE(uint16_t, Pjll, LeafPop1, Index);
}

static inline int judySearchLeaf3(Pjll_t Pjll, Word_t LeafPop1, Word_t Index)
{
	SEARCHLEAFNONNAT(Pjll, LeafPop1, Index, 3, JL_COPY3_PINDEX_TO_LONG);
}

static inline int judySearchLeafW(Pjlw_t Pjlw, Word_t LeafPop1, Word_t Index)
{
	SEARCHLEAFNATIVE(Word_t, Pjlw, LeafPop1, Index);
}
#endif // ! _JUDYPRIVATE_INCLUDED
