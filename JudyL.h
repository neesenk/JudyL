#ifndef __JUDYL_INCLUDED__
#define __JUDYL_INCLUDED__

#include "Judy.h"
#include <assert.h>

/* assert(sizeof(Word_t) == sizeof(void *)) */
typedef unsigned long Word_t, *PWord_t;

/* Machine (CPU) cache line size:
 * NOTE:  A leaf size of 2 cache lines maximum is the target (optimal) for
 * Judy.  Its hard to obtain a machines cache line size at compile time, but
 * if the machine has an unexpected cache line size, its not devastating if
 * the following constants end up causing leaves that are 1 cache line in size,
 * or even 4 cache lines in size.  The assumed 32-bit system has 16-word =
 * 64-byte cache lines, and the assumed 64-bit system has 16-word = 128-byte
 * cache lines. */
#define cJL_BYTESPERCL  64	/* cache line size in bytes. */
#define cJL_BITSPERBYTE 0x8

#define cJL_BYTESPERPTR (sizeof(Word_t))

/* Bytes Per Word and Bits Per Word, latter assuming sizeof(byte) is 8 bits:
 * Expect 32 [64] bits per word. */
#define cJL_BYTESPERWORD (sizeof(uint32_t))
#define cJL_BITSPERWORD  (sizeof(uint32_t) * cJL_BITSPERBYTE)

/* A word that is all-ones, normally equal to -1UL, but safer with ~0: */
#define cJL_ALLONES  (~0UL)

/* Note, these are forward references, but thats OK: */
#define cJL_FULLBITMAPB ((BITMAPB_t) cJL_ALLONES)
#define cJL_FULLBITMAPL ((BITMAPL_t) cJL_ALLONES)

/* ROOT STATE:
 * State at the start of the Judy SM, based on 1 byte decoded per state; equal
 * to the number of bytes per Index to decode. */
#define cJL_ROOTSTATE (sizeof(uint32_t))

/* SUBEXPANSES PER STATE:
 * Number of subexpanses per state traversed, which is the number of JPs in a
 * branch (actual or theoretical) and the number of bits in a bitmap. */
#define cJL_SUBEXPPERSTATE  256

/* LEAF AND VALUE POINTERS:
 * Some other basic object types are in declared in JudyPrivateBranch.h
 * (Pjbl_t, Pjbb_t, Pjbu_t, Pjp_t) or are Judy1/L-specific (Pjlb_t).  The
 * few remaining types are declared below.
 * Note:  Leaf pointers are cast to different-sized objects depending on the
 * leafs level, but are at least addresses (not just numbers), so use void *
 * (void *), not PWord_t or Word_t for them, except use Pjlw_t for whole-word
 * (top-level, root-level) leaves.  Value areas, however, are always whole
 * words.
 * Furthermore, use Pjll_t only for generic leaf pointers (for various size
 * LeafLs).  Use Pjlw_t for LeafWs.  Use Pleaf (with type uint8_t *, uint16_t
 * *, etc) when the leaf index size is known. */
typedef void * Pjll_t;		/* pointer to lower-level linear leaf. */
typedef PWord_t Pjlw_t;		/* pointer to root-level leaf (whole-word indexes). */
typedef PWord_t Pjv_t;		/* pointer to JudyL value area. */

/* POINTER PREPARATION MACROS:
 * These macros are used to strip malloc-namespace-type bits from a pointer +
 * malloc-type word (which references any Judy mallocd object that might be
 * obtained from other than a direct call of malloc()), prior to dereferencing
 * the pointer as an address.  The malloc-type bits allow Judy mallocd objects
 * to come from different "malloc() namespaces".
 *    (root pointer)    (JRP, see above)
 *    jp.jp_Addr        generic pointer to next-level node, except when used
 *                      as a JudyL Immed01 value area
 *    JL_JBB_PJP        macro hides jbbs_Pjp (pointer to JP subarray)
 *    JL_JLB_PVALUE     macro hides jLlbs_PValue (pointer to value subarray)
 * When setting one of these fields or passing an address to judyFree*(), the
 * "raw" memory address is used; otherwise the memory address must be passed
 * through one of the macros below before its dereferenced.
 * Note:  After much study, the typecasts below appear in the macros rather
 * than at the point of use, which is both simpler and allows the compiler to
 * do type-checking. */
#define P_JLW(  ADDR) ((Pjlw_t) (ADDR))	/* root leaf. */
#define P_JPM(  ADDR) ((Pjpm_t) (ADDR))	/* root JPM. */
#define P_JBL(  ADDR) ((Pjbl_t) (ADDR))	/* BranchL. */
#define P_JBB(  ADDR) ((Pjbb_t) (ADDR))	/* BranchB. */
#define P_JBU(  ADDR) ((Pjbu_t) (ADDR))	/* BranchU. */
#define P_JLL(  ADDR) ((Pjll_t) (ADDR))	/* LeafL. */
#define P_JLB(  ADDR) ((Pjlb_t) (ADDR))	/* LeafB1. */
#define P_JP(   ADDR) ((Pjp_t)  (ADDR))	/* JP. */
#define P_JV(   ADDR) ((Pjv_t)  (ADDR))	/* &value. */

/* LEAST BYTES:
 * Mask for least bytes of a word, and a macro to perform this mask on an
 * Index.
 * Note:  This macro has been problematic in the past to get right and to make
 * portable.  Its not OK on all systems to shift by the full word size.  This
 * macro should allow shifting by 1..N bytes, where N is the word size, but
 * should produce a compiler warning if the macro is called with Bytes == 0.
 * Warning:  JL_LEASTBYTESMASK() is not a constant macro unless Bytes is a
 * constant; otherwise it is a variable shift, which is expensive on some
 * processors. */
#define JL_LEASTBYTESMASK(BYTES) \
        ((0x100UL << (cJL_BITSPERBYTE * ((BYTES) - 1))) - 1)
#define JL_LEASTBYTES(INDEX,BYTES)  ((INDEX) & JL_LEASTBYTESMASK(BYTES))

/* BITS IN EACH BITMAP SUBEXPANSE FOR BITMAP BRANCH AND LEAF:
 * The bits per bitmap subexpanse times the number of subexpanses equals a
 * constant (cJL_SUBEXPPERSTATE).  You can also think of this as a compile-time
 * choice of "aspect ratio" for bitmap branches and leaves (which can be set
 * independently for each).
 * A default aspect ratio is hardwired here if not overridden at compile time,
 * such as by "EXTCCOPTS=-DBITMAP_BRANCH16x16 make". */
#define BITMAPB_t uint32_t
#define BITMAPL_t uint32_t

extern const uint8_t jL_BranchBJPPopToWords[];
#define SEARCHLEAFNATIVE(LEAFTYPE,ADDR,POP1,INDEX)              \
    LEAFTYPE *P_leaf = (LEAFTYPE *)(ADDR);                      \
    LEAFTYPE I_ndex = (LEAFTYPE)INDEX; /* truncate hi bits */   \
    Word_t l_ow   = cJL_ALLONES;				\
    Word_t m_id;						\
    Word_t h_igh  = POP1;					\
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
    Word_t l_ow   = cJL_ALLONES;				\
    Word_t m_id;						\
    Word_t h_igh  = POP1;					\
    Word_t I_ndex = JL_LEASTBYTES((INDEX), (LFBTS));		\
    Word_t i_ndex;						\
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

static inline BITMAPL_t judyCountBits(BITMAPL_t word)
{
	word = (word & 0x55555555) + ((word & 0xAAAAAAAA) >> 1);
	word = (word & 0x33333333) + ((word & 0xCCCCCCCC) >> 2);
	word = (word & 0x0F0F0F0F) + ((word & 0xF0F0F0F0) >> 4);	/* >= 8 bits. */
	word = (word & 0x00FF00FF) + ((word & 0xFF00FF00) >> 8);	/* >= 16 bits. */
	word = (word & 0x0000FFFF) + ((word & 0xFFFF0000) >> 16);	/* >= 32 bits. */
	return (word);
}

/* Get from jp_DcdPopO the Pop0 for various JP Types.
 * Notes:
 * - Different macros require different parameters...
 * - There are no simple macros for cJL_BRANCH* Types because their
 *   populations must be added up and dont reside in an already-calculated
 *   place.  (TBD:  This is no longer true, now its in the JPM.)
 * - cJL_JPIMM_POP0() is not defined because it would be redundant because the
 *   Pop1 is already encoded in each enum name.
 * - A linear or bitmap leaf Pop0 cannot exceed cJL_SUBEXPPERSTATE - 1 (Pop0 =
 *   0..255), so use a simpler, faster macro for it than for other JP Types.
 * - Avoid any complex calculations that would slow down the compiled code.
 *   Assume these macros are only called for the appropriate JP Types.
 *   Unfortunately theres no way to trigger an assertion here if the JP type
 *   is incorrect for the macro, because these are merely expressions, not
 *   statements. */
#define JL_LEAFW_POP0(JRP)	(*P_JLW(JRP))
#define cJL_JPFULLPOPU1_POP0	(cJL_SUBEXPPERSTATE - 1)

/* GET JP Type:
 * Since bit fields greater than 32 bits are not supported in some compilers
 * the jp_DcdPopO field is expanded to include the jp_Type in the high 8 bits
 * of the Word_t .
 * First the read macro: */
#define JL_JPTYPE(PJP)	((PJP)->jp_Type)
#define JL_JPLEAF_POP0(PJP) ((PJP)->jp_DcdP0[2])
#define JL_JPDCDPOP0(PJP) ((Word_t)(PJP)->jp_DcdP0[0] << 16 | \
			   (Word_t)(PJP)->jp_DcdP0[1] <<  8 | \
			   (Word_t)(PJP)->jp_DcdP0[2])

#define JL_JPSETADT(PJP,ADDR,DCDPOP0,TYPE) do {                 \
    (PJP)->jp_Addr     = (ADDR);                                \
    (PJP)->jp_DcdP0[0] = (uint8_t)((Word_t)(DCDPOP0) >> 16);    \
    (PJP)->jp_DcdP0[1] = (uint8_t)((Word_t)(DCDPOP0) >>  8);    \
    (PJP)->jp_DcdP0[2] = (uint8_t)((Word_t)(DCDPOP0));          \
    (PJP)->jp_Type     = (TYPE);                                \
} while (0)

/* NUMBER OF BITS IN A BRANCH OR LEAF BITMAP AND SUBEXPANSE:
 * Note:  cJL_BITSPERBITMAP must be the same as the number of JPs in a branch. */
#define cJL_BITSPERBITMAP cJL_SUBEXPPERSTATE

/* Bitmaps are accessed in units of "subexpanses": */
#define cJL_BITSPERSUBEXPB  (sizeof(BITMAPB_t) * cJL_BITSPERBYTE)
#define cJL_NUMSUBEXPB      (cJL_BITSPERBITMAP / cJL_BITSPERSUBEXPB)
#define cJL_BITSPERSUBEXPL  (sizeof(BITMAPL_t) * cJL_BITSPERBYTE)
#define cJL_NUMSUBEXPL      (cJL_BITSPERBITMAP / cJL_BITSPERSUBEXPL)

/* MASK FOR A SPECIFIED BIT IN A BITMAP:
 * Warning:  If BitNum is a variable, this results in a variable shift that is
 * expensive, at least on some processors.  Use with caution.
 * Warning:  BitNum must be less than cJL_BITSPERWORD, that is, 0 ..
 * cJL_BITSPERWORD - 1, to avoid a truncated shift on some machines. */
#define JL_BITPOSMASKB(BITNUM) (1L << ((BITNUM) % cJL_BITSPERSUBEXPB))
#define JL_BITPOSMASKL(BITNUM) (1L << ((BITNUM) % cJL_BITSPERSUBEXPL))

/* TEST/SET/CLEAR A BIT IN A BITMAP LEAF:
 * Test if a byte-sized Digit (portion of Index) has a corresponding bit set in
 * a bitmap, or set a byte-sized Digits bit into a bitmap, by looking up the
 * correct subexpanse and then checking/setting the correct bit.
 * Note:  Mask higher bits, if any, for the convenience of the user of this
 * macro, in case they pass a full Index, not just a digit.  If the caller has
 * a true 8-bit digit, make it of type uint8_t and the compiler should skip the
 * unnecessary mask step. */
#define JL_SUBEXPL(DIGIT) (((DIGIT) / cJL_BITSPERSUBEXPL) & (cJL_NUMSUBEXPL-1))
#define JL_BITMAPTESTL(PJLB, INDEX)  \
    (JL_JLB_BITMAP(PJLB, JL_SUBEXPL(INDEX)) &  JL_BITPOSMASKL(INDEX))
#define JL_BITMAPSETL(PJLB, INDEX)   \
    (JL_JLB_BITMAP(PJLB, JL_SUBEXPL(INDEX)) |= JL_BITPOSMASKL(INDEX))
#define JL_BITMAPCLEARL(PJLB, INDEX) \
    (JL_JLB_BITMAP(PJLB, JL_SUBEXPL(INDEX)) ^= JL_BITPOSMASKL(INDEX))

/* MAP BITMAP BIT OFFSET TO DIGIT:
 * Given a digit variable to set, a bitmap branch or leaf subexpanse (base 0),
 * the bitmap (BITMAP*_t) for that subexpanse, and an offset (Nth set bit in
 * the bitmap, base 0), compute the digit (also base 0) corresponding to the
 * subexpanse and offset by counting all bits in the bitmap until offset+1 set
 * bits are seen.  Avoid expensive variable shifts.  Offset should be less than
 * the number of set bits in the bitmap; assert this.
 * If theres a better way to do this, I dont know what it is. */
#define JL_BITMAPDIGITB(DIGIT,SUBEXP,BITMAP,OFFSET) do {		\
        BITMAPB_t bitmap = (BITMAP); int remain = (OFFSET);		\
        (DIGIT) = (SUBEXP) * cJL_BITSPERSUBEXPB;			\
        while ((remain -= (bitmap & 1)) >= 0) {				\
                bitmap >>= 1; ++(DIGIT);				\
                assert((DIGIT) < ((SUBEXP) + 1) * cJL_BITSPERSUBEXPB);	\
        }								\
} while (0)

#define JL_BITMAPDIGITL(DIGIT,SUBEXP,BITMAP,OFFSET) do {		\
        BITMAPL_t bitmap = (BITMAP); int remain = (OFFSET);		\
        (DIGIT) = (SUBEXP) * cJL_BITSPERSUBEXPL;			\
        while ((remain -= (bitmap & 1)) >= 0) {				\
		bitmap >>= 1; ++(DIGIT);				\
                assert((DIGIT) < ((SUBEXP) + 1) * cJL_BITSPERSUBEXPL);	\
	}								\
} while (0)

/* MASKS FOR PORTIONS OF 32-BIT WORDS:
 * These are useful for bitmap subexpanses.
 * "LOWER"/"HIGHER" means bits representing lower/higher-valued Indexes.  The
 * exact order of bits in the word is explicit here but is hidden from the
 * caller.
 * "EXC" means exclusive of the specified bit; "INC" means inclusive.
 * In each case, BitPos is either "JL_BITPOSMASK*(BitNum)", or a variable saved
 * from an earlier call of that macro; either way, it must be a 32-bit word
 * with a single bit set.  In the first case, assume the compiler is smart
 * enough to optimize out common subexpressions.
 * The expressions depend on unsigned decimal math that should be universal. */
#define JL_MASKLOWEREXC( BITPOS)  ((BITPOS) - 1)
#define JL_MASKLOWERINC( BITPOS)  (JL_MASKLOWEREXC(BITPOS) | (BITPOS))
#define JL_MASKHIGHERINC(BITPOS)  (-(BITPOS))
#define JL_MASKHIGHEREXC(BITPOS)  (JL_MASKHIGHERINC(BITPOS) ^ (BITPOS))

/* Copy a series of generic objects (uint8_t, uint16_t, uint32_t, Word_t) from
 * one place to another. */
#define JL_COPYMEM(PDST,PSRC,POP1) do {                 \
        Word_t i_ndex = 0;                              \
        assert((POP1) > 0);                             \
        do { (PDST)[i_ndex] = (PSRC)[i_ndex]; }		\
        while (++i_ndex < (POP1));                      \
} while (0)

/* Copy a 3-byte Index pointed by a uint8_t * to a Word_t : */
#define JL_COPY3_PINDEX_TO_LONG(DESTLONG,PINDEX)        \
    DESTLONG  = (Word_t)(PINDEX)[0] << 16;              \
    DESTLONG += (Word_t)(PINDEX)[1] <<  8;              \
    DESTLONG += (Word_t)(PINDEX)[2]

/* Copy a Word_t to a 3-byte Index pointed at by a uint8_t *: */
#define JL_COPY3_LONG_TO_PINDEX(PINDEX,SOURCELONG)      \
    (PINDEX)[0] = (uint8_t)((SOURCELONG) >> 16);        \
    (PINDEX)[1] = (uint8_t)((SOURCELONG) >>  8);        \
    (PINDEX)[2] = (uint8_t)((SOURCELONG))

/* These code chunks are shared between various source files.
 * SET (REPLACE) ONE DIGIT IN AN INDEX:
 * To avoid endian issues, use masking and ORing, which operates in a
 * big-endian register, rather than treating the Index as an array of bytes,
 * though that would be simpler, but would operate in endian-specific memory. */
#define JL_SETDIGIT(INDEX,DIGIT,STATE)                  \
        (INDEX) = ((INDEX) & (~cJL_MASKATSTATE(STATE))) \
                | (((Word_t) (DIGIT)) << (((STATE) - 1) * cJL_BITSPERBYTE))

/* Fast version for single LSB: */
#define JL_SETDIGIT1(INDEX,DIGIT) (INDEX) = ((INDEX) & ~0xff) | (DIGIT)

/* SET (REPLACE) "N" LEAST DIGITS IN AN INDEX: */
#define JL_SETDIGITS(INDEX,INDEX2,cSTATE) \
        (INDEX) = ((INDEX ) & (~JL_LEASTBYTESMASK(cSTATE))) \
                | ((INDEX2) & ( JL_LEASTBYTESMASK(cSTATE)))

/* COPY DECODE BYTES FROM JP TO INDEX:
 * Modify Index digit(s) to match the bytes in jp_DcdPopO in case one or more
 * branches are skipped and the digits are significant.  Its probably faster
 * to just do this unconditionally than to check if its necessary.
 * To avoid endian issues, use masking and ORing, which operates in a
 * big-endian register, rather than treating the Index as an array of bytes,
 * though that would be simpler, but would operate in endian-specific memory.
 * WARNING:  Must not call JL_LEASTBYTESMASK (via cJL_DCDMASK) with Bytes =
 * cJL_ROOTSTATE or a bad mask is generated, but there are no Dcd bytes to copy
 * in this case anyway.  In fact there are no Dcd bytes unless State <
 * cJL_ROOTSTATE - 1, so dont call this macro except in those cases. */
#define JL_SETDCD(INDEX,PJP,cSTATE)                             \
    (INDEX) = ((INDEX) & ~cJL_DCDMASK(cSTATE)) | (JL_JPDCDPOP0(PJP) & cJL_DCDMASK(cSTATE))

/* INSERT/DELETE AN INDEX IN-PLACE IN MEMORY:
 * Given a pointer to an array of "even" (native), same-sized objects
 * (indexes), the current population of the array, an offset in the array, and
 * a new Index to insert, "shift up" the array elements (Indexes) above the
 * insertion point and insert the new Index.  Assume there is sufficient memory
 * to do this.
 * In these macros, "i_offset" is an index offset, and "b_off" is a byte
 * offset for odd Index sizes.
 * Note:  Endian issues only arise fro insertion, not deletion, and even for
 * insertion, they are transparent when native (even) objects are used, and
 * handled explicitly for odd (non-native) Index sizes.
 * Note:  The following macros are tricky enough that there is some test code
 * for them appended to this file. */
#define JL_INSERTINPLACE(PARRAY,POP1,OFFSET,INDEX)              \
        assert((long) (POP1) > 0);                              \
        assert((Word_t) (OFFSET) <= (Word_t) (POP1));           \
        {                                                       \
            Word_t i_offset = (POP1);                           \
            while (i_offset-- > (OFFSET))                       \
                (PARRAY)[i_offset + 1] = (PARRAY)[i_offset];    \
            (PARRAY)[OFFSET] = (INDEX);                         \
        }

/* Variation for non-native Indexes, where cIS = Index Size
 * and PByte must point to a uint8_t (byte); shift byte-by-byte: */
#define JL_INSERTINPLACE3(PBYTE,POP1,OFFSET,INDEX) do {		\
	Word_t i_off = POP1;                                    \
	while (i_off-- > (OFFSET)) {                            \
		Word_t i_dx = i_off * 3;			\
		(PBYTE)[i_dx + 0 + 3] = (PBYTE)[i_dx + 0];      \
		(PBYTE)[i_dx + 1 + 3] = (PBYTE)[i_dx + 1];      \
		(PBYTE)[i_dx + 2 + 3] = (PBYTE)[i_dx + 2];      \
	}                                                       \
	JL_COPY3_LONG_TO_PINDEX(&(PBYTE)[(OFFSET) * 3], INDEX);	\
} while (0)

/* Counterparts to the above for deleting an Index:
 * "Shift down" the array elements starting at the Index to be deleted. */
#define JL_DELETEINPLACE(PARRAY,POP1,OFFSET,IGNORE) do {        \
        Word_t i_offset = (OFFSET);				\
	Word_t i_pop1 = (POP1);					\
        assert((long)i_pop1 > 0);				\
        assert(i_offset < i_pop1);				\
        while (++i_offset < i_pop1)				\
                (PARRAY)[i_offset - 1] = (PARRAY)[i_offset];    \
} while (0)

/* Variation for odd-byte-sized (non-native) Indexes, where cIS = Index Size
 * and PByte must point to a uint8_t (byte); copy byte-by-byte:
 * Note:  If cIS == 1, JL_DELETEINPLACE_ODD == JL_DELETEINPLACE.
 * Note:  There are no endian issues here because bytes are just shifted as-is,
 * not converted to/from an Index. */
#define JL_DELETEINPLACE_ODD(PBYTE,POP1,OFFSET,cIS) do {        \
        Word_t b_off = (((OFFSET) + 1) * (cIS)) - 1;		\
        assert((long) (POP1) > 0);                              \
        assert((Word_t) (OFFSET) < (Word_t) (POP1));            \
        while (++b_off < ((POP1) * (cIS)))			\
                (PBYTE)[b_off - (cIS)] = (PBYTE)[b_off];        \
} while (0)

/* INSERT/DELETE AN INDEX WHILE COPYING OTHERS:
 * Copy PSource[] to PDest[], where PSource[] has Pop1 elements (Indexes),
 * inserting Index at PDest[Offset].  Unlike JL_*INPLACE*() above, these macros
 * are used when moving Indexes from one memory object to another. */
#define JL_INSERTCOPY(PDEST,PSOURCE,POP1,OFFSET,INDEX) do {     \
        Word_t i_offset;					\
        assert((long) (POP1) > 0);                              \
        assert((Word_t) (OFFSET) <= (Word_t) (POP1));           \
        for (i_offset = 0; i_offset < (OFFSET); ++i_offset)	\
                (PDEST)[i_offset] = (PSOURCE)[i_offset];        \
        (PDEST)[i_offset] = (INDEX);				\
        for (/* null */; i_offset < (POP1); ++i_offset)		\
                (PDEST)[i_offset + 1] = (PSOURCE)[i_offset];    \
} while (0)

#define JL_INSERTCOPY3(PDEST,PSOURCE,POP1,OFFSET,INDEX)  do {   \
	Word_t o_ff;                                            \
	assert((long) (POP1) > 0);                              \
	assert((Word_t) (OFFSET) <= (Word_t) (POP1));           \
	for (o_ff = 0; o_ff < (OFFSET); o_ff++) {               \
		Word_t i_dx = o_ff * 3;				\
		(PDEST)[i_dx + 0] = (PSOURCE)[i_dx + 0];        \
		(PDEST)[i_dx + 1] = (PSOURCE)[i_dx + 1];        \
		(PDEST)[i_dx + 2] = (PSOURCE)[i_dx + 2];        \
	}                                                       \
	JL_COPY3_LONG_TO_PINDEX(&(PDEST)[(OFFSET) * 3], INDEX); \
	for (; o_ff < (POP1); o_ff++) {		                \
		Word_t i_dx = o_ff * 3;				\
		(PDEST)[i_dx + 0 + 3] = (PSOURCE)[i_dx + 0];    \
		(PDEST)[i_dx + 1 + 3] = (PSOURCE)[i_dx + 1];    \
		(PDEST)[i_dx + 2 + 3] = (PSOURCE)[i_dx + 2];    \
	}                                                       \
} while (0)

/* Counterparts to the above for deleting an Index: */
#define JL_DELETECOPY(PDEST,PSOURCE,POP1,OFFSET,IGNORE) do {    \
        Word_t i_offset;					\
        assert((long) (POP1) > 0);                              \
        assert((Word_t) (OFFSET) < (Word_t) (POP1));            \
        for (i_offset = 0; i_offset < (OFFSET); ++i_offset)	\
                (PDEST)[i_offset] = (PSOURCE)[i_offset];        \
        for (++i_offset; i_offset < (POP1); ++i_offset)		\
                (PDEST)[i_offset - 1] = (PSOURCE)[i_offset];    \
} while (0)

/* Variation for odd-byte-sized (non-native) Indexes, where cIS = Index Size;
 * copy byte-by-byte:
 * Note:  There are no endian issues here because bytes are just shifted as-is,
 * not converted to/from an Index.
 * Note:  If cIS == 1, JL_DELETECOPY_ODD == JL_DELETECOPY, at least in concept. */
#define JL_DELETECOPY_ODD(PDEST,PSOURCE,POP1,OFFSET,cIS)  do {		\
        uint8_t *_Pdest   = (uint8_t *) (PDEST);			\
        uint8_t *_Psource = (uint8_t *) (PSOURCE);			\
        Word_t b_off;							\
	assert((long) (POP1) > 0);					\
	assert((Word_t) (OFFSET) < (Word_t) (POP1));			\
        for (b_off = 0; b_off < ((OFFSET) * (cIS)); ++b_off)		\
		*_Pdest++ = *_Psource++;				\
        _Psource += (cIS);						\
        for (b_off += (cIS); b_off < ((POP1) * (cIS)); ++b_off)		\
		*_Pdest++ = *_Psource++;				\
} while (0)

#define JL_ALLOC_ERRNO(ADDR)		\
        (((void *) (ADDR) != NULL) ? JLE_OVERRUN : JLE_NOMEM)

#define JL_CHECKALLOC(Type, Ptr, Retval) do {				\
        if ((Ptr) < (Type) sizeof(Word_t)) {				\
		JL_SET_ERRNO(JL_ALLOC_ERRNO(Ptr));			\
		return(Retval);						\
        }								\
} while (0)

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
	Word_t j_po_Addr;	/* first word:  Pjp_t, Word_t, etc. */
	union {
		uint8_t j_po_DcdP0[sizeof(Word_t) - 1];
		uint8_t j_po_Bytes[sizeof(Word_t)];	/* last byte = jp_Type. */
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
	uint8_t j_pi_Type; /* JP type, 1 of cJ*_JPIMMED*. */
} jpi_t;

/* UNION OF JP TYPES:
 * A branch is an array of cJL_BRANCHUNUMJPS (256) of this object, or an
 * alternate data type such as:  A linear branch which is a list of 2..7 JPs,
 * or a bitmap branch which contains 8 lists of 0..32 JPs.  JPs reside only in
 * branches of a Judy SM.
 */
typedef union JUDY_POINTER {
	jpo_t j_po;		/* other than immediate indexes. */
	jpi_t j_pi;		/* immediate indexes. */
} jp_t, *Pjp_t;

#define jp_LIndex  j_pi.j_pi_LIndex	/* for storing Indexes in second word. */
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
#define cJL_DCDMASK(cPopBytes) (0x00ffffffUL & (~cJL_POP0MASK(cPopBytes)))

/* Mask off the high byte from INDEX to it can be compared to DcdPopO: */
#define JL_TRIMTODCDSIZE(INDEX) (0x00ffffffUL & (INDEX))

/* Get from jp_DcdPopO the Pop0 for various branch JP Types:
 * There are no simple macros for cJL_BRANCH* Types because their populations
 * must be added up and dont reside in an already-calculated place. */
#define JL_JPBRANCH_POP0(PJP,cPopBytes) \
        (JL_JPDCDPOP0(PJP) & cJL_POP0MASK(cPopBytes))

/* METHOD FOR DETERMINING IF OBJECTS HAVE ROOM TO GROW:
 * J__U_GROWCK() is a generic method to determine if an object can grow in
 * place, based on whether the next population size (one more) would use the
 * same space. */
#define J__U_GROWCK(POP1,MAXPOP1,POPTOWORDS) \
        (((POP1) != (MAXPOP1)) && (POPTOWORDS[POP1] == POPTOWORDS[(POP1) + 1]))
#define JL_BRANCHBJPGROWINPLACE(NumJPs) \
        J__U_GROWCK(NumJPs, cJL_BITSPERSUBEXPB, jL_BranchBJPPopToWords)

/* DETERMINE IF AN INDEX IS (NOT) IN A JPS EXPANSE: */
#define JL_DCDNOTMATCHINDEX(INDEX,PJP,POP0BYTES) \
        (((INDEX) ^ JL_JPDCDPOP0(PJP)) & cJL_DCDMASK(POP0BYTES))

/* NUMBER OF JPs IN AN UNCOMPRESSED BRANCH:
 * An uncompressed branch is simply an array of 256 Judy Pointers (JPs).  It is
 * a minimum cacheline fill object.  Define it here before its first needed. */
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

/* LINEAR BRANCH STRUCT:
 * 1-byte count, followed by array of byte-sized expanses, followed by JPs. */
typedef struct JUDY_BRANCH_LINEAR {
	uint8_t jbl_NumJPs;	/* num of JPs (Pjp_t), 1..N. */
	uint8_t jbl_Expanse[cJL_BRANCHLMAXJPS];	/* 1..7 MSbs of pop exps. */
	jp_t jbl_jp[cJL_BRANCHLMAXJPS];	/* JPs for populated exps. */
} jbl_t, *Pjbl_t;

/* A bitmap branch is a way of compressing empty expanses (null JPs) out of
 * uncompressed 256-way branch.  This costs 1 additional cache line fill, but
 * can save a lot of memory when it matters most, near the leaves, and
 * typically there will be only one at most in the path to any Index (leaf).
 * The bitmap indicates which of the cJL_BRANCHUNUMJPS (256) JPs in the branch
 * are NOT null, that is, their expanses are populated.  The jbb_t also
 * contains N pointers to "mini" Judy branches ("subexpanses") of up to M JPs
 * each (see BITMAP_BRANCHMxN, for example, BITMAP_BRANCH32x8), where M x N =
 * cJL_BRANCHUNUMJPS.  These are dynamically allocated and never contain
 * cJ*_JPNULL* jp_Types.  An empty subexpanse is represented by no bit sets in
 * the corresponding subexpanse bitmap, in which case the corresponding
 * jbbs_Pjp pointers value is unused.
 * Note that the number of valid JPs in each 1-of-N subexpanses is determined
 * by POPULATION rather than by EXPANSE -- the desired outcome to save memory
 * when near the leaves.  Note that the memory required for 185 JPs is about as
 * much as an uncompressed 256-way branch, therefore 184 is set as the maximum.
 * However, it is expected that a conversion to an uncompressed 256-way branch
 * will normally take place before this limit is reached for other reasons,
 * such as improving performance when the "wasted" memory is well amortized by
 * the population under the branch, preserving an acceptable overall
 * bytes/Index in the Judy array.
 * The number of pointers to arrays of JPs in the Judy bitmap branch:
 * Note:  The numbers below are the same in both 32 and 64 bit systems. */
#define cJL_BRANCHBMAXJPS  184	/* maximum JPs for bitmap branches. */

/* Convenience wrappers for referencing BranchB bitmaps or JP subarray
 * pointers:
 * Note: JL_JBB_PJP produces a "raw" memory address that must pass through
 * P_JP before use, except when freeing memory: */
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

/* Convenience wrapper for referencing BranchU JPs:
 * Note:  This produces a non-"raw" address already passed through P_JBU(). */
#define JL_JBU_PJP(Pjp,Index,Level) \
        (&((P_JBU((Pjp)->jp_Addr))->jbu_jp[JL_DIGITATSTATE(Index, Level)]))
#define JL_JBU_PJP0(Pjp) (&((P_JBU((Pjp)->jp_Addr))->jbu_jp[0]))

typedef struct JUDY_BRANCH_UNCOMPRESSED {
	jp_t jbu_jp[cJL_BRANCHUNUMJPS];	/* JPs for populated exp. */
} jbu_t, *Pjbu_t;

/* OBJECT SIZES IN WORDS:
 * Word_ts per various JudyL structures that have constant sizes.
 * cJL_WORDSPERJP should always be 2; this is fundamental to the Judy
 * structures. */
#define cJL_WORDSPERJP (sizeof(jp_t)   / cJL_BYTESPERPTR)
#define cJL_WORDSPERCL (cJL_BYTESPERCL / cJL_BYTESPERPTR)

/* OPPORTUNISTIC UNCOMPRESSION:
 * Define populations at which a BranchL or BranchB must convert to BranchU.
 * Earlier conversion is possible with good memory efficiency -- see below.
 * Max population below BranchL, then convert to BranchU: */
#define JL_BRANCHL_MAX_POP      1000

/* Minimum global population increment before next conversion of a BranchB to a
 * BranchU: This is was done to allow malloc() to coalesce memory before the
 * next big (~512 words) allocation. */
#define JL_BTOU_POP_INCREMENT    300

/* Min/max population below BranchB, then convert to BranchU: */
#define JL_BRANCHB_MIN_POP       135
#define JL_BRANCHB_MAX_POP       750

/* Get N most significant bits from the shifted Index word:
 * As Index words are decoded, they are shifted left so only relevant,
 * undecoded Index bits remain. */
#define JL_BITSFROMSFTIDX(SFTIDX, N)  ((SFTIDX) >> (cJL_BITSPERWORD - (N)))

#define cJL_MASKATSTATE(State)  (0xffL << (((State) - 1) * cJL_BITSPERBYTE))

/* Get byte (digit) from Index at the specified state, right justified:
 * Note:  State must be 1..cJL_ROOTSTATE, and Digits must be 1..(cJL_ROOTSTATE
 * - 1), but theres no way to assert these within an expression. */
#define JL_DIGITATSTATE(Index,cState) \
         ((uint8_t)((Index) >> (((cState) - 1) * cJL_BITSPERBYTE)))

/* Similarly, place byte (digit) at correct position for the specified state:
 * Note:  Cast digit to a Word_t first so there are no complaints or problems
 * about shifting it more than 32 bits on a 64-bit system, say, when it is a
 * uint8_t from jbl_Expanse[].  (Believe it or not, the C standard says to
 * promote an unsigned char to a signed int; -Ac does not do this, but -Ae
 * does.)
 * Also, to make lint happy, cast the whole result again because apparently
 * shifting a Word_t does not result in a Word_t ! */
#define JL_DIGITTOSTATE(Digit,cState) \
        ((Word_t) (((Word_t) (Digit)) << (((cState) - 1) * cJL_BITSPERBYTE)))

/* uint8_t */
typedef enum {
#define cJL_JPNULLMAX cJL_JPNULL3
	cJL_JPNULL1 = 1,	/* Index Size 1[1] byte  when 1 Index inserted. */
	cJL_JPNULL2,		/* Index Size 2[2] bytes when 1 Index inserted. */
	cJL_JPNULL3,		/* Index Size 3[3] bytes when 1 Index inserted. */
	cJL_JPBRANCH_L2,	/* 2[2] bytes Pop0, 1[5] bytes Dcd. */
	cJL_JPBRANCH_L3,	/* 3[3] bytes Pop0, 0[4] bytes Dcd. */
	cJL_JPBRANCH_L,		/* note:  DcdPopO field not used. */
	cJL_JPBRANCH_B2,	/* 2[2] bytes Pop0, 1[5] bytes Dcd. */
	cJL_JPBRANCH_B3,	/* 3[3] bytes Pop0, 0[4] bytes Dcd. */
	cJL_JPBRANCH_B,		/* note:  DcdPopO field not used. */
	cJL_JPBRANCH_U2,	/* 2[2] bytes Pop0, 1[5] bytes Dcd. */
	cJL_JPBRANCH_U3,	/* 3[3] bytes Pop0, 0[4] bytes Dcd. */
	cJL_JPBRANCH_U,		/* note:  DcdPopO field not used. */
	cJL_JPLEAF1,		/* 1[1] byte  Pop0, 2    bytes Dcd. */
	cJL_JPLEAF2,		/* 2[2] bytes Pop0, 1[5] bytes Dcd. */
	cJL_JPLEAF3,		/* 3[3] bytes Pop0, 0[4] bytes Dcd. */
	cJL_JPLEAF_B1,		/* 1[1] byte Pop0, 2[6] bytes Dcd. */
	cJL_JPIMMED_1_01,	/* Index Size = 1, Pop1 = 1. */
	cJL_JPIMMED_2_01,	/* Index Size = 2, Pop1 = 1. */
	cJL_JPIMMED_3_01,	/* Index Size = 3, Pop1 = 1. */
	cJL_JPIMMED_1_02,	/* Index Size = 1, Pop1 = 2. */
	cJL_JPIMMED_1_03,	/* Index Size = 1, Pop1 = 3. */
	cJL_JPIMMED_CAP
} jpL_Type_t;

/* RELATED VALUES: Index Size (state) for leaf JP, and JP type based on Index Size (state) */
#define JL_LEAFINDEXSIZE(jpType) ((jpType)    - cJL_JPLEAF1 + 1)
#define JL_LEAFTYPE(IndexSize)   ((IndexSize) + cJL_JPLEAF1 - 1)

/* MAXIMUM POPULATIONS OF LINEAR LEAVES: */
#define J_L_MAXB                (sizeof(uint32_t) * 64)
#define ALLOCSIZES { 3, 5, 7, 11, 15, 23, 32, 47, 64, TERMINATOR }	/* in words. */
#define cJL_LEAF1_MAXWORDS      (32)	/* max Leaf1 size in words. */

/* cJL_LEAF1_MAXPOP1 is chosen such that the index portion is less than
 * 32 bytes -- the number of bytes the index takes in a bitmap leaf. */
#define cJL_LEAF1_MAXPOP1 ((cJL_LEAF1_MAXWORDS * cJL_BYTESPERWORD)/(1 + cJL_BYTESPERWORD))
#define cJL_LEAF2_MAXPOP1 (J_L_MAXB / (2 + cJL_BYTESPERWORD))
#define cJL_LEAF3_MAXPOP1 (J_L_MAXB / (3 + cJL_BYTESPERWORD))
#define cJL_LEAFW_MAXPOP1 ((J_L_MAXB - cJL_BYTESPERWORD) / (2 * cJL_BYTESPERWORD))

/* MAXIMUM POPULATIONS OF IMMEDIATE JPs:
 * These specify the maximum Population of immediate JPs with various Index
 * Sizes (== sizes of remaining undecoded Index bits).  Since the JP Types enum
 * already lists all the immediates in order by state and size, calculate these
 * values from it to avoid redundancy. */
#define cJL_IMMED1_MAXPOP1  ((cJL_BYTESPERWORD - 1) / 1)	/* 3 [7]. */
#define cJL_IMMED2_MAXPOP1  ((cJL_BYTESPERWORD - 1) / 2)	/* 1 [3]. */
#define cJL_IMMED3_MAXPOP1  ((cJL_BYTESPERWORD - 1) / 3)	/* 1 [2]. */

/* Assemble bitmap leaves out of smaller units that put bitmap subexpanses
 * close to their associated pointers.  Why not just use a bitmap followed by a
 * series of pointers?  (See 4.27.)  Turns out this wastes a cache fill on
 * systems with smaller cache lines than the assumed value cJL_WORDSPERCL. */
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
	Word_t jpm_Pop0;	/* Must be first */
	jp_t jpm_JP;
	Word_t jpm_LastUPop0;
	Pjv_t jpm_PValue;
	Word_t jpm_TotalMemWords;
} jLpm_t, *Pjpm_t;

/* TABLES FOR DETERMINING IF LEAVES HAVE ROOM TO GROW:
 * These tables indicate if a given memory chunk can support growth of a given
 * object into wasted (rounded-up) memory in the chunk.  Note:  This violates
 * the hiddenness of the JudyMalloc code. */
extern const uint8_t jL_Leaf1PopToWords[cJL_LEAF1_MAXPOP1 + 1];
extern const uint8_t jL_Leaf2PopToWords[cJL_LEAF2_MAXPOP1 + 1];
extern const uint8_t jL_Leaf3PopToWords[cJL_LEAF3_MAXPOP1 + 1];
extern const uint8_t jL_LeafWPopToWords[cJL_LEAFW_MAXPOP1 + 1];
extern const uint8_t jL_LeafVPopToWords[];

/* These tables indicate where value areas start: */
extern const uint8_t jL_Leaf1Offset[cJL_LEAF1_MAXPOP1 + 1];
extern const uint8_t jL_Leaf2Offset[cJL_LEAF2_MAXPOP1 + 1];
extern const uint8_t jL_Leaf3Offset[cJL_LEAF3_MAXPOP1 + 1];
extern const uint8_t jL_LeafWOffset[cJL_LEAFW_MAXPOP1 + 1];

/* Also define macros to hide the details in the code using these tables. */
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
void   judyLFreeSM(Pjp_t, Pjpm_t);

#endif /* ! __JUDYL_INCLUDED__ */
