#ifndef _JUDY_INCLUDED
#define _JUDY_INCLUDED
#include <stdint.h>
#include <stdlib.h>
#include <limits.h>
#include <string.h>
#include <assert.h>
#include <errno.h>

#ifdef __cplusplus      /* support use by C++ code */
extern "C" {
#endif

/* Judy error numbers */
typedef enum {
        JLE_NONE           = 0,
        JLE_FULL           = 1,
        JLE_NFMAX          = JLE_FULL,
        JLE_NOMEM          = 2,		/* cannot obtain needed memory */
        JLE_NULLPPARRAY    = 3,		/* PPARRAY is NULL */
        JLE_NONNULLPARRAY  = 10,	/* PARRAY is not NULL */
        JLE_NULLPINDEX     = 4,		/* PIndex is NULL */
        JLE_NULLPVALUE     = 11,	/* PValue is NULL */
        JLE_NOTJUDYL       = 6,		/* PArray is not to a JudyL array. */
        JLE_NOTJUDYSL      = 7,		/* PArray is not to a JudySL array. */
        JLE_UNSORTED       = 12,	/* Index or Value is not sorted */
        JLE_OVERRUN        = 8,		/* Judy detects a not recoverable point */
        JLE_CORRUPT        = 9		/* Judy detects an impossible value */
} JL_Errno_t;

#define JL_SET_ERRNO(JErrno)	do {		\
	assert((JErrno) != JLE_OVERRUN);	\
	assert((JErrno) != JLE_CORRUPT);	\
	errno = (JErrno);			\
} while (0)

#define JERR		(-1)			/* functions returning int or Word_t */
#define PJERR		((void *)(~0UL))	/* mainly for use here, see below    */
#define PPJERR		((void **)(~0UL))	/* functions that return void ***/
#define JLAP_INVALID    0x1			/* flag to mark pointer "not a Judy array" */

extern void **	JudyLGet(const void *PArray, uint32_t Index);
extern void **	JudyLIns(void **PPArray, uint32_t Index);
extern int	JudyLInsArray(void **PPArray, size_t, const uint32_t *, void * const *);
extern int	JudyLDel(void **PPArray, uint32_t Index, void **Value);
extern size_t	JudyLCount(const void *PArray, uint32_t Idx_beg, uint32_t Idx_end);
extern void **	JudyLByCount(const void *PArray, uint32_t Count, uint32_t *PIndex);
extern void	JudyLFreeArray(void **PPArray);
extern size_t	JudyLMemUsed(const void *PArray);
extern void **	JudyLFirst(const void *PArray, uint32_t *PIndex);
extern void **	JudyLNext(const void *PArray, uint32_t *PIndex);
extern void **	JudyLLast(const void *PArray, uint32_t *PIndex);
extern void **	JudyLPrev(const void *PArray, uint32_t *PIndex);
extern int      JudyLFirstEmpty(const void *PArray, uint32_t *PIndex);
extern int      JudyLNextEmpty(const void *PArray, uint32_t *PIndex);
extern int      JudyLLastEmpty(const void *PArray, uint32_t *PIndex);
extern int      JudyLPrevEmpty(const void *PArray, uint32_t *PIndex);


/* if return not 0, then walk stop */
typedef int	(*walk_fn_t)(void *ctx, uint32_t Index, void **Value);
extern int	JudyLWalk(void *PArray, walk_fn_t fn, void *ctx);
extern int	JudyLWalkRang(void *PArray, uint32_t Begin,
			      uint32_t End, walk_fn_t fn, void *ctx);

extern void **	JudySLGet(const void *, const uint8_t *Index);
extern void **	JudySLIns(void **, const uint8_t *Index);
extern int      JudySLDel(void **, const uint8_t *Index, void **Value);
extern void	JudySLFreeArray(void **);
extern void **	JudySLFirst(const void *, uint8_t *Index);
extern void **	JudySLNext(const void *, uint8_t *Index);
extern void **	JudySLLast(const void *, uint8_t *Index);
extern void **	JudySLPrev(const void *, uint8_t *Index);
typedef int (*WalkFN)(void *ctx, const uint8_t *key, size_t len, void *value);
extern size_t JudySLPrefixGet(const void *PArray, uint8_t *Prefix, WalkFN fn, void *ctx);
extern size_t JudySLSub(const void *PArray, const uint8_t *Str, WalkFN fn, void *ctx);

extern void **	JudyHSGet(const void *PArray, void *Index, size_t len);
extern void **	JudyHSIns(void **PPArray, void *Index, size_t len);
extern int      JudyHSDel(void **PPArray, void *Index, size_t len, void **Value);
extern void	JudyHSFreeArray(void **PPArray);

extern void **	JudyHtbGet(const void *PArray, void *Str, size_t Len);
extern void **	JudyHtbIns(void **PPArray, void *Str, size_t Len);
extern int	JudyHtbDel(void **PPArray, void *Str, size_t Len, void **PPValue);
extern void	JudyHtbFreeArray(void **PPArray);

#ifdef __cplusplus
}
#endif
#endif /* ! _JUDY_INCLUDED */
