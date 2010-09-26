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
        JL_ERRNO_NONE           = 0,
        JL_ERRNO_FULL           = 1,
        JL_ERRNO_NFMAX          = JL_ERRNO_FULL,
        JL_ERRNO_NOMEM          = 2,	// cannot obtain needed memory
        JL_ERRNO_NULLPPARRAY    = 3,    // PPARRAY is NULL
        JL_ERRNO_NONNULLPARRAY  = 10,   // PARRAY is not NULL
        JL_ERRNO_NULLPINDEX     = 4,    // PIndex is NULL
        JL_ERRNO_NULLPVALUE     = 11,   // PValue is NULL
        JL_ERRNO_NOTJUDYL       = 6,    // PArray is not to a JudyL array.
        JL_ERRNO_NOTJUDYSL      = 7,    // PArray is not to a JudySL array.
        JL_ERRNO_UNSORTED       = 12,   // see above.
        JL_ERRNO_OVERRUN        = 8,	// Judy detects a not recoverable point
        JL_ERRNO_CORRUPT        = 9	// Judy detects an impossible value
} JL_Errno_t;

#define JL_SET_ERRNO(JErrno) do {			\
	assert((JErrno) != JL_ERRNO_OVERRUN);		\
	assert((JErrno) != JL_ERRNO_CORRUPT);		\
	errno = (JErrno);				\
} while (0)

#define JERR		(-1)			/* functions returning int or Word_t */
#define PJERR		((void *)(~0UL))	/* mainly for use here, see below    */
#define PPJERR		((void **)(~0UL))	/* functions that return void ***/
#define JLAP_INVALID    0x1			/* flag to mark pointer "not a Judy array" */

extern void **	JudyLGet(const void *PArray, uint32_t Index);
extern void **	JudyLIns(void **PPArray, uint32_t Index);
extern int	JudyLInsArray(void **PPArray, size_t, const uint32_t *, const void **);
extern int	JudyLDel(void **PPArray, uint32_t Index);
extern uint32_t	JudyLCount(const void *PArray, uint32_t Index1, uint32_t Index2);
extern void **	JudyLByCount(const void *PArray, uint32_t Count, uint32_t *PIndex);
extern size_t	JudyLFreeArray(void **PPArray);
extern size_t	JudyLMemUsed(const void *PArray);
extern size_t	JudyLMemActive(const void *PArray);
extern void **	JudyLFirst(const void *PArray, uint32_t *PIndex);
extern void **	JudyLNext(const void *PArray, uint32_t *PIndex);
extern void **	JudyLLast(const void *PArray, uint32_t *PIndex);
extern void **	JudyLPrev(const void *PArray, uint32_t *PIndex);

/* if return not 0, then walk stop */
typedef int (*walk_fn_t)(void *ctx, uint32_t Index, void **Value);
extern int JudyLWalk(void *PArray, walk_fn_t fn, void *ctx);

extern void **	JudySLGet(const void *, const uint8_t *Index);
extern void **	JudySLIns(void **, const uint8_t *Index);
extern int      JudySLDel(void **, const uint8_t *Index);
extern size_t	JudySLFreeArray(void **);
extern void **	JudySLFirst(const void *, uint8_t *Index);
extern void **	JudySLNext(const void *, uint8_t *Index);
extern void **	JudySLLast(const void *, uint8_t *Index);
extern void **	JudySLPrev(const void *, uint8_t *Index);

extern void **	JudyHSGet(const void *PArray, void *Index, size_t len);
extern void **	JudyHSIns(void **PPArray, void *Index, size_t len);
extern int      JudyHSDel(void **PPArray, void *Index, size_t len);
extern size_t	JudyHSFreeArray(void **PPArray);
extern uint32_t JudyHashStr(void *, size_t);

#ifdef __cplusplus
}
#endif
#endif /* ! _JUDY_INCLUDED */
