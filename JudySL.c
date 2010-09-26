#include "Judy.h"

#define WORDSIZE		(sizeof (uint32_t))	// bytes in word = JudyL index.
#define WORDS(BYTES)		(((BYTES) + WORDSIZE - 1) / WORDSIZE)	// round up.
#define IS_PSCL(PSCL)		(((unsigned long) (PSCL)) & JLAP_INVALID)
#define CLEAR_PSCL(PSCL)	((Pscl_t)(((unsigned long) (PSCL)) & (~JLAP_INVALID)))
#define SET_PSCL(PSCL)		(((unsigned long) (PSCL)) | JLAP_INVALID)
#define LASTWORD_BY_VALUE(WORD) (!((WORD) & 0xffL))

#define COPYSTRINGtoWORD(WORD,STR) do {                 \
        uint8_t chr;                                    \
        WORD =  (STR)[0] << 24;				\
        if (WORD == 0) break;                           \
        if (!(chr = (STR)[1])) break;			\
        WORD += (uint32_t )(chr << 16);                    \
        if (!(chr  = (STR)[2])) break;                  \
        WORD += (uint32_t )(chr << 8) + (STR)[3];          \
} while(0)

#define COPYWORDtoSTRING(STR,WORD)      do {                    \
        if (!((STR)[0] = (uint8_t)((WORD) >> 24))) break;       \
        if (!((STR)[1] = (uint8_t)((WORD) >> 16))) break;       \
        if (!((STR)[2] = (uint8_t)((WORD) >>  8))) break;       \
        (STR)[3]       = (uint8_t)(WORD);                       \
} while(0)

typedef struct SHORCUTLEAF {
	void *scl_Pvalue;		// callers value area.
	uint8_t scl_Index[WORDSIZE];	// base Index string.
} scl_t, *Pscl_t;

#define STRUCTOVD       (sizeof(scl_t) - WORDSIZE)
#define SCLSIZE(LEN)  (((LEN) + STRUCTOVD + WORDSIZE - 1) / WORDSIZE)
#define STRCMP(S1,S2)   strcmp((void *)(S1), (void *)(S2))
#define STRCPY(S1,S2)   strcpy((void *)(S1), (void *)(S2))
#define STRLEN(S1)      (strlen((void *)(S1)) + 1)
#define PSCLINDEX(PSCL)  ((CLEAR_PSCL(PSCL))->scl_Index)
#define PSCLVALUE(PSCL)  ((CLEAR_PSCL(PSCL))->scl_Pvalue)
#define SCLCMP(INDEX,PSCL) STRCMP(INDEX, PSCLINDEX(PSCL))
#define PPSCLVALUE_EQ(INDEX,PSCL)                                       \
    ((SCLCMP(INDEX, PSCL) == 0) ? &PSCLVALUE(PSCL) : NULL)
#define PPSCLVALUE_LT(INDEX,PSCL)                                       \
    ((SCLCMP(INDEX, PSCL) < 0) ? &PSCLVALUE(PSCL) : NULL)
#define PPSCLVALUE_GT(INDEX,PSCL)                                       \
    ((SCLCMP(INDEX, PSCL) > 0) ? &PSCLVALUE(PSCL) : NULL)

#define APPEND_SCL(PSCL,PPARRAY,INDEX,LEN)				\
{                                                                       \
    if (((PSCL) = (Pscl_t) JudyMalloc(SCLSIZE(LEN))) == NULL) { \
        JL_SET_ERRNO(JL_ERRNO_NOMEM);					\
        return PPJERR;                                                \
    }                                                                   \
    *(PPARRAY) = (void *)SET_PSCL(PSCL);                               \
    ((PSCL)->scl_Pvalue) = NULL;                               \
    (void)STRCPY((PSCL)->scl_Index, INDEX);                             \
}

static void *JudyMalloc(size_t words)
{
	return calloc(words, WORDSIZE);
}

static void JudyFree(void *PWord, size_t Words)
{
	(void)Words;
	free(PWord);
}

static int JudySLDelSub(void **, void **, const uint8_t *, size_t);
static void **JudySLPrevSub(const void *, uint8_t *, int, size_t);
static void **JudySLNextSub(const void *, uint8_t *, int, size_t);

static void JudySLModifyErrno(const void *PArray, const void *PArrayOrig)
{
	JL_SET_ERRNO(PArray == PArrayOrig ? JL_ERRNO_NOTJUDYSL : JL_ERRNO_CORRUPT);
}

void **JudySLGet(const void *PArray, const uint8_t *Index)
{
	const uint8_t *pos = Index;
	uint32_t indexword;
	void **PPValue;

	if (Index == NULL) {
		JL_SET_ERRNO(JL_ERRNO_NULLPINDEX);
		return PPJERR;
	}

	do {
		if (IS_PSCL(PArray))
			return (PPSCLVALUE_EQ(pos, PArray));

		COPYSTRINGtoWORD(indexword, pos);
		PPValue = JudyLGet(PArray, indexword);
		if ((PPValue == NULL) || LASTWORD_BY_VALUE(indexword))
			return PPValue;
		pos += WORDSIZE;
		PArray = *PPValue;
	} while (1);
}

void **JudySLIns(void **PPArray, const uint8_t *Index)
{
	void **PPArrayOrig = PPArray;	// for error reporting.
	const uint8_t *pos = Index;	// place in Index.
	const uint8_t *pos2 = NULL;	// old Index (SCL being moved).
	uint32_t len;		// bytes remaining.

	uint32_t len2 = 0;	// for old Index (SCL being moved).
	uint32_t scl2 = 0;	// size in words of SCL
	uint32_t indexword;	// buffer for aligned copy.
	uint32_t indexword2;	// for old Index (SCL being moved).
	void **PPValue;	// from JudyL array.
	void **PPValue2;	// for old Index (SCL being moved).
	Pscl_t Pscl = NULL;	// shortcut leaf.
	Pscl_t Pscl2;		// for old Index (SCL being moved).

	if (PPArray == NULL) {
		JL_SET_ERRNO(JL_ERRNO_NULLPPARRAY);
		return PPJERR;
	}
	if (Index == NULL) {
		JL_SET_ERRNO(JL_ERRNO_NULLPINDEX);
		return PPJERR;
	}

	len = STRLEN(Index);

	while (1) {
		if (*PPArray == NULL)	{
			if (Pscl == NULL) {
				APPEND_SCL(Pscl, PPArray, pos, len);
				return (&(Pscl->scl_Pvalue));
			}
		} else if (IS_PSCL(*PPArray)) {
			assert(Pscl == NULL);	// no nested SCLs.
			Pscl = CLEAR_PSCL(*PPArray);
			pos2 = Pscl->scl_Index;	// note: pos2 is always word-aligned.
			len2 = STRLEN(pos2);	// bytes remaining.

			if ((len == len2) && (STRCMP(pos, pos2) == 0))
				return (&(Pscl->scl_Pvalue));

			*PPArray = NULL;	// disconnect SCL.
			scl2 = SCLSIZE(len2);	// save for JudyFree
		}

		COPYSTRINGtoWORD(indexword, pos);	// copy next 4[8] bytes.
		if (Pscl != NULL) {
			COPYSTRINGtoWORD(indexword2, pos2);

			if (indexword != indexword2) {
				assert(*PPArray == NULL);

				if ((PPValue2 = JudyLIns(PPArray, indexword2)) == PPJERR) {
					JudySLModifyErrno(*PPArray, *PPArrayOrig);
					return PPJERR;
				}

				assert(PPValue2 != NULL);

				if (len2 <= WORDSIZE) {
					*PPValue2 = Pscl->scl_Pvalue;
				} else {
					APPEND_SCL(Pscl2, PPValue2, pos2 + WORDSIZE, len2 - WORDSIZE);
					(Pscl2->scl_Pvalue) = Pscl->scl_Pvalue;
				}
				JudyFree((void *)Pscl, scl2);
				Pscl = NULL;
			}
		}

		if ((PPValue = JudyLIns(PPArray, indexword)) == PPJERR) {
			JudySLModifyErrno(*PPArray, *PPArrayOrig);
			return PPJERR;
		}

		assert(PPValue != NULL);

		if (len <= WORDSIZE) {
			assert(Pscl == NULL);
			return PPValue;	// is value for whole Index string.
		}

		pos += WORDSIZE, len -= WORDSIZE;
		pos2 += WORDSIZE, len2 -= WORDSIZE;
		PPArray = PPValue;	// each value -> next array.
	}
}

int JudySLDel(void **PPArray, const uint8_t * Index)
{
	if (PPArray == NULL) {
		JL_SET_ERRNO(JL_ERRNO_NULLPPARRAY);
		return JERR;
	}

	if (Index == NULL) {
		JL_SET_ERRNO(JL_ERRNO_NULLPINDEX);
		return JERR;
	}

	return (JudySLDelSub(PPArray, PPArray, Index, STRLEN(Index)));
}

static int JudySLDelSub(void **PPArray, void **PPArrayOrig,
			const uint8_t *Index, size_t len)
{
	uint32_t indexword;	// next word to find.
	void **PPValue;		// from JudyL array.
	int retcode;		// from lower-level call.

	assert(PPArray != NULL);
	assert(Index != NULL);

	if (IS_PSCL(*PPArray)) {
		Pscl_t Pscll = CLEAR_PSCL(*PPArray);
		uint32_t words;

		if (STRCMP(Index, Pscll->scl_Index))
			return 0;

		words = SCLSIZE(STRLEN(Pscll->scl_Index));
		JudyFree((void *)Pscll, words);

		*PPArray = NULL;
		return 1;
	}

	COPYSTRINGtoWORD(indexword, Index);

	if (len <= WORDSIZE) {
		if ((retcode = JudyLDel(PPArray, indexword)) == JERR) {
			JudySLModifyErrno(*PPArray, *PPArrayOrig);
			return JERR;
		}
		return retcode;
	}

	PPValue = JudyLGet(*PPArray, indexword); 
	if (PPValue == NULL)
		return 0;
	if ((retcode = JudySLDelSub(PPValue, PPArrayOrig, Index + WORDSIZE,
				    len - WORDSIZE)) != 1) {
		return retcode;
	}

	if (*PPValue == NULL) {
		if ((retcode = JudyLDel(PPArray, indexword)) == JERR) {
			JudySLModifyErrno(*PPArray, *PPArrayOrig);
			return JERR;
		}

		return retcode;
	}

	return 1;
}

void **JudySLPrev(const void *PArray, uint8_t * Index)
{

	if (Index == NULL) {
		JL_SET_ERRNO(JL_ERRNO_NULLPINDEX);
		return PPJERR;
	}

	if (PArray == NULL)
		return NULL;
	return JudySLPrevSub(PArray, Index, 1, STRLEN(Index));
}

static void **JudySLPrevSub(const void *PArray, uint8_t * Index, int orig, size_t len)
{
	uint32_t indexword;	// next word to find.
	void **PPValue;		// from JudyL array.

	if (orig) {
		if (IS_PSCL(PArray)) {
			if ((PPValue = PPSCLVALUE_GT(Index, PArray)) != NULL)
				(void)STRCPY(Index, PSCLINDEX(PArray));
			return PPValue;
		}

		COPYSTRINGtoWORD(indexword, Index);
		if (len > WORDSIZE) {
			PPValue = JudyLGet(PArray, indexword);
			if (PPValue != NULL) {
				PPValue = JudySLPrevSub(*PPValue, Index + WORDSIZE, 1,
							len - WORDSIZE);
				if (PPValue == PPJERR)
					return PPJERR;
				if (PPValue != NULL)
					return PPValue;
			}
		}

		if ((PPValue = JudyLPrev(PArray, &indexword)) == PPJERR) {
			JudySLModifyErrno(PArray, orig ? PArray : NULL);
			return PPJERR;
		}

		if (PPValue == NULL)
			return NULL;
	} else {
		if (IS_PSCL(PArray)) {
			(void)STRCPY(Index, PSCLINDEX(PArray));
			return (&PSCLVALUE(PArray));
		}

		indexword = (uint32_t)~0UL;
		if ((PPValue = JudyLLast(PArray, &indexword)) == PPJERR) {
			JudySLModifyErrno(PArray, orig ? PArray : NULL);
			return PPJERR;
		}

		if (PPValue == NULL)
			return NULL;
	}

	COPYWORDtoSTRING(Index, indexword);
	if (LASTWORD_BY_VALUE(indexword))
		return PPValue;
	return (JudySLPrevSub(*PPValue, Index + WORDSIZE, 0, len - WORDSIZE));
}

void **JudySLNext(const void *PArray, uint8_t * Index)
{
	if (Index == NULL) {
		JL_SET_ERRNO(JL_ERRNO_NULLPINDEX);
		return PPJERR;
	}

	if (PArray == NULL)
		return NULL;
	return (JudySLNextSub(PArray, Index, 1, STRLEN(Index)));
}

static void **JudySLNextSub(const void *PArray, uint8_t *Index, int orig, size_t len)
{
	uint32_t indexword;
	void **PPValue;
	if (orig) {
		if (IS_PSCL(PArray)) {
			if ((PPValue = PPSCLVALUE_LT(Index, PArray)) != NULL)
				(void)STRCPY(Index, PSCLINDEX(PArray));
			return PPValue;
		}

		COPYSTRINGtoWORD(indexword, Index);

		if (len > WORDSIZE) {
			PPValue = JudyLGet(PArray, indexword);
			if (PPValue != NULL) {
				PPValue = JudySLNextSub(*PPValue, Index + WORDSIZE, 1,
							len - WORDSIZE);
				if (PPValue == PPJERR)
					return PPJERR;
				if (PPValue != NULL)
					return PPValue;	
			}
		}

		if ((PPValue = JudyLNext(PArray, &indexword)) == PPJERR) {
			JudySLModifyErrno(PArray, orig ? PArray : NULL);
			return PPJERR;
		}

		if (PPValue == NULL)
			return NULL;
	} else {
		if (IS_PSCL(PArray)) {
			(void)STRCPY(Index, PSCLINDEX(PArray));
			return (&PSCLVALUE(PArray));
		}

		indexword = 0;
		if ((PPValue = JudyLFirst(PArray, &indexword)) == PPJERR) {
			JudySLModifyErrno(PArray, orig ? PArray : NULL);
			return PPJERR;
		}
		if (PPValue == NULL)
			return NULL;
	}

	COPYWORDtoSTRING(Index, indexword);
	if (LASTWORD_BY_VALUE(indexword))
		return PPValue;

	return JudySLNextSub(*PPValue, Index + WORDSIZE, 0, len - WORDSIZE);
}

void **JudySLFirst(const void *PArray, uint8_t *Index)
{
	void **PPValue;
	if (Index == NULL) {
		JL_SET_ERRNO(JL_ERRNO_NULLPINDEX);
		return PPJERR;
	}

	if ((PPValue = JudySLGet(PArray, Index)) == PPJERR)
		return PPJERR;
	if (PPValue == NULL && (PPValue = JudySLNext(PArray, Index)) == PPJERR)
		return PPJERR;

	return PPValue;
}

void **JudySLLast(const void *PArray, uint8_t * Index)
{
	void **PPValue;
	if (Index == NULL) {
		JL_SET_ERRNO(JL_ERRNO_NULLPINDEX);
		return PPJERR;
	}

	if ((PPValue = JudySLGet(PArray, Index)) == PPJERR)
		return PPJERR;
	if ((PPValue == NULL)
	    && ((PPValue = JudySLPrev(PArray, Index)) == PPJERR)) {
		return PPJERR;
	}

	return PPValue;
}

size_t JudySLFreeArray(void **PPArray)
{
	void **PPArrayOrig = PPArray;	// for error reporting.
	uint32_t indexword = 0;		// word just found.
	void **PPValue;			// from Judy array.
	uint32_t bytes_freed = 0;	// bytes freed at this level.
	uint32_t bytes_total = 0;	// bytes freed at all levels.
	if (PPArray == NULL) {
		JL_SET_ERRNO(JL_ERRNO_NULLPPARRAY);
		return JERR;
	}

	if (IS_PSCL(*PPArray)) {
		Pscl_t Pscl = CLEAR_PSCL(*PPArray);
		uint32_t freewords = SCLSIZE(STRLEN(Pscl->scl_Index));
		JudyFree((void *)Pscl, freewords);
		*PPArray = NULL;
		return freewords * WORDSIZE;
	}

	for (PPValue = JudyLFirst(*PPArray, &indexword);
	     (PPValue != NULL) && (PPValue != PPJERR);
	     PPValue = JudyLNext(*PPArray, &indexword)) {
		if (!LASTWORD_BY_VALUE(indexword)) {
			if ((bytes_freed = JudySLFreeArray(PPValue)) == JERR)
				return JERR;	// propagate serious error.
			bytes_total += bytes_freed;
		}
	}

	if (PPValue == PPJERR) {
		JudySLModifyErrno(*PPArray, *PPArrayOrig);
		return JERR;
	}

	if ((bytes_freed = JudyLFreeArray(PPArray)) == JERR) {
		JudySLModifyErrno(*PPArray, *PPArrayOrig);
		return JERR;
	}

	return bytes_total + bytes_freed;
}
