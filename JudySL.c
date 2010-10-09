#include "Judy.h"

#define WORDSIZE		(sizeof (uint32_t))
#define WORDS(BYTES)		(((BYTES) + WORDSIZE - 1) / WORDSIZE)
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
	void *scl_Pvalue;		/* callers value area. */
	uint8_t scl_Index[WORDSIZE];	/* base Index string. */
} scl_t, *Pscl_t;

#define STRUCTOVD       (sizeof(scl_t) - WORDSIZE)
#define SCLSIZE(LEN)  (((LEN) + STRUCTOVD + WORDSIZE - 1) / WORDSIZE)
#define STRCMP(S1,S2)   strcmp((void *)(S1), (void *)(S2))
#define STRCPY(S1,S2)   strcpy((void *)(S1), (void *)(S2))
#define STRLEN(S1)      (strlen((void *)(S1)) + 1)
#define PSCLINDEX(PSCL)  ((CLEAR_PSCL(PSCL))->scl_Index)
#define PSCLVALUE(PSCL)  ((CLEAR_PSCL(PSCL))->scl_Pvalue)
#define SCLCMP(INDEX,PSCL) STRCMP(INDEX, PSCLINDEX(PSCL))
#define PPSCLVALUE_EQ(INDEX,PSCL) ((SCLCMP(INDEX, PSCL) == 0) ? &PSCLVALUE(PSCL) : NULL)
#define PPSCLVALUE_LT(INDEX,PSCL) ((SCLCMP(INDEX, PSCL) < 0) ? &PSCLVALUE(PSCL) : NULL)
#define PPSCLVALUE_GT(INDEX,PSCL) ((SCLCMP(INDEX, PSCL) > 0) ? &PSCLVALUE(PSCL) : NULL)

#define APPEND_SCL(PSCL,PPARRAY,INDEX,LEN) do {				\
    if (((PSCL) = (Pscl_t) JudyMalloc(SCLSIZE(LEN))) == NULL) {		\
        JL_SET_ERRNO(JLE_NOMEM);					\
        return PPJERR;							\
    }                                                                   \
    *(PPARRAY) = (void *)SET_PSCL(PSCL);				\
    ((PSCL)->scl_Pvalue) = NULL;					\
    (void)STRCPY((PSCL)->scl_Index, INDEX);                             \
} while (0)

static void *JudyMalloc(size_t words)
{
	return calloc(words, WORDSIZE);
}

static void JudyFree(void *PWord, size_t Words)
{
	(void)Words;
	free(PWord);
}

static void JudySLModifyErrno(const void *PArray, const void *PArrayOrig)
{
	JL_SET_ERRNO(PArray == PArrayOrig ? JLE_NOTJUDYSL : JLE_CORRUPT);
}

void **JudySLGet(const void *PArray, const uint8_t *Index)
{
	const uint8_t *pos = Index;
	uint32_t indexword;
	void **PPValue;

	if (Index == NULL) {
		JL_SET_ERRNO(JLE_NULLPINDEX);
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
	void **PPArrayOrig = PPArray;
	const uint8_t *pos = Index, *pos2 = NULL;
	uint32_t len = 0, len2 = 0, scl2 = 0, indexword = 0, indexword2 = 0;
	void **PPValue, **PPValue2;
	Pscl_t Pscl = NULL, Pscl2 = NULL;

	if (PPArray == NULL || Index == NULL) {
		JL_SET_ERRNO(JLE_NULLPPARRAY);
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
			assert(Pscl == NULL);
			Pscl = CLEAR_PSCL(*PPArray);
			pos2 = Pscl->scl_Index;
			len2 = STRLEN(pos2);

			if ((len == len2) && (STRCMP(pos, pos2) == 0))
				return (&(Pscl->scl_Pvalue));

			*PPArray = NULL;
			scl2 = SCLSIZE(len2);
		}

		COPYSTRINGtoWORD(indexword, pos);
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
			return PPValue;
		}

		pos += WORDSIZE, len -= WORDSIZE;
		pos2 += WORDSIZE, len2 -= WORDSIZE;
		PPArray = PPValue;
	}
}

static int JudySLDelSub(void **PPArray, void **PPArrayOrig,
			const uint8_t *Index, size_t len, void **Value)
{
	uint32_t indexword;
	void **PPValue;
	int retcode;

	assert(PPArray != NULL);
	assert(Index != NULL);

	if (IS_PSCL(*PPArray)) {
		Pscl_t Pscll = CLEAR_PSCL(*PPArray);
		uint32_t words;

		if (STRCMP(Index, Pscll->scl_Index))
			return 0;

		if (Value)
			*Value = Pscll->scl_Pvalue;
		words = SCLSIZE(STRLEN(Pscll->scl_Index));
		JudyFree((void *)Pscll, words);

		*PPArray = NULL;
		return 1;
	}

	COPYSTRINGtoWORD(indexword, Index);

	if (len <= WORDSIZE) {
		if ((retcode = JudyLDel(PPArray, indexword, Value)) == JERR) {
			JudySLModifyErrno(*PPArray, *PPArrayOrig);
			return JERR;
		}
		return retcode;
	}

	PPValue = JudyLGet(*PPArray, indexword);
	if (PPValue == NULL)
		return 0;
	if ((retcode = JudySLDelSub(PPValue, PPArrayOrig, Index + WORDSIZE,
				    len - WORDSIZE, Value)) != 1) {
		return retcode;
	}

	if (*PPValue == NULL) {
		void *PPdel = NULL;
		if ((retcode = JudyLDel(PPArray, indexword, &PPdel)) == JERR) {
			JudySLModifyErrno(*PPArray, *PPArrayOrig);
			return JERR;
		}
		assert(*PPValue == PPdel);

		return retcode;
	}

	return 1;
}

int JudySLDel(void **PPArray, const uint8_t *Index, void **PPvalue)
{
	if (PPArray == NULL || Index == NULL) {
		JL_SET_ERRNO(JLE_NULLPPARRAY);
		return JERR;
	}

	return JudySLDelSub(PPArray, PPArray, Index, STRLEN(Index), PPvalue);
}

static void **JudySLPrevSub(const void *PArray, uint8_t * Index, int orig, size_t len)
{
	uint32_t indexword;
	void **PPValue;

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

void **JudySLPrev(const void *PArray, uint8_t * Index)
{

	if (Index == NULL) {
		JL_SET_ERRNO(JLE_NULLPINDEX);
		return PPJERR;
	}

	if (PArray == NULL)
		return NULL;
	return JudySLPrevSub(PArray, Index, 1, STRLEN(Index));
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

void **JudySLNext(const void *PArray, uint8_t *Index)
{
	if (Index == NULL) {
		JL_SET_ERRNO(JLE_NULLPINDEX);
		return PPJERR;
	}

	if (PArray == NULL)
		return NULL;
	return JudySLNextSub(PArray, Index, 1, STRLEN(Index));
}

void **JudySLFirst(const void *PArray, uint8_t *Index)
{
	void **PPValue;
	if (Index == NULL) {
		JL_SET_ERRNO(JLE_NULLPINDEX);
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
		JL_SET_ERRNO(JLE_NULLPINDEX);
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

void JudySLFreeArray(void **PPArray)
{
	void **PPArrayOrig = PPArray;
	uint32_t indexword = 0;
	void **PPValue;

	if (PPArray == NULL)
		return;

	if (IS_PSCL(*PPArray)) {
		Pscl_t Pscl = CLEAR_PSCL(*PPArray);
		uint32_t freewords = SCLSIZE(STRLEN(Pscl->scl_Index));
		JudyFree((void *)Pscl, freewords);
		*PPArray = NULL;
		return;
	}

	for (PPValue = JudyLFirst(*PPArray, &indexword);
	     (PPValue != NULL) && (PPValue != PPJERR);
	     PPValue = JudyLNext(*PPArray, &indexword)) {
		if (!LASTWORD_BY_VALUE(indexword))
			JudySLFreeArray(PPValue);
	}

	if (PPValue == PPJERR) {
		JudySLModifyErrno(*PPArray, *PPArrayOrig);
		return;
	}

	JudyLFreeArray(PPArray);
}
