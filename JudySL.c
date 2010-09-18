#include "Judy.h"

#define JL_SET_ERRNO(PJERROR, JERRNO)           \
{                                               \
    if (PJERROR != (PJError_t)NULL) {           \
        JL_ERRNO(PJERROR) = (JERRNO);           \
        JL_ERRID(PJERROR) = __LINE__;           \
    }                                           \
}

#define JL_SET_ERRNO_NONNULL(PJERROR, JERRNO)   \
{                                               \
    JL_ERRNO(PJERROR) = (JERRNO);               \
    JL_ERRID(PJERROR) = __LINE__;               \
}

#define WORDSIZE (sizeof (Word_t))	// bytes in word = JudyL index.
#define WORDS(BYTES) (((BYTES) + WORDSIZE - 1) / WORDSIZE)	// round up.
#define IS_PSCL(PSCL)     (((Word_t) (PSCL)) & JLAP_INVALID)
#define CLEAR_PSCL(PSCL)  ((Pscl_t)(((Word_t) (PSCL)) & (~JLAP_INVALID)))
#define SET_PSCL(PSCL)    (((Word_t) (PSCL)) | JLAP_INVALID)
#define LASTWORD_BY_VALUE(WORD) (! ((WORD) & 0xffL))

#define COPYSTRINGtoWORD(WORD,STR) do {                 \
        uint8_t chr;                                    \
        WORD =  (STR)[0] << 24;				\
        if (WORD == 0) break;                           \
        if (!(chr = (STR)[1])) break;			\
        WORD += (Word_t)(chr << 16);                    \
        if (!(chr  = (STR)[2])) break;                  \
        WORD += (Word_t)(chr << 8) + (STR)[3];          \
} while(0)

#define COPYWORDtoSTRING(STR,WORD)      do {                    \
        if (!((STR)[0] = (uint8_t)((WORD) >> 24))) break;       \
        if (!((STR)[1] = (uint8_t)((WORD) >> 16))) break;       \
        if (!((STR)[2] = (uint8_t)((WORD) >>  8))) break;       \
        (STR)[3]       = (uint8_t)(WORD);                       \
} while(0)

typedef struct SHORCUTLEAF {
	Pvoid_t scl_Pvalue;	// callers value area.
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
    ((SCLCMP(INDEX, PSCL) == 0) ? &PSCLVALUE(PSCL) : (PPvoid_t)NULL)
#define PPSCLVALUE_LT(INDEX,PSCL)                                       \
    ((SCLCMP(INDEX, PSCL) < 0) ? &PSCLVALUE(PSCL) : (PPvoid_t)NULL)
#define PPSCLVALUE_GT(INDEX,PSCL)                                       \
    ((SCLCMP(INDEX, PSCL) > 0) ? &PSCLVALUE(PSCL) : (PPvoid_t)NULL)

#define APPEND_SCL(PSCL,PPARRAY,INDEX,LEN,PJERROR)                      \
{                                                                       \
    if (((PSCL) = (Pscl_t) JudyMalloc(SCLSIZE(LEN))) == (Pscl_t)NULL) { \
        JL_SET_ERRNO(PJERROR, JL_ERRNO_NOMEM);                          \
        return (PPJERR);                                                \
    }                                                                   \
    *(PPARRAY) = (Pvoid_t)SET_PSCL(PSCL);                               \
    ((PSCL)->scl_Pvalue) = (Pvoid_t)NULL;                               \
    (void)STRCPY((PSCL)->scl_Index, INDEX);                             \
}

static void JudySLModifyErrno(PJError_t, Pcvoid_t, Pcvoid_t);
static int JudySLDelSub(PPvoid_t, PPvoid_t, const uint8_t *, Word_t, PJError_t);
static PPvoid_t JudySLPrevSub(Pcvoid_t, uint8_t *, int, Word_t, PJError_t);
static PPvoid_t JudySLNextSub(Pcvoid_t, uint8_t *, int, Word_t, PJError_t);

static void JudySLModifyErrno(PJError_t PJError, Pcvoid_t PArray, Pcvoid_t PArrayOrig)
{
	if ((PJError != PJE0) && (JL_ERRNO(PJError) == JL_ERRNO_NOTJUDYL)) {
		if (PArray == PArrayOrig) {
			JL_SET_ERRNO_NONNULL(PJError, JL_ERRNO_NOTJUDYSL);
		} else {
			JL_SET_ERRNO_NONNULL(PJError, JL_ERRNO_CORRUPT);
		}
	}
}

PPvoid_t JudySLGet(Pcvoid_t PArray, const uint8_t * Index, PJError_t PJError)
{
	const uint8_t *pos = Index;	// place in Index.
	Word_t indexword;	// buffer for aligned copy.
	PPvoid_t PPValue;	// from JudyL array.

	if (Index == (uint8_t *) NULL) {
		JL_SET_ERRNO(PJError, JL_ERRNO_NULLPINDEX);
		return (PPJERR);
	}

	do {
		if (IS_PSCL(PArray))	// a shortcut leaf.
			return (PPSCLVALUE_EQ(pos, PArray));

		COPYSTRINGtoWORD(indexword, pos);	// copy next 4[8] bytes.
		JLG(PPValue, PArray, indexword);
		if ((PPValue == (PPvoid_t) NULL) || LASTWORD_BY_VALUE(indexword))
			return (PPValue);
		pos += WORDSIZE;
		PArray = *PPValue;	// each value -> next array.
	} while (1);
}

PPvoid_t JudySLIns(PPvoid_t PPArray, const uint8_t * Index, PJError_t PJError)
{
	PPvoid_t PPArrayOrig = PPArray;	// for error reporting.
	const uint8_t *pos = Index;	// place in Index.
	const uint8_t *pos2 = (uint8_t *) NULL;	// old Index (SCL being moved).
	Word_t len;		// bytes remaining.

	Word_t len2 = 0;	// for old Index (SCL being moved).
	Word_t scl2 = 0;	// size in words of SCL
	Word_t indexword;	// buffer for aligned copy.
	Word_t indexword2;	// for old Index (SCL being moved).
	PPvoid_t PPValue;	// from JudyL array.
	PPvoid_t PPValue2;	// for old Index (SCL being moved).
	Pscl_t Pscl = (Pscl_t) NULL;	// shortcut leaf.
	Pscl_t Pscl2;		// for old Index (SCL being moved).

	if (PPArray == (PPvoid_t) NULL) {
		JL_SET_ERRNO(PJError, JL_ERRNO_NULLPPARRAY);
		return (PPJERR);
	}
	if (Index == (uint8_t *) NULL) {
		JL_SET_ERRNO(PJError, JL_ERRNO_NULLPINDEX);
		return (PPJERR);
	}

	len = STRLEN(Index);	// bytes remaining.

	while (1) {
		if (*PPArray == (Pvoid_t) NULL)	{
			if (Pscl == (Pscl_t) NULL) {
				APPEND_SCL(Pscl, PPArray, pos, len, PJError);
				return (&(Pscl->scl_Pvalue));
			}
		} else if (IS_PSCL(*PPArray)) {
			assert(Pscl == (Pscl_t) NULL);	// no nested SCLs.
			Pscl = CLEAR_PSCL(*PPArray);
			pos2 = Pscl->scl_Index;	// note: pos2 is always word-aligned.
			len2 = STRLEN(pos2);	// bytes remaining.

			if ((len == len2) && (STRCMP(pos, pos2) == 0))
				return (&(Pscl->scl_Pvalue));

			*PPArray = (Pvoid_t) NULL;	// disconnect SCL.
			scl2 = SCLSIZE(len2);	// save for JudyFree
		}

		COPYSTRINGtoWORD(indexword, pos);	// copy next 4[8] bytes.
		if (Pscl != (Pscl_t) NULL) {
			COPYSTRINGtoWORD(indexword2, pos2);

			if (indexword != indexword2) {
				assert(*PPArray == (Pvoid_t) NULL);

				if ((PPValue2 = JudyLIns(PPArray, indexword2, PJError)) == PPJERR) {
					JudySLModifyErrno(PJError, *PPArray,
							  *PPArrayOrig);
					return (PPJERR);
				}

				assert(PPValue2 != (PPvoid_t) NULL);

				if (len2 <= WORDSIZE) {
					*((PWord_t) PPValue2) = (Word_t) (Pscl->scl_Pvalue);
				} else {
					APPEND_SCL(Pscl2, PPValue2, pos2 + WORDSIZE, len2 - WORDSIZE, PJError);
					(Pscl2->scl_Pvalue) = Pscl->scl_Pvalue;
				}
				JudyFree((void *)Pscl, scl2);
				Pscl = (Pscl_t) NULL;
			}
		}

		if ((PPValue = JudyLIns(PPArray, indexword, PJError)) == PPJERR) {
			JudySLModifyErrno(PJError, *PPArray, *PPArrayOrig);
			return (PPJERR);
		}

		assert(PPValue != (PPvoid_t) NULL);

		if (len <= WORDSIZE) {
			assert(Pscl == (Pscl_t) NULL);
			return (PPValue);	// is value for whole Index string.
		}

		pos += WORDSIZE;
		len -= WORDSIZE;
		pos2 += WORDSIZE;	// useless unless Pscl is set.
		len2 -= WORDSIZE;
		PPArray = PPValue;	// each value -> next array.
	}
}

int JudySLDel(PPvoid_t PPArray, const uint8_t * Index, PJError_t PJError)
{
	if (PPArray == (PPvoid_t) NULL) {
		JL_SET_ERRNO(PJError, JL_ERRNO_NULLPPARRAY);
		return (JERR);
	}

	if (Index == (uint8_t *) NULL) {
		JL_SET_ERRNO(PJError, JL_ERRNO_NULLPINDEX);
		return (JERR);
	}

	return (JudySLDelSub(PPArray, PPArray, Index, STRLEN(Index), PJError));
}

static int JudySLDelSub(PPvoid_t PPArray, PPvoid_t PPArrayOrig,
			const uint8_t *Index, Word_t len, PJError_t PJError)
{
	Word_t indexword;	// next word to find.
	PPvoid_t PPValue;	// from JudyL array.
	int retcode;		// from lower-level call.

	assert(PPArray != (PPvoid_t) NULL);
	assert(Index != (uint8_t *) NULL);

	if (IS_PSCL(*PPArray)) {
		Pscl_t Pscll = CLEAR_PSCL(*PPArray);
		Word_t words;

		if (STRCMP(Index, Pscll->scl_Index))
			return (0);	// incorrect index.

		words = SCLSIZE(STRLEN(Pscll->scl_Index));
		JudyFree((void *)Pscll, words);

		*PPArray = (Pvoid_t) NULL;
		return (1);	// correct index deleted.
	}

	COPYSTRINGtoWORD(indexword, Index);

	if (len <= WORDSIZE) {
		if ((retcode = JudyLDel(PPArray, indexword, PJError)) == JERR) {
			JudySLModifyErrno(PJError, *PPArray, *PPArrayOrig);
			return (JERR);
		}
		return (retcode);
	}

	JLG(PPValue, *PPArray, indexword);
	if (PPValue == (PPvoid_t) NULL)
		return 0;
	if ((retcode = JudySLDelSub(PPValue, PPArrayOrig, Index + WORDSIZE,
				    len - WORDSIZE, PJError)) != 1) {
		return retcode;
	}

	if (*PPValue == (Pvoid_t) NULL) {
		if ((retcode = JudyLDel(PPArray, indexword, PJError)) == JERR) {
			JudySLModifyErrno(PJError, *PPArray, *PPArrayOrig);
			return JERR;
		}

		return retcode;
	}

	return (1);
}

PPvoid_t JudySLPrev(Pcvoid_t PArray, uint8_t * Index, PJError_t PJError)
{

	if (Index == (uint8_t *) NULL) {
		JL_SET_ERRNO(PJError, JL_ERRNO_NULLPINDEX);
		return (PPJERR);
	}

	if (PArray == (Pvoid_t) NULL)
		return ((PPvoid_t) NULL);
	return (JudySLPrevSub(PArray, Index, 1, STRLEN(Index), PJError));
}

static PPvoid_t JudySLPrevSub(Pcvoid_t PArray, uint8_t * Index, int orig, Word_t len,
			      PJError_t PJError)
{
	Word_t indexword;	// next word to find.
	PPvoid_t PPValue;	// from JudyL array.

	if (orig) {
		if (IS_PSCL(PArray)) {
			if ((PPValue = PPSCLVALUE_GT(Index, PArray)) != (PPvoid_t) NULL)
				(void)STRCPY(Index, PSCLINDEX(PArray));
			return (PPValue);
		}

		COPYSTRINGtoWORD(indexword, Index);	// copy next 4[8] bytes.
		if (len > WORDSIZE) {	// not at end of Index.
			JLG(PPValue, PArray, indexword);
			if (PPValue != (PPvoid_t) NULL) {
				PPValue = JudySLPrevSub(*PPValue, Index + WORDSIZE, 1,
							len - WORDSIZE, PJError);
				if (PPValue == PPJERR)
					return (PPJERR);	// propagate error.
				if (PPValue != (PPvoid_t) NULL)
					return (PPValue);	// see above.
			}
		}

		if ((PPValue = JudyLPrev(PArray, &indexword, PJError)) == PPJERR) {
			JudySLModifyErrno(PJError, PArray,
					  orig ? PArray : (Pvoid_t) NULL);
			return (PPJERR);
		}

		if (PPValue == (PPvoid_t) NULL)
			return ((PPvoid_t) NULL);	// no previous index word.
	} else {
		if (IS_PSCL(PArray)) {
			(void)STRCPY(Index, PSCLINDEX(PArray));
			return (&PSCLVALUE(PArray));
		}

		indexword = ~0UL;
		if ((PPValue = JudyLLast(PArray, &indexword, PJError)) == PPJERR) {
			JudySLModifyErrno(PJError, PArray, orig ? PArray : (Pvoid_t) NULL);
			return (PPJERR);
		}

		if (PPValue == (PPvoid_t) NULL)
			return ((PPvoid_t) NULL);	// no previous index word.
	}

	COPYWORDtoSTRING(Index, indexword);	// copy next 4[8] bytes.
	if (LASTWORD_BY_VALUE(indexword))
		return (PPValue);
	return (JudySLPrevSub(*PPValue, Index + WORDSIZE, 0, len - WORDSIZE, PJError));
}

PPvoid_t JudySLNext(Pcvoid_t PArray, uint8_t * Index, PJError_t PJError)
{
	if (Index == (uint8_t *) NULL) {
		JL_SET_ERRNO(PJError, JL_ERRNO_NULLPINDEX);
		return (PPJERR);
	}

	if (PArray == (Pvoid_t) NULL)
		return ((PPvoid_t) NULL);
	return (JudySLNextSub(PArray, Index, 1, STRLEN(Index), PJError));
}

static PPvoid_t JudySLNextSub(Pcvoid_t PArray, uint8_t * Index, int orig,
			      Word_t len, PJError_t PJError)
{
	Word_t indexword;
	PPvoid_t PPValue;
	if (orig) {
		if (IS_PSCL(PArray)) {
			if ((PPValue = PPSCLVALUE_LT(Index, PArray)) != (PPvoid_t) NULL)
				(void)STRCPY(Index, PSCLINDEX(PArray));
			return (PPValue);
		}

		COPYSTRINGtoWORD(indexword, Index);	// copy next 4[8] bytes.

		if (len > WORDSIZE) {
			JLG(PPValue, PArray, indexword);
			if (PPValue != (PPvoid_t) NULL) {
				PPValue = JudySLNextSub(*PPValue, Index + WORDSIZE, 1,
							len - WORDSIZE, PJError);
				if (PPValue == PPJERR)
					return (PPJERR);	// propagate error.
				if (PPValue != (PPvoid_t) NULL)
					return (PPValue);	// see above.
			}
		}

		if ((PPValue = JudyLNext(PArray, &indexword, PJError)) == PPJERR) {
			JudySLModifyErrno(PJError, PArray, orig ? PArray : (Pvoid_t) NULL);
			return (PPJERR);
		}

		if (PPValue == (PPvoid_t) NULL)
			return ((PPvoid_t) NULL);	// no next index word.
	} else {
		if (IS_PSCL(PArray)) {
			(void)STRCPY(Index, PSCLINDEX(PArray));
			return (&PSCLVALUE(PArray));
		}

		indexword = 0;
		if ((PPValue = JudyLFirst(PArray, &indexword, PJError)) == PPJERR) {
			JudySLModifyErrno(PJError, PArray, orig ? PArray : (Pvoid_t) NULL);
			return (PPJERR);
		}
		if (PPValue == (PPvoid_t) NULL)
			return ((PPvoid_t) NULL);	// no next index word.
	}

	COPYWORDtoSTRING(Index, indexword);	// copy next 4[8] bytes
	if (LASTWORD_BY_VALUE(indexword))
		return (PPValue);
	return (JudySLNextSub(*PPValue, Index + WORDSIZE, 0, len - WORDSIZE, PJError));
}

PPvoid_t JudySLFirst(Pcvoid_t PArray, uint8_t * Index, PJError_t PJError)
{
	PPvoid_t PPValue;	// from JudyL array.
	if (Index == (uint8_t *) NULL) {
		JL_SET_ERRNO(PJError, JL_ERRNO_NULLPINDEX);
		return (PPJERR);
	}

	if ((PPValue = JudySLGet(PArray, Index, PJError)) == PPJERR)
		return (PPJERR);	// propagate serious error.
	if ((PPValue == (PPvoid_t) NULL)	// first try failed.
	    && ((PPValue = JudySLNext(PArray, Index, PJError)) == PPJERR)) {
		return (PPJERR);	// propagate serious error.
	}

	return (PPValue);
}

PPvoid_t JudySLLast(Pcvoid_t PArray, uint8_t * Index, PJError_t PJError)
{
	PPvoid_t PPValue;	// from JudyL array.
	if (Index == (uint8_t *) NULL) {
		JL_SET_ERRNO(PJError, JL_ERRNO_NULLPINDEX);
		return (PPJERR);
	}

	if ((PPValue = JudySLGet(PArray, Index, PJError)) == PPJERR)
		return (PPJERR);	// propagate serious error.
	if ((PPValue == (PPvoid_t) NULL)	// first try failed.
	    && ((PPValue = JudySLPrev(PArray, Index, PJError)) == PPJERR)) {
		return (PPJERR);	// propagate serious error.
	}

	return (PPValue);
}

Word_t JudySLFreeArray(PPvoid_t PPArray, PJError_t PJError)
{
	PPvoid_t PPArrayOrig = PPArray;	// for error reporting.
	Word_t indexword = 0;	// word just found.
	PPvoid_t PPValue;	// from Judy array.
	Word_t bytes_freed = 0;	// bytes freed at this level.
	Word_t bytes_total = 0;	// bytes freed at all levels.
	if (PPArray == (PPvoid_t) NULL) {
		JL_SET_ERRNO(PJError, JL_ERRNO_NULLPPARRAY);
		return (JERR);
	}

	if (IS_PSCL(*PPArray)) {
		Pscl_t Pscl = CLEAR_PSCL(*PPArray);
		Word_t freewords = SCLSIZE(STRLEN(Pscl->scl_Index));
		JudyFree((void *)Pscl, freewords);
		*PPArray = (Pvoid_t) NULL;
		return (freewords * WORDSIZE);
	}

	for (PPValue = JudyLFirst(*PPArray, &indexword, PJError);
	     (PPValue != (PPvoid_t) NULL) && (PPValue != PPJERR);
	     PPValue = JudyLNext(*PPArray, &indexword, PJError)) {
		if (!LASTWORD_BY_VALUE(indexword)) {
			if ((bytes_freed = JudySLFreeArray(PPValue, PJError)) == JERR)
				return (JERR);	// propagate serious error.
			bytes_total += bytes_freed;
		}
	}

	if (PPValue == PPJERR) {
		JudySLModifyErrno(PJError, *PPArray, *PPArrayOrig);
		return (JERR);
	}

	if ((bytes_freed = JudyLFreeArray(PPArray, PJError)) == JERR) {
		JudySLModifyErrno(PJError, *PPArray, *PPArrayOrig);
		return (JERR);
	}
	return (bytes_total + bytes_freed);
}
