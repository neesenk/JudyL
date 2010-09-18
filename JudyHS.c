#include "Judy.h"

#define IS_PLS(PLS)     (((Word_t) (PLS)) & JLAP_INVALID)
#define CLEAR_PLS(PLS)  (((Word_t) (PLS)) & (~JLAP_INVALID))
#define SET_PLS(PLS)    (((Word_t) (PLS)) | JLAP_INVALID)
#define WORDSIZE     (sizeof(Word_t))

typedef struct L_EAFSTRING {
	Word_t ls_Value;	// Value area (cannot change size)
	uint8_t ls_String[WORDSIZE];	// to fill out to a Word_t size
} ls_t, *Pls_t;

#define LS_STRUCTOVD     (sizeof(ls_t) - WORDSIZE)
#define LS_WORDLEN(LEN)  (((LEN) + LS_STRUCTOVD + WORDSIZE - 1) / WORDSIZE)
#define  COPYSTRING4toWORD(WORD,STR,LEN)			\
{								\
    WORD = 0;							\
    switch (LEN) {						\
    default:    /* four and greater */				\
    case 4: WORD += (Word_t)(((uint8_t *)(STR))[3] << 24);	\
    case 3: WORD += (Word_t)(((uint8_t *)(STR))[2] << 16);	\
    case 2: WORD += (Word_t)(((uint8_t *)(STR))[1] <<  8);	\
    case 1: WORD += (Word_t)(((uint8_t *)(STR))[0]);		\
    case 0: break;						\
    }								\
}
#define COPYSTRINGtoWORD COPYSTRING4toWORD

#define JL_SET_ERRNO(PJERROR, JERRNO)           \
{                                               \
    if (PJERROR != (PJError_t) NULL) {          \
        if (JERRNO)                             \
            JL_ERRNO(PJError) = (JERRNO);       \
        JL_ERRID(PJERROR) = __LINE__;           \
    }                                           \
}

#define JUDYHASHSTR(HVALUE,STRING,LENGTH)       \
{                                               \
    uint8_t *p_ = (uint8_t *)(STRING);          \
    uint8_t *q_ = p_ + (LENGTH);                \
    uint32_t c_ = 0;                            \
    for (; p_ != q_; ++p_) {                    \
        c_ = (c_ * 31) + *p_;                   \
    }                                           \
    (HVALUE) = c_;                              \
}

PPvoid_t JudyHSGet(Pcvoid_t PArray, void *Str, Word_t Len)
{
	uint8_t *String = (uint8_t *) Str;
	PPvoid_t PPValue;
	Word_t Index;

	JLG(PPValue, PArray, Len);	// find hash table for strings of Len
	if (PPValue == (PPvoid_t) NULL)
		return ((PPvoid_t) NULL);	// no strings of this Len

	if ((String == (void *)NULL) && (Len != 0))
		return ((PPvoid_t) NULL);	// avoid null-pointer dereference

	if (Len > WORDSIZE) {
		uint32_t HValue;	// hash of input string
		JUDYHASHSTR(HValue, String, Len);	// hash to no more than 32 bits
		JLG(PPValue, *PPValue, (Word_t)HValue);	// get ^ to hash bucket
		if (PPValue == (PPvoid_t) NULL)
			return ((PPvoid_t) NULL);	// no entry in Hash table
	}

	while (Len > WORDSIZE) {
		if (IS_PLS(*PPValue)) {
			Pls_t Pls;
			Pls = (Pls_t) CLEAR_PLS(*PPValue);	// remove flag from ^

			if (memcmp(String, Pls->ls_String, Len) == 0)
				return ((PPvoid_t) (&(Pls->ls_Value)));
			else
				return ((PPvoid_t) NULL);
		} else {
			COPYSTRINGtoWORD(Index, String, WORDSIZE);

			JLG(PPValue, *PPValue, Index);
			if (PPValue == (PPvoid_t) NULL)
				return ((PPvoid_t) NULL);

			String += WORDSIZE;
			Len -= WORDSIZE;
		}
	}

	COPYSTRINGtoWORD(Index, String, Len);
	JLG(PPValue, *PPValue, Index);	// decode last 1-4[8] bytes
	return (PPValue);
}

static PPvoid_t insStrJudyLTree(uint8_t * String, Word_t Len, PPvoid_t PPValue, PJError_t PJError)
{
	Word_t Index;		// next 4[8] bytes of String

	while (Len > WORDSIZE) {
		if (*PPValue == (Pvoid_t) NULL) {
			Pls_t Pls;	// memory for a ls_t
			Pls = (Pls_t) JudyMalloc(LS_WORDLEN(Len));
			if (Pls == NULL) {
				JL_SET_ERRNO(PJError, JL_ERRNO_NOMEM);
				return (PPJERR);
			}
			Pls->ls_Value = 0;	// clear Value word
			memcpy(Pls->ls_String, String, Len);	// copy to new struct
			*PPValue = (Pvoid_t) SET_PLS(Pls);	// mark pointer
			return ((PPvoid_t) (&Pls->ls_Value));	// return ^ to Value
		}

		if (IS_PLS(*PPValue)) {
			Pls_t Pls;	// ^ to ls_t
			uint8_t *String0;	// ^ to string in ls_t
			Word_t Index0;	// 4[8] bytes in string
			Word_t FreeLen;	// length of ls_t
			PPvoid_t PPsplit;

			FreeLen = LS_WORDLEN(Len);	// length of ls_t

			Pls = (Pls_t) CLEAR_PLS(*PPValue);	// demangle ^ to ls_t
			String0 = Pls->ls_String;
			if (memcmp(String, String0, Len) == 0)
				return ((PPvoid_t) (&Pls->ls_Value));	// yes, duplicate

			*PPValue = NULL;	// clear ^ to ls_t and make JudyL

			do {	// decode next 4[8] bytes of string
				COPYSTRINGtoWORD(Index0, String0, WORDSIZE);
				String0 += WORDSIZE;
				COPYSTRINGtoWORD(Index, String, WORDSIZE);
				String += WORDSIZE;
				Len -= WORDSIZE;
				PPsplit = PPValue;	// save for split below
				PPValue = JudyLIns(PPValue, Index0, PJError);
				if (PPValue == PPJERR) {
					JL_SET_ERRNO(PJError, 0);
					return (PPJERR);
				}
			} while ((Index0 == Index) && (Len > WORDSIZE));

			PPValue = insStrJudyLTree(String0, Len, PPValue, PJError);
			if (PPValue == PPJERR)
				return (PPJERR);
			*(PWord_t) PPValue = Pls->ls_Value;

			JudyFree((Pvoid_t) Pls, FreeLen);
			PPValue = JudyLIns(PPsplit, Index, PJError);
			if (PPValue == PPJERR) {
				JL_SET_ERRNO(PJError, 0);
				return (PPValue);
			}

			PPValue = insStrJudyLTree(String, Len, PPValue, PJError);
			return (PPValue);
		}

		COPYSTRINGtoWORD(Index, String, WORDSIZE);
		Len -= WORDSIZE;
		String += WORDSIZE;

		PPValue = JudyLIns(PPValue, Index, PJError);
		if (PPValue == PPJERR) {
			JL_SET_ERRNO(PJError, 0);
			return (PPValue);
		}
	}

	COPYSTRINGtoWORD(Index, String, Len);
	PPValue = JudyLIns(PPValue, Index, PJError);

	return (PPValue);
}

PPvoid_t JudyHSIns(PPvoid_t PPArray, void *Str, Word_t Len, PJError_t PJError)
{
	uint8_t *String = (uint8_t *) Str;
	PPvoid_t PPValue;

	if ((String == (uint8_t *) NULL) && (Len != 0UL)) {
		JL_SET_ERRNO(PJError, JL_ERRNO_NULLPINDEX);
		return (PPJERR);
	}

	JLG(PPValue, *PPArray, Len);
	if (PPValue == (PPvoid_t) NULL) {
		PPValue = JudyLIns(PPArray, Len, PJError);
		if (PPValue == PPJERR) {
			JL_SET_ERRNO(PJError, 0);
			return (PPJERR);
		}
	}

	if (Len > WORDSIZE) {
		uint32_t HValue;
		JUDYHASHSTR(HValue, String, Len);
		PPValue = JudyLIns(PPValue, (Word_t) HValue, PJError);
		if (PPValue == PPJERR) {
			JL_SET_ERRNO(PJError, 0);
			return (PPJERR);
		}
	}

	PPValue = insStrJudyLTree(String, Len, PPValue, PJError);
	return PPValue;
}

static int delStrJudyLTree(uint8_t *String, Word_t Len, PPvoid_t PPValue, PJError_t PJError)
{
	PPvoid_t PPValueN;
	Word_t Index;
	int Ret;

	if (IS_PLS(*PPValue)) {
		Pls_t Pls;
		Pls = (Pls_t) CLEAR_PLS(*PPValue);
		JudyFree((Pvoid_t) Pls, LS_WORDLEN(Len));
		*PPValue = (Pvoid_t) NULL;
		return (1);
	}

	if (Len > WORDSIZE) {
		COPYSTRINGtoWORD(Index, String, WORDSIZE);
		JLG(PPValueN, *PPValue, Index);

		String += WORDSIZE;
		Len -= WORDSIZE;

		Ret = delStrJudyLTree(String, Len, PPValueN, PJError);
		if (Ret != 1)
			return (Ret);

		if (*PPValueN == (PPvoid_t) NULL)
			Ret = JudyLDel(PPValue, Index, PJError);
	} else {
		COPYSTRINGtoWORD(Index, String, Len);	// get leaf element
		Ret = JudyLDel(PPValue, Index, PJError);
	}
	return (Ret);
}

int JudyHSDel(PPvoid_t PPArray,	void *Str, Word_t Len, PJError_t PJError)
{
	uint8_t *String = (uint8_t *) Str;
	PPvoid_t PPBucket, PPHtble;
	int Ret;		// return bool from Delete routine
	uint32_t HValue = 0;	// hash value of input string

	if (PPArray == NULL)
		return (0);

	if (JudyHSGet(*PPArray, String, Len) == (PPvoid_t) NULL)
		return (0);

	JLG(PPHtble, *PPArray, Len);
	if (Len > WORDSIZE) {
		JUDYHASHSTR(HValue, String, Len);
		JLG(PPBucket, *PPHtble, (Word_t) HValue);
	} else {
		PPBucket = PPHtble;	// no bucket to JLGet
	}

	Ret = delStrJudyLTree(String, Len, PPBucket, PJError);
	if (Ret != 1) {
		JL_SET_ERRNO(PJError, 0);
		return (-1);
	}

	if (*PPBucket == (Pvoid_t) NULL) {
		if (Len > WORDSIZE) {
			Ret = JudyLDel(PPHtble, (Word_t) HValue, PJError);
			if (Ret != 1) {
				JL_SET_ERRNO(PJError, 0);
				return (-1);
			}
		}
		if (*PPHtble == (PPvoid_t) NULL) {
			Ret = JudyLDel(PPArray, Len, PJError);
			if (Ret != 1) {
				JL_SET_ERRNO(PJError, 0);
				return (-1);
			}
		}
	}
	return (1);
}

static Word_t delJudyLTree(PPvoid_t PPValue, Word_t Len, PJError_t PJError)
{
	Word_t bytes_freed = 0;	// bytes freed at point
	Word_t bytes_total = 0;	// accumulated bytes freed
	PPvoid_t PPValueN;

	if (Len > WORDSIZE) {
		Word_t NEntry;

		if (IS_PLS(*PPValue)) {
			Pls_t Pls;
			Word_t freewords;

			freewords = LS_WORDLEN(Len);	// calculate length
			Pls = (Pls_t) CLEAR_PLS(*PPValue);	// demangle pointer

			JudyFree((Pvoid_t) Pls, freewords);	// free the ls_t

			return (freewords * WORDSIZE);
		}

		NEntry = 0;	// start at beginning
		for (PPValueN = JudyLFirst(*PPValue, &NEntry, PJError);
		     (PPValueN != (PPvoid_t) NULL) && (PPValueN != PPJERR);
		     PPValueN = JudyLNext(*PPValue, &NEntry, PJError)) {

			bytes_freed = delJudyLTree(PPValueN, Len - WORDSIZE, PJError);
			if (bytes_freed == JERR)
				return (JERR);
			bytes_total += bytes_freed;
		}
		if (PPValueN == PPJERR)
			return (JERR);

		bytes_freed = JudyLFreeArray(PPValue, PJError);
		if (bytes_freed == JERR)
			return (JERR);
		bytes_total += bytes_freed;

		return bytes_total;
	}
	bytes_freed = JudyLFreeArray(PPValue, PJError);

	return (bytes_freed);
}

Word_t JudyHSFreeArray(PPvoid_t PPArray, PJError_t PJError)
{
	Word_t Len;		// start at beginning
	Word_t bytes_freed;	// bytes freed at this level.
	Word_t bytes_total;	// bytes total at all levels.
	PPvoid_t PPHtble;

	if (PPArray == NULL)
		return (0);	// no pointer, return none

	bytes_freed = 0;
	bytes_total = 0;
	Len = 0;

	for (PPHtble = JudyLFirst(*PPArray, &Len, PJError);
	     (PPHtble != (PPvoid_t) NULL) && (PPHtble != PPJERR);
	     PPHtble = JudyLNext(*PPArray, &Len, PJError)) 
	{
		PPvoid_t PPValueH;
		if (Len > WORDSIZE) {
			Word_t HEntry = 0;	// walk the hash tables

			for (PPValueH = JudyLFirst(*PPHtble, &HEntry, PJError);
			     (PPValueH != (PPvoid_t) NULL) && (PPValueH != PPJERR);
			     PPValueH = JudyLNext(*PPHtble, &HEntry, PJError)) 
			{
				bytes_freed = delJudyLTree(PPValueH, Len, PJError);
				if (bytes_freed == JERR)
					return (JERR);
				bytes_total += bytes_freed;
			}

			if (PPValueH == PPJERR)
				return (JERR);

			bytes_freed = JudyLFreeArray(PPHtble, PJError);
			if (bytes_freed == JERR)
				return (JERR);
			bytes_total += bytes_freed;
		} else {
			PPValueH = PPHtble;	// simulate hash table

			bytes_freed = delJudyLTree(PPValueH, Len, PJError);
			if (bytes_freed == JERR)
				return (JERR);
			bytes_total += bytes_freed;
		}
	}
	if (PPHtble == PPJERR)
		return (JERR);

	bytes_freed = JudyLFreeArray(PPArray, PJError);
	if (bytes_freed == JERR)
		return (JERR);
	bytes_total += bytes_freed;

	return bytes_total;
}
