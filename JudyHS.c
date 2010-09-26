#include "Judy.h"

#define IS_PLS(PLS)     (((unsigned long) (PLS)) & JLAP_INVALID)
#define CLEAR_PLS(PLS)  (((unsigned long) (PLS)) & (~JLAP_INVALID))
#define SET_PLS(PLS)    (((unsigned long) (PLS)) | JLAP_INVALID)
#define WORDSIZE     (sizeof(uint32_t))

typedef struct L_EAFSTRING {
	void *ls_Value;			// Value area (cannot change size)
	uint8_t ls_String[WORDSIZE];	// to fill out to a uint32_t size
} ls_t, *Pls_t;

#define LS_STRUCTOVD     (sizeof(ls_t) - WORDSIZE)
#define LS_WORDLEN(LEN)  (((LEN) + LS_STRUCTOVD + WORDSIZE - 1) / WORDSIZE)
#define COPYSTRINGtoWORD(WORD,STR,LEN)	do {				\
	WORD = 0;							\
	switch (LEN) {							\
	default:    /* four and greater */				\
	case 4: WORD += (uint32_t )(((uint8_t *)(STR))[3] << 24);	\
	case 3: WORD += (uint32_t )(((uint8_t *)(STR))[2] << 16);	\
	case 2: WORD += (uint32_t )(((uint8_t *)(STR))[1] <<  8);	\
	case 1: WORD += (uint32_t )(((uint8_t *)(STR))[0]);		\
	case 0: break;							\
	}								\
} while (0)

#define JUDYHASHSTR(HVALUE,STRING,LENGTH) do {				\
	uint8_t *p_ = (uint8_t *)(STRING);				\
	uint8_t *q_ = p_ + (LENGTH);					\
	uint32_t c_ = 0;						\
	for (; p_ != q_; ++p_)						\
		c_ = (c_ * 31) + *p_;					\
	(HVALUE) = c_;							\
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

void **JudyHSGet(const void *PArray, void *Str, size_t Len)
{
	uint8_t *String = (uint8_t *) Str;
	void **PPValue;
	uint32_t Index;

	if ((PPValue = JudyLGet(PArray, Len)) == NULL)
		return NULL;

	if ((String == NULL) && (Len != 0))
		return NULL;	// avoid null-pointer dereference

	if (Len > WORDSIZE) {
		uint32_t HValue;	// hash of input string
		JUDYHASHSTR(HValue, String, Len);	// hash to no more than 32 bits
		PPValue = JudyLGet(*PPValue, (uint32_t )HValue); // get ^ to hash bucket
		if (PPValue == NULL)
			return NULL;	// no entry in Hash table
	}

	while (Len > WORDSIZE) {
		if (IS_PLS(*PPValue)) {
			Pls_t Pls = (Pls_t) CLEAR_PLS(*PPValue);	// remove flag from ^

			if (memcmp(String, Pls->ls_String, Len) == 0)
				return (void **)(&Pls->ls_Value);
			else
				return NULL;
		} else {
			COPYSTRINGtoWORD(Index, String, WORDSIZE);

			PPValue = JudyLGet(*PPValue, Index);
			if (PPValue == NULL)
				return NULL;

			String += WORDSIZE;
			Len -= WORDSIZE;
		}
	}

	COPYSTRINGtoWORD(Index, String, Len);
	PPValue = JudyLGet(*PPValue, Index); // decode last 1-4[8] bytes
	return PPValue;
}

static void **insStrJudyLTree(uint8_t *String, uint32_t Len, void **PPValue)
{
	uint32_t Index;		// next 4[8] bytes of String

	while (Len > WORDSIZE) {
		if (*PPValue == NULL) {
			Pls_t Pls;	// memory for a ls_t
			Pls = (Pls_t) JudyMalloc(LS_WORDLEN(Len));
			if (Pls == NULL) {
				JL_SET_ERRNO(JL_ERRNO_NOMEM);
				return PPJERR;
			}
			Pls->ls_Value = 0;	// clear Value word
			memcpy(Pls->ls_String, String, Len);	// copy to new struct
			*PPValue = (void *) SET_PLS(Pls);	// mark pointer
			return &Pls->ls_Value;	// return ^ to Value
		}

		if (IS_PLS(*PPValue)) {
			Pls_t Pls;	// ^ to ls_t
			uint8_t *String0;	// ^ to string in ls_t
			uint32_t Index0;	// 4[8] bytes in string
			uint32_t FreeLen;	// length of ls_t
			void **PPsplit;

			FreeLen = LS_WORDLEN(Len);	// length of ls_t

			Pls = (Pls_t) CLEAR_PLS(*PPValue);	// demangle ^ to ls_t
			String0 = Pls->ls_String;
			if (memcmp(String, String0, Len) == 0)
				return &Pls->ls_Value;	// yes, duplicate

			*PPValue = NULL;	// clear ^ to ls_t and make JudyL

			do {	// decode next 4[8] bytes of string
				COPYSTRINGtoWORD(Index0, String0, WORDSIZE);
				String0 += WORDSIZE;
				COPYSTRINGtoWORD(Index, String, WORDSIZE);
				String += WORDSIZE;
				Len -= WORDSIZE;
				PPsplit = PPValue;	// save for split below
				PPValue = JudyLIns(PPValue, Index0);
				if (PPValue == PPJERR)
					return PPJERR;
			} while ((Index0 == Index) && (Len > WORDSIZE));

			PPValue = insStrJudyLTree(String0, Len, PPValue);
			if (PPValue == PPJERR)
				return PPJERR;
			*PPValue = Pls->ls_Value;

			JudyFree((void *) Pls, FreeLen);
			PPValue = JudyLIns(PPsplit, Index);
			if (PPValue == PPJERR)
				return PPValue;

			PPValue = insStrJudyLTree(String, Len, PPValue);
			return PPValue;
		}

		COPYSTRINGtoWORD(Index, String, WORDSIZE);
		Len -= WORDSIZE;
		String += WORDSIZE;

		PPValue = JudyLIns(PPValue, Index);
		if (PPValue == PPJERR)
			return PPValue;
	}

	COPYSTRINGtoWORD(Index, String, Len);
	PPValue = JudyLIns(PPValue, Index);

	return PPValue;
}

void **JudyHSIns(void **PPArray, void *Str, size_t Len)
{
	uint8_t *String = (uint8_t *) Str;
	void **PPValue;

	if ((String == NULL) && (Len != 0UL)) {
		JL_SET_ERRNO(JL_ERRNO_NULLPINDEX);
		return PPJERR;
	}

	PPValue = JudyLGet(*PPArray, Len);
	if (PPValue == NULL) {
		PPValue = JudyLIns(PPArray, Len);
		if (PPValue == PPJERR)
			return PPJERR;
	}

	if (Len > WORDSIZE) {
		uint32_t HValue;
		JUDYHASHSTR(HValue, String, Len);
		PPValue = JudyLIns(PPValue, (uint32_t ) HValue);
		if (PPValue == PPJERR)
			return PPJERR;
	}

	PPValue = insStrJudyLTree(String, Len, PPValue);
	return PPValue;
}

static int delStrJudyLTree(uint8_t *String, uint32_t Len, void **PPValue)
{
	void **PPValueN;
	uint32_t Index;
	int Ret;

	if (IS_PLS(*PPValue)) {
		Pls_t Pls;
		Pls = (Pls_t) CLEAR_PLS(*PPValue);
		JudyFree((void *) Pls, LS_WORDLEN(Len));
		*PPValue = NULL;
		return 1;
	}

	if (Len > WORDSIZE) {
		COPYSTRINGtoWORD(Index, String, WORDSIZE);
		PPValueN = JudyLGet(*PPValue, Index);

		String += WORDSIZE;
		Len -= WORDSIZE;

		Ret = delStrJudyLTree(String, Len, PPValueN);
		if (Ret != 1)
			return Ret;

		if (*PPValueN == NULL)
			Ret = JudyLDel(PPValue, Index);
	} else {
		COPYSTRINGtoWORD(Index, String, Len);	// get leaf element
		Ret = JudyLDel(PPValue, Index);
	}
	return Ret;
}

int JudyHSDel(void **PPArray, void *Str, size_t Len)
{
	uint8_t *String = (uint8_t *) Str;
	void **PPBucket, **PPHtble;
	int Ret;		// return bool from Delete routine
	uint32_t HValue = 0;	// hash value of input string

	if (PPArray == NULL)
		return 0;

	if (JudyHSGet(*PPArray, String, Len) == NULL)
		return 0;

	PPHtble = JudyLGet(*PPArray, Len);
	if (Len > WORDSIZE) {
		JUDYHASHSTR(HValue, String, Len);
		PPBucket = JudyLGet(*PPHtble, (uint32_t ) HValue);
	} else {
		PPBucket = PPHtble;	// no bucket to JLGet
	}

	Ret = delStrJudyLTree(String, Len, PPBucket);
	if (Ret != 1)
		return -1;

	if (*PPBucket == NULL) {
		if (Len > WORDSIZE) {
			Ret = JudyLDel(PPHtble, (uint32_t ) HValue);
			if (Ret != 1)
				return -1;
		}
		if (*PPHtble == NULL) {
			Ret = JudyLDel(PPArray, Len);
			if (Ret != 1)
				return -1;
		}
	}
	return 1;
}

static void delJudyLTree(void **PPValue, uint32_t Len)
{
	void **PPValueN;

	if (Len > WORDSIZE) {
		uint32_t NEntry = 0;

		if (IS_PLS(*PPValue)) {
			uint32_t freewords = LS_WORDLEN(Len);	// calculate length
			Pls_t Pls = (Pls_t) CLEAR_PLS(*PPValue);// demangle pointer
			JudyFree((void *) Pls, freewords);	// free the ls_t
			return;
		}

		for (PPValueN = JudyLFirst(*PPValue, &NEntry);
		     (PPValueN != NULL) && (PPValueN != PPJERR);
		     PPValueN = JudyLNext(*PPValue, &NEntry)) {
			delJudyLTree(PPValueN, Len - WORDSIZE);
		}
		if (PPValueN == PPJERR)
			return;

		JudyLFreeArray(PPValue);
		return;
	}
	
	JudyLFreeArray(PPValue);
}

void JudyHSFreeArray(void **PPArray)
{
	uint32_t Len = 0;
	void **PPHtble;

	if (PPArray == NULL)
		return;

	for (PPHtble = JudyLFirst(*PPArray, &Len);
	     (PPHtble != NULL) && (PPHtble != PPJERR);
	     PPHtble = JudyLNext(*PPArray, &Len)) {
		if (Len > WORDSIZE) {
			uint32_t HEntry = 0;	// walk the hash tables
			void **PPValueH;

			for (PPValueH = JudyLFirst(*PPHtble, &HEntry);
			     (PPValueH != NULL) && (PPValueH != PPJERR);
			     PPValueH = JudyLNext(*PPHtble, &HEntry)) {
				delJudyLTree(PPValueH, Len);
			}

			if (PPValueH == PPJERR)
				return;

			JudyLFreeArray(PPHtble);
		} else {
			delJudyLTree(PPHtble, Len);
		}
	}
	if (PPHtble == PPJERR)
		return;
	JudyLFreeArray(PPArray);
}
