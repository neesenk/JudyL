#include "Judy.h"

#define WORDSIZE     (sizeof(uint32_t))

typedef struct L_EAFSTRING {
	void *ls_Value;			/* Value area (cannot change size) */
	struct L_EAFSTRING *next;	/* to Next Leaf */
	uint8_t ls_String[0];		/* to fill out to a uint32_t size */
} ls_t, *Pls_t;

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

#define __rot(x, k) (((x) << (k)) | ((x) >> (32 - (k))))
/* __jhash_mix - mix 3 32-bit values reversibly. */
#define __jhash_mix(a,b,c) \
{ \
	a -= c;  a ^= __rot(c, 4);  c += b; \
	b -= a;  b ^= __rot(a, 6);  a += c; \
	c -= b;  c ^= __rot(b, 8);  b += a; \
	a -= c;  a ^= __rot(c,16);  c += b; \
	b -= a;  b ^= __rot(a,19);  a += c; \
	c -= b;  c ^= __rot(b, 4);  b += a; \
}

/* __jhash_final - final mixing of 3 32-bit values (a,b,c) into c */
#define __jhash_final(a,b,c) \
{ \
	c ^= b; c -= __rot(b,14); \
	a ^= c; a -= __rot(c,11); \
	b ^= a; b -= __rot(a,25); \
	c ^= b; c -= __rot(b,16); \
	a ^= c; a -= __rot(c, 4); \
	b ^= a; b -= __rot(a,14); \
	c ^= b; c -= __rot(b,24); \
}

/* The golden ration: an arbitrary value */
#define JHASH_GOLDEN_RATIO	0xdeadbeef

/* The most generic version, hashes an arbitrary sequence
 * of bytes.  No alignment or len assumptions are made about
 * the input key. The result depends on endianness. */
typedef uint32_t u32;
typedef uint8_t u8;
static inline uint32_t jhash(const void *key, uint32_t len, uint32_t seed)
{
	u32 a,b,c;
	const u8 *k = key;

	/* Set up the internal state */
	a = b = c = JHASH_GOLDEN_RATIO + len + seed;
	/* all but the last block: affect some 32 bits of (a,b,c) */
	for (; len > 12; len -= 12, k += 12) {
    		a += (k[0] + ((u32)k[1]<<8) + ((u32)k[2]<<16) + ((u32)k[3]<<24));
		b += (k[4] + ((u32)k[5]<<8) + ((u32)k[6]<<16) + ((u32)k[7]<<24));
		c += (k[8] + ((u32)k[9]<<8) + ((u32)k[10]<<16) + ((u32)k[11]<<24));
		__jhash_mix(a, b, c);
	}

	/* last block: affect all 32 bits of (c) */
	/* all the case statements fall through */
	switch (len) {
	case 12: c += (u32)k[11]<<24;
	case 11: c += (u32)k[10]<<16;
	case 10: c += (u32)k[9]<<8;
	case 9 : c += k[8];
	case 8 : b += (u32)k[7]<<24;
	case 7 : b += (u32)k[6]<<16;
	case 6 : b += (u32)k[5]<<8;
	case 5 : b += k[4];
	case 4 : a += (u32)k[3]<<24;
	case 3 : a += (u32)k[2]<<16;
	case 2 : a += (u32)k[1]<<8;
	case 1 : a += k[0]; __jhash_final(a, b, c);
	}

	return c;
}

void **JudyHtbGet(const void *PArray, void *Str, size_t Len)
{
	uint8_t *String = (uint8_t *) Str;
	void **PPValue;
	uint32_t Index;

	if (String == NULL && Len != 0)
		return NULL;

	if ((PPValue = JudyLGet(PArray, Len)) == NULL)
		return NULL;

	if (Len > WORDSIZE) {
		Pls_t list = NULL;
		Index = jhash(String, Len, 0);
		if ((PPValue = JudyLGet(*PPValue, Index)) == NULL)
			return NULL;
		for (list = *PPValue; list; list = list->next) {
			if (memcmp(String, list->ls_String, Len) == 0)
				return &list->ls_Value;
		}
	} else {
		COPYSTRINGtoWORD(Index, String, Len);
		return JudyLGet(*PPValue, Index);
	}

	return NULL;
}

void **JudyHtbIns(void **PPArray, void *Str, size_t Len)
{
	uint8_t *String = (uint8_t *) Str;
	void **PPValue;
	uint32_t Index;
	Pls_t list = NULL;

	if (String == NULL && Len != 0) {
		JL_SET_ERRNO(JLE_NULLPINDEX);
		return PPJERR;
	}

	if ((PPValue = JudyLGet(*PPArray, Len)) == NULL) {
		if ((PPValue = JudyLIns(PPArray, Len)) == PPJERR)
			return PPJERR;
	}

	if (Len <= WORDSIZE) {
		COPYSTRINGtoWORD(Index, String, Len);
		return JudyLIns(PPValue, Index);
	}

	Index = jhash(String, Len, 0);
	if ((PPValue = JudyLIns(PPValue, Index)) == PPJERR)
		return PPJERR;

	for (list = *PPValue; list; list = list->next) {
		if (memcmp(String, list->ls_String, Len) == 0)
			return &list->ls_Value;
	}

	if ((list = malloc(Len + sizeof(*list))) == NULL)
		return PPJERR;

	list->ls_Value = NULL;
	list->next = *PPValue;
	memcpy(list->ls_String, String, Len);
	*PPValue = list;

	return &list->ls_Value;
}

int JudyHtbDel(void **PPArray, void *Str, size_t Len, void **PPValue)
{
	uint8_t *String = (uint8_t *) Str;
	void **PPBucket, **PPHtble;
	uint32_t HValue = 0;
	Pls_t *list = NULL, item;

	if (PPArray == NULL)
		return 0;

	if ((PPHtble = JudyLGet(*PPArray, Len)) == NULL)
		return 0;

	if (Len <= WORDSIZE) {
		int r = 0;
		COPYSTRINGtoWORD(HValue, String, Len);
		r = JudyLDel(PPHtble, HValue, PPValue);
		if (*PPHtble == NULL)
			JudyLDel(PPArray, Len, NULL);
		return r;
	}

	HValue = jhash(String, Len, 0);
	if ((PPBucket = JudyLGet(*PPHtble, HValue)) == NULL)
		return 0;
	for (list = (Pls_t *)PPBucket; (item = *list) != NULL; list = &item->next) {
		if (memcmp(item->ls_String, String, Len) == 0) {
			*list = item->next;
			if (PPValue)
				*PPValue = item->ls_Value;
			free(item);
			if (*PPBucket == NULL)
				JudyLDel(PPHtble, HValue, NULL);
			if (*PPHtble == NULL)
				JudyLDel(PPArray, Len, NULL);
			return 1;
		}
	}

	return 0;
}

struct HtbWalkCTX {
	walk_fn_t fn;
	void *ctx;
	int Len;
};

static int JudyHtbWalkFn(void *ctx, uint32_t Index, void **Value)
{
	Pls_t list = NULL;
	struct HtbWalkCTX *c = ctx;
	int ret = 0;
	for (list = *Value; list; list = list->next) {
		ret = c->fn(c->ctx, Index, &list->ls_Value);
		if (ret != 0)
			return ret;
	}

	return 0;
}

int JudyHtbWalk(void *PArray, walk_fn_t fn, void *ctx)
{
	uint32_t Len = 0;
	int ret = 0;
	void **PPHtble;

	if (PArray == NULL)
		return 0;

	for (PPHtble = JudyLFirst(PArray, &Len);
	     (PPHtble != NULL) && (PPHtble != PPJERR);
	     PPHtble = JudyLNext(PArray, &Len)) {
		if (Len > WORDSIZE) {
			struct HtbWalkCTX hctx = { fn, ctx, Len };
			ret = JudyLWalk(*PPHtble, JudyHtbWalkFn, &hctx);
		} else {
			ret = JudyLWalk(*PPHtble, fn, ctx);
		}

		if (ret != 0)
			return ret;
	}

	return 0;
}

size_t JudyHtbMemUsed(void *PArray)
{
	uint32_t Len = 0;
	size_t ret = 0;
	void **PPHtble;

	if (PArray == NULL)
		return 0;

	for (PPHtble = JudyLFirst(PArray, &Len);
	     (PPHtble != NULL) && (PPHtble != PPJERR);
	     PPHtble = JudyLNext(PArray, &Len)) {
		ret += JudyLMemUsed(*PPHtble);
		/* ret += JudyLCount(*PPHtble, 0, ~0) * (sizeof(ls_t) + Len); */
	}

	ret += JudyLMemUsed(PArray);

	return ret;
}

void JudyHtbFreeArray(void **PPArray)
{
	uint32_t Len = 0;
	void **PPHtble;

	if (PPArray == NULL)
		return;

	for (PPHtble = JudyLFirst(*PPArray, &Len);
	     (PPHtble != NULL) && (PPHtble != PPJERR);
	     PPHtble = JudyLNext(*PPArray, &Len)) {
		if (Len > WORDSIZE) {
			uint32_t HEntry = 0;
			void **PPValueH;

			for (PPValueH = JudyLFirst(*PPHtble, &HEntry);
			     (PPValueH != NULL) && (PPValueH != PPJERR);
			     PPValueH = JudyLNext(*PPHtble, &HEntry)) {
				Pls_t list = NULL;
				while ((list = *PPValueH) != NULL) {
					*PPValueH = list->next;
					free(list);
				}
			}
		}

		JudyLFreeArray(PPHtble);
	}

	JudyLFreeArray(PPArray);
}
