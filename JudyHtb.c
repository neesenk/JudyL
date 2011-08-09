#include "Judy.h"
#define WORDSIZE     (sizeof(uint32_t))
typedef struct L_EAFSTRING {
	void *value;
	uint8_t key[0];
} ls_t, *Pls_t;

#define IS_LINK(ptr) (((unsigned long)ptr) & 0x1)
#define GET_LINK(ptr) (void *)(((unsigned long)ptr) & ~0x1UL)
#define SET_LINK(ptr) (void *)(((unsigned long)ptr) | 0x1UL)
#define JUDY_FOREACH(j, i, p)	\
	for ((i)=0,(p)=JudyLFirst(j,&(i));(p)!=NULL&&(p)!=PPJERR;(p)=JudyLNext(j,&(i)))

static inline uint32_t string_to_word(uint8_t *str, size_t len)
{
	uint32_t ret = 0;
	switch (len) {
	case 4: ret += ((uint32_t)str[3]) << 24;
	case 3: ret += ((uint32_t)str[2]) << 16;
	case 2: ret += ((uint32_t)str[1]) <<  8;
	case 1: ret += ((uint32_t)str[0]);
	}
	return ret;
}

static Pls_t pls_new(void *key, size_t len)
{
	Pls_t ret = malloc(len + sizeof(*ret));
	if (ret) {
		ret->value = NULL;
		memcpy(ret->key, key, len);
	}
	return ret;
}

void **JudyHtbGet(const void *PArray, void *Str, size_t Len, uint32_t Index)
{
	void **PPValue, *judy;
	Pls_t list = NULL;
	uint32_t idx = 0;

	if (Str == NULL && Len != 0)
		return NULL;
	if ((PPValue = JudyLGet(PArray, Len)) == NULL)
		return NULL;
	if (Len <= WORDSIZE)
		return JudyLGet(*PPValue, string_to_word(Str, Len));
	if ((PPValue = JudyLGet(*PPValue, Index)) == NULL)
		return NULL;
	list = *PPValue;
	if (!IS_LINK(list))
		return list && !memcmp(Str, list->key, Len) ?  &list->value : NULL;

	judy = GET_LINK(list);
	JUDY_FOREACH(judy, idx, PPValue) {
		list = *PPValue;
		if (list && memcmp(Str, list->key, Len) == 0)
			return &list->value;
	}
	return NULL;
}

void **JudyHtbIns(void **PPArray, void *Str, size_t Len, uint32_t Index)
{
	void **PPValue, *judy = NULL, **ptr;
	Pls_t list = NULL;
	uint32_t idx = 0;

	if (Str == NULL && Len != 0) {
		JL_SET_ERRNO(JLE_NULLPINDEX);
		return PPJERR;
	}

	if ((PPValue = JudyLIns(PPArray, Len)) == PPJERR)
		return PPJERR;
	if (Len <= WORDSIZE)
		return JudyLIns(PPValue, string_to_word(Str, Len));
	if ((PPValue = JudyLIns(PPValue, Index)) == PPJERR)
		return PPJERR;
	if (*PPValue == NULL) {
		if ((list = pls_new(Str, Len)) == NULL)
			return PPJERR;
		*PPValue = list;
		return &list->value;
	}

	if (!IS_LINK(*PPValue)) {
		list = *PPValue;
		if (memcmp(Str, list->key, Len) == 0)
			return &list->value;
		if ((ptr = JudyLIns(&judy, idx++)) == PPJERR)
			return PPJERR;
		*ptr = list;
	} else {
		judy = GET_LINK(*PPValue);
		JUDY_FOREACH(judy, idx, ptr) {
			list = *ptr;
			if (list && memcmp(Str, list->key, Len) == 0)
				return &list->value;
		}
	}

	if ((ptr = JudyLIns(&judy, idx)) == PPJERR)
		return PPJERR;
	if ((list = pls_new(Str, Len)) == NULL)
		return PPJERR;
	assert(*ptr == NULL);
	*ptr = list;
	*PPValue = SET_LINK(judy);
	return &list->value;
}

int JudyHtbDel(void **PPArray, void *Str, size_t Len, void **PPValue, uint32_t Index)
{
	void **PPBucket, **PPHtble;
	Pls_t list = NULL;

	if (PPArray == NULL)
		return 0;
	if ((PPHtble = JudyLGet(*PPArray, Len)) == NULL)
		return 0;
	if (Len <= WORDSIZE) {
		int r = JudyLDel(PPHtble, string_to_word(Str, Len), PPValue);
		if (*PPHtble == NULL)
			JudyLDel(PPArray, Len, NULL);
		return r;
	}

	if ((PPBucket = JudyLGet(*PPHtble, Index)) == NULL)
		return 0;
	if (!IS_LINK(*PPBucket)) {
		list = *PPValue;
		if (list == NULL || memcmp(Str, list->key, Len))
			return 0;
		if (PPValue)
			*PPValue = list->value;
		free(list);
		JudyLDel(PPHtble, Index, NULL);
	} else {
		uint32_t idx = 0;
		void *judy = GET_LINK(*PPBucket), **ptr;
		JUDY_FOREACH(judy, idx, ptr) {
			list = *ptr;
			if (list && memcmp(list->key, Str, Len) == 0) {
				if (PPValue)
					*PPValue = list->value;
				free(list);
				JudyLDel(&judy, idx, NULL);
				if (judy == NULL)
					JudyLDel(PPHtble, Index, NULL);
				else
					*PPBucket = SET_LINK(judy);
				goto END;
			}
		}
		return 0;
	}
END:
	if (*PPHtble == NULL)
		JudyLDel(PPArray, Len, NULL);
	return 1;
}

void JudyHtbFreeArray(void **judy)
{
	uint32_t len = 0, idx = 0, hidx = 0;
	void **htb, **slot, **item;

	if (judy == NULL)
		return;
	JUDY_FOREACH(*judy, len, htb) {
		if (len > WORDSIZE) {
			JUDY_FOREACH(*htb, idx, slot) {
				if (IS_LINK(*slot)) {
					void *nj = GET_LINK(*slot);
					JUDY_FOREACH(nj, hidx, item) {
						free(*item);
					}
					JudyLFreeArray(nj);
				} else {
					free(*slot);
				}
			}
		}
		JudyLFreeArray(htb);
	}
	JudyLFreeArray(judy);
}
