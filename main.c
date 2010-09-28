#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include "Judy.h"

static unsigned cyc_hi = 0;
static unsigned cyc_lo = 0;

void access_counter(unsigned *hi, unsigned *lo)
{
	asm("rdtsc; movl %%edx,%0; movl %%eax,%1" : "=r" (*hi), "=r" (*lo): : "%edx", "%eax");
}

void start_counter(void)
{
	access_counter(&cyc_hi, &cyc_lo);
}

double get_counter(void)
{
	unsigned ncyc_hi, ncyc_lo;
	unsigned hi, lo, borrow;
	double result;

	access_counter(&ncyc_hi, &ncyc_lo);

	lo = ncyc_lo - cyc_lo;
	borrow = lo > ncyc_lo;
	hi = ncyc_hi - cyc_hi - borrow;

	result = (double) hi * (1<<30) * 4 + lo;

	if (result < 0) {
		fprintf(stderr, "Error: counter return neg value: %.0f\n", result);
	}

	return result;
}

#define INSERT		0x100
#define NEXTITER	0x010
#define PREVITER	0x001
#define CALL		0x200

#define SN_ 100000
struct object {
	int nums;
	unsigned long state;
	int rank;
} buff[SN_];

int nums[SN_];

int N_ = SN_;

int insert(void **root)
{
	int i = 0;
	int num = 0;
	double count = 0;
	void ** ret;
	start_counter();
	for (i=0; i< N_; i++) {
		if (buff[i].nums == 0)
			continue;
		ret = JudyLIns(root, buff[i].nums);
		if (ret == (void **)-1)
			abort();
		if (*ret == NULL) {
			*ret = (void *)(buff + i);
			buff[i].state |= INSERT;
			num++;
		} else {
			struct object *p = *ret;
			assert(p->nums = buff[i].nums);
			buff[i].nums = 0;
		}
	}
	count = get_counter();
	printf("insert count %lf\n", count);

	return num;
}

void search(const void *root)
{
	int i = 0, j = 0;
	double count = 0;
	uint32_t p = 0;
	struct object **ret = 0;
	start_counter();
	for (i=0; i< N_; i++) {
		if (buff[i].nums == 0) {
			ret = (struct object **)JudyLGet(root, buff[i].nums);
			assert(ret != (struct object **)-1);
			assert(ret == NULL);
		} else {
			ret = (struct object **)JudyLGet(root, buff[i].nums);
			assert(ret != NULL);
			assert(ret != (struct object **)-1);
			assert(*ret == buff + i);
		}
	}

	p = 0;
	ret = (struct object **)JudyLGet(root, p);
	assert(ret == NULL);
	p = ~0UL;
	ret = (struct object **)JudyLGet(root, p);
	if (ret != NULL) {
		assert(*ret < buff + N_ && *ret >= buff);
		assert((*ret)->nums == p);
	}
	count = get_counter();
	printf("search count %lf\n", count);

	start_counter();
	for (i=N_-1; i>=0; i--) {
		if (buff[i].nums == 0) {
			ret = (struct object **)JudyLGet(root, buff[i].nums);
			assert(ret != (struct object **)-1);
			assert(ret == NULL);
		} else {
			ret = (struct object **)JudyLGet(root, buff[i].nums);
			assert(ret != NULL);
			assert(ret != (struct object **)-1);
			assert(*ret == buff + i);
		}
	}
	count = get_counter();
	printf("search count %lf\n", count);

	start_counter();
	for (j = 0; j < N_; j++) {
		int i = random() % N_;
		if (buff[i].nums == 0) {
			ret = (struct object **)JudyLGet(root, buff[i].nums);
			assert(ret != (struct object **)-1);
			assert(ret == NULL);
		} else {
			ret = (struct object **)JudyLGet(root, buff[i].nums);
			assert(ret != NULL);
			assert(ret != (struct object **)-1);
			assert(*ret == buff + i);
		}
	}
	count = get_counter();
	printf("search count %lf\n", count);

	start_counter();
	for (i = 0; i < N_; i++) {
		j = i % 1;
		ret = (struct object **)JudyLGet(root, buff[i].nums + j);
		if (j == 0 && buff[i].nums != 0) {
			assert(ret != NULL);
			assert(ret != (struct object **)-1);
			assert(*ret == buff + i);
		} else {
			assert(ret == NULL);
		}
	}
	count = get_counter();
	printf("search count %lf\n", count);

	start_counter();
	for (j = 0; j < N_; j++) {
		int i = random() % N_;
		int s = i % 1;

		ret = (struct object **)JudyLGet(root, buff[i].nums + s);
		if (s == 0 && buff[i].nums != 0) {
			assert(ret != NULL);
			assert(ret != (struct object **)-1);
			assert(*ret == buff + i);
		} else {
			assert(ret == NULL);
		}
	}
	count = get_counter();
	printf("search count %lf\n", count);
}

void delete(void **root)
{
	int i = 0;
	double count = 0;

	start_counter();
	for (i=0; i< N_; i++)
		JudyLDel(root, buff[i].nums);

	count = get_counter();
	assert(*root == NULL);
	printf("delete count %lf\n", count);
}

void next(void *root, int num)
{
	uint32_t p = 0;
	struct object **ret = NULL;
	int n = 0;
	uint32_t pp = 0;
	double count = 0;
	int i = 0;
	start_counter();
	for (ret = (struct object **)JudyLFirst(root, &p);
	     (ret != (struct object **)-1 && ret != NULL);
	     ret = (struct object **)JudyLNext(root, &p)) {
		n++;
		assert((*ret)->nums == p);
		assert(*ret >= buff && *ret < buff + N_);
		assert(pp <= p);
		assert((*ret)->state & INSERT);
		(*ret)->state |= NEXTITER;
		if ((*ret)->rank != 0)
			assert(n == (*ret)->rank);
		else
			(*ret)->rank = n;
		pp = p;
	}

	for (i = 0; i < N_; i++)
		assert(!(buff[i].state & INSERT) || (buff[i].state & NEXTITER));

	assert(n == num);
	count = get_counter();

	printf("next count %lf\n", count);
}

void prev(void *root, int num)
{
	double count = 0;
	uint32_t p = ~0;
	struct object **ret = NULL;
	int n = 0;
	uint32_t pp = ~0;
	int i = 0;
	for (ret = (struct object **)JudyLLast(root, &p);
	     (ret != (struct object **)-1 && ret != NULL);
	     ret = (struct object **)JudyLPrev(root, &p)) {
		n++;
		assert((*ret)->nums == p);
		assert(*ret >= buff && *ret < buff + N_);
		assert(pp >= p);
		assert((*ret)->state & INSERT);
		(*ret)->state |= PREVITER;
		if ((*ret)->rank != 0)
			assert(n + (*ret)->rank == num + 1);
		else
			(*ret)->rank = num - n + 1;
		nums[(*ret)->rank - 1] = (*ret)->nums;
		pp = p;
	}

	for (i = 0; i < N_; i++)
		assert(!(buff[i].state & INSERT) || (buff[i].state & PREVITER));

	assert(n == num);
	count = get_counter();
	printf("prev count %lf\n", count);
}

void bycount(void *root)
{
	int i = 0;
	double count = 0;
	void ** ret;
	uint32_t p = 0;
	start_counter();
	for (i=0; i< N_; i++) {
		if (buff[i].nums == 0)
			continue;
		ret = JudyLByCount(root, buff[i].rank, &p);
		assert(ret != (void **)-1);
		assert(*ret == (void *)(buff + i));
	}
	count = get_counter();
	printf("bycount count %lf\n", count);
}

void ccount(void *root, int num)
{
	int i = 0;
	double count = 0;
	start_counter();
	for (i=0; i< N_ / 1000; i++) {
		int p1 = random() % num;
		int p2 = random() % num;
		int ret = 0;
		if (p1 > p2) {
			int t = p1; p1 = p2; p2 = t;
		}
		ret = JudyLCount(root, nums[p1], nums[p2]);
		assert(ret == (p2 - p1 + 1));
	}
	count = get_counter();
	printf("count count %lf\n", count);
}

struct walk_ctx {
	int nums;
	int calls;
	int prev;
};

int walk_fn(void *ctx, uint32_t idx, void **value)
{
	struct object *ret = *value;
	struct walk_ctx *c = ctx;
	assert(ret < buff + N_ && ret >= buff);
	assert(ret->nums == idx);
	assert(ret->nums != 0);
	assert((ret->state & CALL) == 0);
	assert(c->prev <= idx);
	ret->state |= CALL;
	c->prev = idx;
	c->calls++;
	return 0;
}

void walk(void *root, int nums)
{
	struct walk_ctx ctx = { 0, 0, 0};
	int ret = 0;

	double count = 0;
	start_counter();

	ret = JudyLWalk(root, walk_fn, &ctx);
	assert(ret == 0);
	assert(ctx.calls == nums);
	count = get_counter();
	printf("walk count %lf\n", count);
}

void do_test(int m_dup)
{
	int num = 0;
	void *root = NULL;

	num = insert(&root);
	if (m_dup == 0)
		assert(num == N_);
	printf("memory count %u\n", JudyLMemUsed(root));
	search(root);
	walk(root, num);
	prev(root, num);
	next(root, num);
	ccount(root, num);
	bycount(root);
	delete(&root);
	// JudyLFreeArray(&root);
}

void insert_array(void)
{
	void *root = NULL;
	uint32_t *index = calloc(N_, sizeof(uint32_t));
	void **value = calloc(N_, sizeof(void *));
	int i = 0;
	int loop = random() % 256 + 1;
	double count = 0;
	memset(buff, 0, sizeof(buff));
	memset(nums, 0, sizeof(nums));

	for (i = 0; i < N_; i++) {
		index[i] = (i + 1) * loop;
		buff[i].nums = index[i];
		buff[i].state |= INSERT;
		value[i] = buff + i;
	}

	start_counter();
	i = JudyLInsArray(&root, N_, index, value);
	count = get_counter();
	free(index);
	free(value);
	assert(i == 1);
	printf("Ins array count %lf\n", count);

	printf("memory count %u\n", JudyLMemUsed(root));
	search(root);
	walk(root, N_);
	prev(root, N_);
	next(root, N_);
	ccount(root, N_);
	bycount(root);
	// JudyLFreeArray(&root);
	delete(&root);
	for (i = 0; i < N_; i++) {
		buff[i].state = 0;
	}
	do_test(0);
}

void random_test(void)
{
	int i = 0;

	printf("RANDOM TEST:\n");
	memset(buff, 0, sizeof(buff));
	memset(nums, 0, sizeof(nums));
	// srand(0);
	for (i = 0; i< N_; i++) {
		buff[i].nums = random();
		if (buff[i].nums == 0)
			buff[i].nums = 1;
	}

	do_test(1);
}

void loop_test(void)
{
	int i = 0;
	int num = 0;
	int loop = 0;

	printf("LOOP TEST:\n");
	memset(buff, 0, sizeof(buff));
	memset(nums, 0, sizeof(nums));
	// srand(0);
	num = random();
	loop = random() % 256 + 1;
	for (i = 0; i< N_; i++)
		buff[i].nums = num + i * loop;

	do_test(0);
}

void line_test(void)
{
	int i = 0;

	printf("LINE TEST:\n");
	memset(buff, 0, sizeof(buff));
	memset(nums, 0, sizeof(nums));
	for (i = 0; i< N_; i++)
		buff[i].nums = 1 + i;

	do_test(0);
}

int main(void)
{
	line_test();
	random_test();
	loop_test();
	insert_array();
	return 0;
}
