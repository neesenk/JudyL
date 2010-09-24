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

#define N_ 10000000
int buff[N_];

int random_test(void)
{
	int arrlen  = sizeof(buff)/sizeof(buff[0]);
	int i = 0;
	int num = 0;
	clock_t beg = 0;
	double count = 0;
	void *root = NULL;
	printf("array size %d\n", arrlen);

	srand(0);
	num = random();
	for (i = 0; i< arrlen; i++) {
		buff[i] = random();
		if (buff[i] == 0)
			buff[i] = 1;
	}
	num = 0;
	start_counter();
	for (i=0; i<arrlen; i++) {
		extern void bianli2(const void *PArray);
		void **ret = JudyLIns(&root, buff[i]);
		if (ret == (void **)-1) 
			abort();
		if (*ret == NULL)
			*ret = (void *)(buff + i);
		else { 
			int *p = *ret;
			assert(*p = buff[i]);
			buff[i] = 0;
			num++;
		}
	}
	count += get_counter();
	printf("insert count %lf\n", count);
	printf("num %d\n", num);

	count = 0;
	start_counter();
	beg = clock();
	for (i=0; i<arrlen; i++) {
		int **ret = 0;
		if (buff[i] == 0) {
			ret = (int **)JudyLGet(root, buff[i]);
			assert(ret != (int **)-1);
			assert(ret == NULL);
		} else {
			ret = (int **)JudyLGet(root, buff[i]);
			assert(ret != (int **)-1);
			assert(*ret == buff + i);
		}
	}
	count += get_counter();
	printf("clock %d\n", (int)(clock() - beg));
	printf("search count %lf\n", count);
	printf("memory count %u\n", JudyLMemUsed(root));


	count = 0;
	start_counter();
	beg = clock();
	{ 
		uint32_t p = 0;
		int **ret = NULL;
		int n = 0;
		uint32_t pp = 0;
		for (ret = (int **)JudyLFirst(root, &p); 
		     (ret != (int **)-1 && ret != NULL);
		     ret = (int **)JudyLNext(root, &p)) {
			n++;
			assert(**ret == p);
			assert(*ret >= buff && *ret < buff + arrlen);
			assert(pp <= p);
			pp = p;
		}
		assert(n+num == arrlen);
	}
	count += get_counter();
	printf("clock %d\n", (int)(clock() - beg));
	printf("next count %lf\n", count);

	count = 0;
	start_counter();
	beg = clock();
	{ 
		uint32_t p = ~0;
		int **ret = NULL;
		int n = 0;
		uint32_t pp = ~0;
		for (ret = (int **)JudyLLast(root, &p); 
		     (ret != (int **)-1 && ret != NULL);
		     ret = (int **)JudyLPrev(root, &p)) {
			n++;
			assert(**ret == p);
			assert(*ret >= buff && *ret < buff + arrlen);
			assert(pp >= p);
			pp = p;
		}

		assert(n+num == arrlen);
	}
	count += get_counter();
	printf("clock %d\n", (int)(clock() - beg));
	printf("prev count %lf\n", count);

	count = 0;
	start_counter();
	for (i=0; i<arrlen; i++) {
		JudyLDel(&root, buff[i]);
	}
	count += get_counter();
	printf("delete count %lf\n", count);

	return 0;
}

int main(void)
{
	random_test();
	return 0;
}
