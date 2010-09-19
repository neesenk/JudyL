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


int buff[10000000];
int main(void)
{
	int arrlen  = sizeof(buff)/sizeof(buff[0]);
	int i = 0;
	int num = 0;
	clock_t beg = 0;
	double count = 0;
	void *root = NULL;
	int j = 0;
	printf("array size %d\n", arrlen);

	srand(0);
	num = random();
	for (i = 0; i< arrlen; i++) {
	//	if (i % 1000 == 0)
	//		num = random();
	//	buff[i] = num + i;
		buff[i] = random();
	}
	num = 0;
	start_counter();
	for (i=0; i<arrlen; i++) {
		void **ret = JudyLIns(&root, buff[i]);
		if (ret == (void **)-1) 
			abort();
		if (*ret == NULL)
			*ret = (void *)buff[i];
		else 
			num++;
	}
	count += get_counter();
	printf("insert count %lf\n", count);
	printf("num %d\n", num);

	count = 0;
	start_counter();
	beg = clock();
	for (j = 0; j < 10000000; j += arrlen)
	for (i=0; i<arrlen; i +=4) {
		JudyLGet(root, buff[i]);
		JudyLGet(root, buff[i + 1]);
		JudyLGet(root, buff[i + 2]);
		JudyLGet(root, buff[i + 3]);
	}
	count += get_counter();
	printf("clock %d\n", (int)(clock() - beg));
	printf("search count %lf\n", count);
	printf("memory count %lu\n", JudyLMemUsed(root));

	count = 0;
	{ int idx = random() % arrlen;
	  while (buff[idx] == 0) idx++; 
	start_counter();
	beg = clock();
	for (j = 0; j < 10000000; j += arrlen)
	for (i=0; i<arrlen; i +=4) {
		JudyLGet(root, buff[idx]);
		JudyLGet(root, buff[idx]);
		JudyLGet(root, buff[idx]);
		JudyLGet(root, buff[idx]);
	}
	count += get_counter();
	printf("clock %d\n", (int)(clock() - beg));
	printf("search count %lf\n", count);
	}

	count = 0;
	start_counter();
	for (i=0; i<arrlen; i++) {
		JudyLDel(&root, buff[i]);
	}
	count += get_counter();
	printf("delete count %lf\n", count);

	return 0;
}
