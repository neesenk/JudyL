#include "JudyL.h"

/* Leave the malloc() sizes readable in the binary (via strings(1)): */
const char *JudyLMallocSizes =
	"JudyLMallocSizes = 3, 5, 7, 11, 15, 23, 32, 47, 64, Leaf1 = 25";

/* cJL_BITSPERSUBEXPB = 32 */
const uint8_t jL_BranchBJPPopToWords[cJL_BITSPERSUBEXPB + 1] =
{
	0, 3,  5,  7, 11, 11, 15, 15, 23, 23, 23, 23, 32, 32, 32, 32, 32,
	47, 47, 47, 47, 47, 47, 47, 64, 64, 64, 64, 64, 64, 64, 64, 64
};

/* cJL_LEAF1_MAXPOP1 = 25 */
const uint8_t jL_Leaf1PopToWords[cJL_LEAF1_MAXPOP1 + 1] =
{
	0, 3,  3,  5,  5,  7, 11, 11, 11, 15, 15, 15, 15,
	23, 23, 23, 23, 23, 23, 32, 32, 32, 32, 32, 32, 32
};

const uint8_t jL_Leaf1Offset[cJL_LEAF1_MAXPOP1 + 1] =
{
	 0, 1, 1, 1, 1, 2, 3, 3, 3, 3, 3, 3, 3,
	 5, 5, 5, 5, 5, 5, 7, 7, 7, 7, 7, 7, 7
};

/* cJL_LEAF2_MAXPOP1 = 42 */
const uint8_t jL_Leaf2PopToWords[cJL_LEAF2_MAXPOP1 + 1] =
{
	0, 3,  3,  5,  7, 11, 11, 11, 15, 15, 15, 23, 23, 23, 23, 23, 32,
	32, 32, 32, 32, 32, 47, 47, 47, 47, 47, 47, 47, 47, 47, 47, 63,
	63, 63, 63, 63, 63, 63, 63, 63, 63, 63
};
const uint8_t jL_Leaf2Offset[cJL_LEAF2_MAXPOP1 + 1] =
{
       	0, 1,  1,  2,  2,  4,  4,  4,  5, 5,  5,  8,  8,  8,  8,  8, 11, 11,
	11, 11, 11, 11, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 21, 21, 21,
	21, 21, 21, 21, 21, 21, 21, 21
};

/* cJL_LEAF3_MAXPOP1 = 36 */
const uint8_t jL_Leaf3PopToWords[cJL_LEAF3_MAXPOP1 + 1] =
{
	0, 3,  5,  7,  7, 11, 11, 15, 15, 23, 23, 23, 23, 23, 32, 32, 32, 32, 32,
	47, 47, 47, 47, 47, 47, 47, 47, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63
};

const uint8_t jL_Leaf3Offset[cJL_LEAF3_MAXPOP1 + 1] =
{
	0, 1,  3,  3,  3,  5,  5,  6,  6, 10, 10, 10, 10, 10, 14, 14, 14, 14,
	14, 20, 20, 20, 20, 20, 20, 20, 20, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27
};

/* cJL_LEAFW_MAXPOP1 = 31 */
const uint8_t jL_LeafWPopToWords[cJL_LEAFW_MAXPOP1 + 1] =
{
	0, 3,  5,  7, 11, 11, 15, 15, 23, 23, 23, 23, 32, 32, 32, 32,
	47, 47, 47, 47, 47, 47, 47, 47, 63, 63, 63, 63, 63, 63, 63, 63
};

const uint8_t jL_LeafWOffset[cJL_LEAFW_MAXPOP1 + 1] =
{
	0, 2, 3, 4, 6, 6, 8, 8, 12, 12, 12, 12, 16, 16, 16, 16, 24,
	24, 24, 24, 24, 24, 24, 24, 32, 32, 32, 32, 32, 32, 32, 32
};

/* cJL_BITSPERSUBEXPL = 32 */
const uint8_t jL_LeafVPopToWords[cJL_BITSPERSUBEXPL + 1] =
{
	0, 3, 3, 3, 5, 5, 7, 7, 11, 11, 11, 11, 15, 15, 15, 15, 23,
	23, 23, 23, 23, 23, 23, 23, 32, 32, 32, 32, 32, 32, 32, 32, 32
};
