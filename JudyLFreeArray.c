#include "JudyL.h"

size_t JudyLFreeArray(void **PPArray)
{
	jLpm_t jpm;

	if (PPArray == NULL) {
		JL_SET_ERRNO(JL_ERRNO_NULLPPARRAY);
		return JERR;
	}
	jpm.jpm_Pop0 = 0;
	jpm.jpm_TotalMemWords = 0;

	if (P_JLW(*PPArray) == NULL)
		return 0;

	if (JL_LEAFW_POP0(*PPArray) < cJL_LEAFW_MAXPOP1) {
		Pjlw_t Pjlw = P_JLW(*PPArray);

		judyLFreeJLW(Pjlw, Pjlw[0] + 1, &jpm);
		*PPArray = NULL;
		return (-(jpm.jpm_TotalMemWords * cJL_BYTESPERWORD));
	} else {
		Pjpm_t Pjpm = P_JPM(*PPArray);
		Word_t TotalMem = Pjpm->jpm_TotalMemWords;

		judyLFreeSM(&(Pjpm->jpm_JP), &jpm);
		judyLFreeJPM(Pjpm, &jpm);

		if (TotalMem + jpm.jpm_TotalMemWords) {
			JL_SET_ERRNO(JL_ERRNO_CORRUPT);
			return JERR;
		}

		*PPArray = NULL;
		return TotalMem * cJL_BYTESPERWORD;
	}
}

void judyLFreeSM(Pjp_t Pjp, Pjpm_t Pjpm)
{
	Word_t Pop1;

	switch (JL_JPTYPE(Pjp)) {
	case cJL_JPBRANCH_L:
	case cJL_JPBRANCH_L2:
	case cJL_JPBRANCH_L3: {
		Pjbl_t Pjbl = P_JBL(Pjp->jp_Addr);
		Word_t offset;

		for (offset = 0; offset < Pjbl->jbl_NumJPs; ++offset)
			judyLFreeSM((Pjbl->jbl_jp) + offset, Pjpm);

		judyLFreeJBL((Pjbl_t) (Pjp->jp_Addr), Pjpm);
		break;
	}
	case cJL_JPBRANCH_B:
	case cJL_JPBRANCH_B2:
	case cJL_JPBRANCH_B3: {
		Word_t subexp, offset, jpcount;
		Pjbb_t Pjbb = P_JBB(Pjp->jp_Addr);
		for (subexp = 0; subexp < cJL_NUMSUBEXPB; ++subexp) {
			jpcount = judyCountBits(JL_JBB_BITMAP(Pjbb, subexp));

			if (jpcount) {
				for (offset = 0; offset < jpcount; ++offset)
					judyLFreeSM(P_JP(JL_JBB_PJP(Pjbb, subexp)) + offset, Pjpm);
				judyLFreeJBBJP(JL_JBB_PJP(Pjbb, subexp), jpcount, Pjpm);
			}
		}
		judyLFreeJBB((Pjbb_t) (Pjp->jp_Addr), Pjpm);
		break;
	}
	case cJL_JPBRANCH_U:
	case cJL_JPBRANCH_U2:
	case cJL_JPBRANCH_U3: {
		Word_t offset;
		Pjbu_t Pjbu = P_JBU(Pjp->jp_Addr);
		for (offset = 0; offset < cJL_BRANCHUNUMJPS; ++offset)
			judyLFreeSM((Pjbu->jbu_jp) + offset, Pjpm);

		judyLFreeJBU((Pjbu_t) (Pjp->jp_Addr), Pjpm);
		break;
	}
	case cJL_JPLEAF1:
		Pop1 = JL_JPLEAF_POP0(Pjp) + 1;
		judyLFreeJLL1((Pjll_t) (Pjp->jp_Addr), Pop1, Pjpm);
		break;
	case cJL_JPLEAF2:
		Pop1 = JL_JPLEAF_POP0(Pjp) + 1;
		judyLFreeJLL2((Pjll_t) (Pjp->jp_Addr), Pop1, Pjpm);
		break;
	case cJL_JPLEAF3:
		Pop1 = JL_JPLEAF_POP0(Pjp) + 1;
		judyLFreeJLL3((Pjll_t) (Pjp->jp_Addr), Pop1, Pjpm);
		break;
	case cJL_JPLEAF_B1: {
		Word_t subexp;
		Word_t jpcount;
		Pjlb_t Pjlb = P_JLB(Pjp->jp_Addr);
		for (subexp = 0; subexp < cJL_NUMSUBEXPL; ++subexp) {
			jpcount = judyCountBits(JL_JLB_BITMAP(Pjlb, subexp));

			if (jpcount)
				judyLFreeJV(JL_JLB_PVALUE(Pjlb, subexp), jpcount, Pjpm);
		}
		judyLFreeJLB1((Pjlb_t) (Pjp->jp_Addr), Pjpm);
		break;
	}
	case cJL_JPIMMED_1_02:
	case cJL_JPIMMED_1_03:
		Pop1 = JL_JPTYPE(Pjp) - cJL_JPIMMED_1_02 + 2;
		judyLFreeJV((Pjv_t) (Pjp->jp_Addr), Pop1, Pjpm);
		break;
	default:
		break;
	}
}
