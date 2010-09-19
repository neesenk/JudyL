#include "JudyL.h"
extern int judyCreateBranchL(Pjp_t, Pjp_t, uint8_t *, Word_t , void *);

int judyInsertBranch(Pjp_t Pjp,	Word_t Index, Word_t BranchLevel, Pjpm_t Pjpm)
{
	jp_t JP2[2], JP;
	Pjp_t PjpNull;
	Word_t XorExp, Inew, Iold, DCDMask;
	uint8_t Exp2[2], DecodeByteN, DecodeByteO;
	int Ret;

	DCDMask = cJL_DCDMASK(BranchLevel);
	XorExp = ((Index ^ JL_JPDCDPOP0(Pjp)) & (cJL_ALLONES >> cJL_BITSPERBYTE))
		 >> (BranchLevel * cJL_BITSPERBYTE);
	assert(XorExp);

	do {
		++BranchLevel;
	} while ((XorExp >>= cJL_BITSPERBYTE));
	assert((BranchLevel > 1) && (BranchLevel < cJL_ROOTSTATE));

	DecodeByteO = JL_DIGITATSTATE(JL_JPDCDPOP0(Pjp), BranchLevel);
	DecodeByteN = JL_DIGITATSTATE(Index, BranchLevel);
	assert(DecodeByteO != DecodeByteN);

	if (DecodeByteN > DecodeByteO)
		Iold = 0, Inew = 1;
	else
		Iold = 1, Inew = 0;

	JP2[Iold] = *Pjp;
	Exp2[Iold] = DecodeByteO;
	Exp2[Inew] = DecodeByteN;

	if ((Ret = judyCreateBranchL(Pjp, JP2, Exp2, 2, Pjpm)) == -1)
		return -1;

	PjpNull = ((P_JBL(Pjp->jp_Addr))->jbl_jp) + Inew;
	JL_JPSETADT(PjpNull, 0, Index, cJL_JPIMMED_1_01 - 2 + BranchLevel);
	Pjpm->jpm_PValue = (Pjv_t) PjpNull;

	Pjp->jp_Type = cJL_JPBRANCH_L2 - 2 + BranchLevel;
	DCDMask ^= cJL_DCDMASK(BranchLevel);
	DCDMask = ~DCDMask & JL_JPDCDPOP0(Pjp);
	JP = *Pjp;
	JL_JPSETADT(Pjp, JP.jp_Addr, DCDMask, JP.jp_Type);

	return 1;
}
