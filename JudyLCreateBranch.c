#include "JudyL.h"

/**
 * Build a BranchL from an array of JPs and associated 1 byte digits
 * (expanses).  Return with Pjp pointing to the BranchL.  Caller must
 * deallocate passed arrays, if necessary.
 * We have no idea what kind of BranchL it is, so caller must set the jp_Type.
 * Return -1 if error (details in Pjpm), otherwise return 1.
 *
 * @Pjp		Build JPs from this place
 * @PJPs	Array of JPs to put into Bitmap branch
 * @Exp[]	Array of expanses to put into bitmap
 * @ExpCnt	Number of above JPs and Expanses
 */
int judyCreateBranchL(Pjp_t Pjp, Pjp_t PJPs, uint8_t Exp[], Word_t ExpCnt, void *Pjpm)
{
	Pjbl_t PjblRaw, Pjbl;

	assert(ExpCnt <= cJL_BRANCHLMAXJPS);
	PjblRaw = judyLAllocJBL(Pjpm);
	if (PjblRaw == NULL)
		return -1;
	Pjbl = P_JBL(PjblRaw);

	Pjbl->jbl_NumJPs = ExpCnt;

	JL_COPYMEM(Pjbl->jbl_Expanse, Exp, ExpCnt);
	JL_COPYMEM(Pjbl->jbl_jp, PJPs, ExpCnt);

	Pjp->jp_Addr = (Word_t) PjblRaw;
	return 1;
}

/**
 * Build a BranchB from an array of JPs and associated 1 byte digits
 * (expanses).  Return with Pjp pointing to the BranchB.  Caller must
 * deallocate passed arrays, if necessary.
 * We have no idea what kind of BranchB it is, so caller must set the jp_Type.
 * Return -1 if error (details in Pjpm), otherwise return 1.
 *
 * @Pjp		Build JPs from this place
 * @PJPs	Array of JPs to put into Bitmap branch
 * @Exp		Array of expanses to put into bitmap
 * @ExpCnt	Number of above JPs and Expanses
 */
int judyCreateBranchB(Pjp_t Pjp, Pjp_t PJPs, uint8_t Exp[], Word_t ExpCnt, void *Pjpm)
{
	Pjbb_t PjbbRaw, Pjbb;
	Word_t ii, jj;
	uint8_t CurrSubExp;

	assert(ExpCnt <= cJL_BRANCHBMAXJPS);

	PjbbRaw = judyLAllocJBB(Pjpm);
	if (PjbbRaw == NULL)
		return -1;
	Pjbb = P_JBB(PjbbRaw);

	CurrSubExp = Exp[0] / cJL_BITSPERSUBEXPB;

	for (jj = ii = 0; ii <= ExpCnt; ii++) {
		Word_t SubExp;

		if (ii == ExpCnt) {
			SubExp = cJL_ALLONES;
		} else {
			SubExp = Exp[ii] / cJL_BITSPERSUBEXPB;
			JL_JBB_BITMAP(Pjbb, SubExp) |= JL_BITPOSMASKB(Exp[ii]);
		}
		if (SubExp != CurrSubExp) {
			Word_t NumJP = ii - jj;
			Pjp_t PjpRaw, Pjp;

			PjpRaw = judyLAllocJBBJP(NumJP, Pjpm);
			Pjp = P_JP(PjpRaw);

			if (PjpRaw == NULL) {
				while (CurrSubExp--) {
					NumJP = judyCountBits(JL_JBB_BITMAP(Pjbb, CurrSubExp));
					if (NumJP == 0)
						continue;
					judyLFreeJBBJP(JL_JBB_PJP(Pjbb, CurrSubExp), NumJP, Pjpm);
				}
				judyLFreeJBB(PjbbRaw, Pjpm);
				return -1;
			}
			JL_JBB_PJP(Pjbb, CurrSubExp) = PjpRaw;
			JL_COPYMEM(Pjp, PJPs + jj, NumJP);
			jj = ii;
			CurrSubExp = SubExp;
		}
	}

	Pjp->jp_Addr = (Word_t) PjbbRaw;
	return 1;
}

int judyCreateBranchU(Pjp_t Pjp, void *Pjpm)
{
	jp_t JPNull;
	Pjbu_t PjbuRaw;
	Pjbu_t Pjbu;
	Pjbb_t PjbbRaw;
	Pjbb_t Pjbb;
	Word_t ii, jj;
	BITMAPB_t BitMap;
	Pjp_t PDstJP;

	PjbuRaw = judyLAllocJBU(Pjpm);
	if (PjbuRaw == NULL)
		return -1;
	Pjbu = P_JBU(PjbuRaw);
	JL_JPSETADT(&JPNull, 0, 0, JL_JPTYPE(Pjp) - cJL_JPBRANCH_B2 + cJL_JPNULL1);

	PjbbRaw = (Pjbb_t) (Pjp->jp_Addr);
	Pjbb = P_JBB(PjbbRaw);

	PDstJP = Pjbu->jbu_jp;
	for (ii = 0; ii < cJL_NUMSUBEXPB; ii++) {
		Pjp_t PjpA;
		Pjp_t PjpB;

		PjpB = PjpA = P_JP(JL_JBB_PJP(Pjbb, ii));

		BitMap = JL_JBB_BITMAP(Pjbb, ii);

		if (BitMap == 0) {
			for (jj = 0; jj < cJL_BITSPERSUBEXPB; jj++)
				PDstJP[jj] = JPNull;
			PDstJP += cJL_BITSPERSUBEXPB;
			continue;
		}
		if (BitMap == cJL_FULLBITMAPB) {
			JL_COPYMEM(PDstJP, PjpA, cJL_BITSPERSUBEXPB);
			PDstJP += cJL_BITSPERSUBEXPB;
			jj = cJL_BITSPERSUBEXPB;
		} else {
			for (jj = 0; jj < cJL_BITSPERSUBEXPB; jj++) {
				if (BitMap & 1)
					*PDstJP = *PjpA++;
				else
					*PDstJP = JPNull;

				PDstJP++;
				BitMap >>= 1;
			}
			jj = PjpA - PjpB;
		}
		judyLFreeJBBJP(JL_JBB_PJP(Pjbb, ii), jj, Pjpm);
	}

	judyLFreeJBB(PjbbRaw, Pjpm);
	Pjp->jp_Addr = (Word_t) PjbuRaw;
	Pjp->jp_Type += cJL_JPBRANCH_U - cJL_JPBRANCH_B;

	return 1;
}
