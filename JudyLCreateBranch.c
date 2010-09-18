#include "JudyL.h"

// Build a BranchL from an array of JPs and associated 1 byte digits
// (expanses).  Return with Pjp pointing to the BranchL.  Caller must
// deallocate passed arrays, if necessary.
// We have no idea what kind of BranchL it is, so caller must set the jp_Type.
// Return -1 if error (details in Pjpm), otherwise return 1.
int judyCreateBranchL(Pjp_t Pjp,	// Build JPs from this place
		      Pjp_t PJPs,	// Array of JPs to put into Bitmap branch
		      uint8_t Exp[],	// Array of expanses to put into bitmap
		      Word_t ExpCnt,	// Number of above JPs and Expanses
		      Pvoid_t Pjpm)
{
	Pjbl_t PjblRaw;		// pointer to linear branch.
	Pjbl_t Pjbl;
	assert(ExpCnt <= cJL_BRANCHLMAXJPS);
	PjblRaw = judyLAllocJBL(Pjpm);
	if (PjblRaw == (Pjbl_t) NULL)
		return (-1);
	Pjbl = P_JBL(PjblRaw);

	Pjbl->jbl_NumJPs = ExpCnt;

	JL_COPYMEM(Pjbl->jbl_Expanse, Exp, ExpCnt);
	JL_COPYMEM(Pjbl->jbl_jp, PJPs, ExpCnt);

	Pjp->jp_Addr = (Word_t) PjblRaw;
	return (1);
}				// judyCreateBranchL()

// Build a BranchB from an array of JPs and associated 1 byte digits
// (expanses).  Return with Pjp pointing to the BranchB.  Caller must
// deallocate passed arrays, if necessary.
// We have no idea what kind of BranchB it is, so caller must set the jp_Type.
// Return -1 if error (details in Pjpm), otherwise return 1.
int judyCreateBranchB(Pjp_t Pjp,	// Build JPs from this place
		      Pjp_t PJPs,	// Array of JPs to put into Bitmap branch
		      uint8_t Exp[],	// Array of expanses to put into bitmap
		      Word_t ExpCnt,	// Number of above JPs and Expanses
		      Pvoid_t Pjpm)
{
	Pjbb_t PjbbRaw;		// pointer to bitmap branch.
	Pjbb_t Pjbb;
	Word_t ii, jj;		// Temps
	uint8_t CurrSubExp;	// Current sub expanse for BM
// This assertion says the number of populated subexpanses is not too large.
// This function is only called when a BranchL overflows to a BranchB or when a
// cascade occurs, meaning a leaf overflows.  Either way ExpCnt cant be very
// large, in fact a lot smaller than cJL_BRANCHBMAXJPS.  (Otherwise a BranchU
// would be used.)  Popping this assertion means something (unspecified) has
// gone very wrong, or else Judys design criteria have changed, although in
// fact there should be no HARM in creating a BranchB with higher actual
// fanout.
	assert(ExpCnt <= cJL_BRANCHBMAXJPS);

	PjbbRaw = judyLAllocJBB(Pjpm);
	if (PjbbRaw == (Pjbb_t) NULL)
		return (-1);
	Pjbb = P_JBB(PjbbRaw);

	CurrSubExp = Exp[0] / cJL_BITSPERSUBEXPB;

	for (jj = ii = 0; ii <= ExpCnt; ii++) {
		Word_t SubExp;	// Cannot be a uint8_t

		if (ii == ExpCnt) {
			SubExp = cJL_ALLONES;	// Force last one
		} else {
			SubExp = Exp[ii] / cJL_BITSPERSUBEXPB;	// Bits 5..7.

			JL_JBB_BITMAP(Pjbb, SubExp) |= JL_BITPOSMASKB(Exp[ii]);
		}
		if (SubExp != CurrSubExp) {
			Word_t NumJP = ii - jj;
			Pjp_t PjpRaw;
			Pjp_t Pjp;

			PjpRaw = judyLAllocJBBJP(NumJP, Pjpm);
			Pjp = P_JP(PjpRaw);

			if (PjpRaw == (Pjp_t) NULL) {
				while (CurrSubExp--) {
					NumJP = judyCountBitsB(JL_JBB_BITMAP(Pjbb, CurrSubExp));
					if (NumJP) {
						judyLFreeJBBJP(JL_JBB_PJP(Pjbb, CurrSubExp),
							      NumJP, Pjpm);
					}
				}
				judyLFreeJBB(PjbbRaw, Pjpm);
				return (-1);
			}
			JL_JBB_PJP(Pjbb, CurrSubExp) = PjpRaw;
			JL_COPYMEM(Pjp, PJPs + jj, NumJP);
			jj = ii;
			CurrSubExp = SubExp;
		}
	}

	Pjp->jp_Addr = (Word_t) PjbbRaw;
	return (1);
}

// Build a BranchU from a BranchB.  Return with Pjp pointing to the BranchU.
// Free the BranchB and its JP subarrays.
// Return -1 if error (details in Pjpm), otherwise return 1.
int judyCreateBranchU(Pjp_t Pjp, Pvoid_t Pjpm)
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
	if (PjbuRaw == (Pjbu_t) NULL)
		return (-1);
	Pjbu = P_JBU(PjbuRaw);
	JL_JPSETADT(&JPNull, 0, 0, JL_JPTYPE(Pjp) - cJL_JPBRANCH_B2 + cJL_JPNULL1);

	// Get the pointer to the BranchB:
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
				if (BitMap & 1) {
					*PDstJP = *PjpA++;
				} else {
					*PDstJP = JPNull;
				}

				PDstJP++;	// advance to next JP
				BitMap >>= 1;
			}
			jj = PjpA - PjpB;
		}
		judyLFreeJBBJP(JL_JBB_PJP(Pjbb, ii), jj, Pjpm);
	}			// for each JP in BranchU
	judyLFreeJBB(PjbbRaw, Pjpm);
	Pjp->jp_Addr = (Word_t) PjbuRaw;
	Pjp->jp_Type += cJL_JPBRANCH_U - cJL_JPBRANCH_B;

	return (1);
}
