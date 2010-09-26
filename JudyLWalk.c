#include <stdio.h>

#include "JudyL.h"
#include "JudyPrivate.h"
#include "JudyPrivateBranch.h"

#define JL_SETINDEX(INDEX, State, V)			\
	(((INDEX) & ~(cJL_MASKATSTATE(State))) |	\
	(((uint8_t)(V)) << (((State) - 1) * cJL_BITSPERBYTE)))

static int JudyWalk(Pjp_t Pjp, uint32_t prefix, walk_fn_t fn, void *ctx)
{
	int i, mask, ret = 0, state = 0;
	uint32_t Index = 0;

	switch (JL_JPTYPE(Pjp)) {
	case cJL_JPNULL1: case cJL_JPNULL2: case cJL_JPNULL3:
		return 0;
	case cJL_JPBRANCH_L2:
		state = 2;
		prefix = JL_SETINDEX(prefix, 3, Pjp->jp_DcdP0[0]);
		goto JPBRANCH_L;
	case cJL_JPBRANCH_L3:
		state = 3;
		goto JPBRANCH_L;
	case cJL_JPBRANCH_L:
		state = cJL_ROOTSTATE;
		goto JPBRANCH_L;
	JPBRANCH_L:
	{
		Pjbl_t Pjbl;
		Pjbl = P_JBL(Pjp->jp_Addr);
		for (i = 0; i < Pjbl->jbl_NumJPs; i++) {
			Index = JL_SETINDEX(prefix, state, Pjbl->jbl_Expanse[i]);
			ret = JudyWalk(Pjbl->jbl_jp + i, Index, fn, ctx);
			if (ret != 0)
				return ret;
		}

		return 0;
	}
	case cJL_JPBRANCH_B2:
		state = 2;
		prefix = JL_SETINDEX(prefix, 3, Pjp->jp_DcdP0[0]);
		goto JPBRANCH_B;
	case cJL_JPBRANCH_B3:
		state = 3;
		goto JPBRANCH_B;
	case cJL_JPBRANCH_B:
		state = cJL_ROOTSTATE;
		goto JPBRANCH_B;
	JPBRANCH_B:
	{
		uint8_t idx = 0;
		Pjbb_t Pjbb;
		BITMAPB_t BitMap, BitMask;

		Pjbb = P_JBB(Pjp->jp_Addr);
		Pjp_t Pjp2, Pjp3;
		for (i = 0; i < cJL_NUMSUBEXPB; i++) {
			BitMap = JL_JBB_BITMAP(Pjbb, i);
			Pjp2 = P_JP(JL_JBB_PJP(Pjbb, i));
			for (mask = 0; mask < cJL_BITSPERSUBEXPB; mask++) {
				BitMask = 1 << mask;
				if (!(BitMap & (BitMask)))
					continue;
				idx = i * cJL_BITSPERSUBEXPB + mask;
				Pjp3 = Pjp2 + judyCountBits(BitMap & (BitMask - 1));

				Index = JL_SETINDEX(prefix, state, idx);
				ret = JudyWalk(Pjp3, Index, fn, ctx);
				if (ret != 0)
					return ret;
			}
		}
		return 0;
	}
	case cJL_JPBRANCH_U:
		state = cJL_ROOTSTATE;
		goto JPBRANCH_U;
	case cJL_JPBRANCH_U3:
		state = 3;
		goto JPBRANCH_U;
	case cJL_JPBRANCH_U2:
		state = 2;
		prefix = JL_SETINDEX(prefix, 3, Pjp->jp_DcdP0[0]);
		goto JPBRANCH_U;
	JPBRANCH_U:
	{
		Pjbu_t Pjbu = P_JBU(Pjp->jp_Addr);
		for (i = 0; i < cJL_BRANCHUNUMJPS; i++) {
			if ((Pjbu->jbu_jp[i].jp_Type) >= cJL_JPNULL1
			    && (Pjbu->jbu_jp[i].jp_Type) <= cJL_JPNULLMAX)
				continue;

			Index = JL_SETINDEX(prefix, state, i);
			ret = JudyWalk(Pjbu->jbu_jp + i, Index, fn, ctx);
			if (ret != 0)
				return ret;
		}
		return 0;
	}
	case cJL_JPLEAF1:
	{
		Word_t Pop1 = JL_JPLEAF_POP0(Pjp) + 1;
		uint8_t *Pjll = (uint8_t *)Pjp->jp_Addr;
		Pjv_t Pjv = JL_LEAF1VALUEAREA(Pjll, Pop1);
		prefix = JL_SETINDEX(prefix, 3, Pjp->jp_DcdP0[0]);
		prefix = JL_SETINDEX(prefix, 2, Pjp->jp_DcdP0[1]);
		for (i = 0; i < Pop1; i++) {
			Index = (prefix & 0xffffff00) | Pjll[i];
			ret = fn(ctx, Index, (void **)(Pjv + i));
			if (ret != 0)
				return ret;
		}

		return 0;
	}
	case cJL_JPLEAF2:
	{
		Word_t Pop1 = JL_JPLEAF_POP0(Pjp) + 1;
		uint16_t *Pjll = (uint16_t *)Pjp->jp_Addr;
		Pjv_t Pjv = JL_LEAF2VALUEAREA(Pjll, Pop1);
		prefix = JL_SETINDEX(prefix, 3, Pjp->jp_DcdP0[0]);
		for (i = 0; i < Pop1; i++) {
			Index = (prefix & 0xffff0000) | Pjll[i];
			ret = fn(ctx, Index, (void **)(Pjv + i));
			if (ret != 0)
				return ret;
		}
		return 0;
	}
	case cJL_JPLEAF3:
	{
		Word_t Pop1 = JL_JPLEAF_POP0(Pjp) + 1;
		uint8_t *Pjll = P_JLL(Pjp->jp_Addr);
		Pjv_t Pjv = JL_LEAF3VALUEAREA(Pjll, Pop1);
		for (i = 0; i < Pop1; i++, Pjll += 3) {
			uint32_t idx = 0;
			JL_COPY3_PINDEX_TO_LONG(idx, Pjll);
			Index = (prefix & 0xff000000) | idx;
			ret = fn(ctx, Index | idx, (void **)(Pjv + i));
			if (ret != 0)
				return ret;
		}
		return 0;
	}
	case cJL_JPLEAF_B1:
	{
		BITMAPB_t BitMap, BitMask;
		Pjlb_t Pjlb = P_JLB(Pjp->jp_Addr);
		Pjv_t Pjv, Pjv2;

		prefix = JL_SETINDEX(prefix, 3, Pjp->jp_DcdP0[0]);
		prefix = JL_SETINDEX(prefix, 2, Pjp->jp_DcdP0[1]);

		for (i = 0; i < cJL_NUMSUBEXPB; i++) {
			BitMap = JL_JLB_BITMAP(Pjlb, i);
			Pjv = P_JV(JL_JLB_PVALUE(Pjlb, i));
			for (mask = 0; mask < cJL_BITSPERSUBEXPB; mask++) {
				BitMask = 1 << mask;
				if (!(BitMap & (BitMask)))
					continue;
				Pjv2 = Pjv + judyCountBits(BitMap & (BitMask - 1));

				Index = (prefix & 0xffffff00) |
					(i * cJL_BITSPERSUBEXPB + mask);
				ret = fn(ctx, Index, (void **)Pjv2);
				if (ret != 0)
					return ret;
			}
		}
		return 0;
	}
	case cJL_JPIMMED_1_01: case cJL_JPIMMED_2_01: case cJL_JPIMMED_3_01:
		Index = (prefix & 0xff000000) | JL_JPDCDPOP0(Pjp);
		return fn(ctx, Index, (void **)(&Pjp->jp_Addr));
	case cJL_JPIMMED_1_03:
		prefix &= 0xffffff00;
		Index = prefix | ((uint8_t *)Pjp->jp_LIndex)[0];
		if ((ret = fn(ctx, Index, (void **)(P_JV(Pjp->jp_Addr) + 0))) != 0)
			return ret;
		Index = prefix | ((uint8_t *)Pjp->jp_LIndex)[1];
		if ((ret = fn(ctx, Index, (void **)(P_JV(Pjp->jp_Addr) + 1))) != 0)
			return ret;
		Index = prefix | ((uint8_t *)Pjp->jp_LIndex)[2];
		return fn(ctx, Index, (void **)(P_JV(Pjp->jp_Addr) + 2));
	case cJL_JPIMMED_1_02:
		prefix &= 0xffffff00;
		Index = prefix | ((uint8_t *)Pjp->jp_LIndex)[0];
		if ((ret = fn(ctx, Index, (void **)(P_JV(Pjp->jp_Addr) + 0))) != 0)
			return ret;
		Index = prefix | ((uint8_t *)Pjp->jp_LIndex)[1];
		return fn(ctx, Index, (void **)(P_JV(Pjp->jp_Addr) + 1));
	default:
		assert("impossible to reach here" && 0);
	}

	return 0;
}

int JudyLWalk(void *PArray, walk_fn_t fn, void *ctx)
{
	Pjpm_t Pjpm = NULL;
	if (JL_LEAFW_POP0(PArray) < cJL_LEAFW_MAXPOP1) {
		int i = 0, ret;
		Pjlw_t Pjlw = P_JLW(PArray);
		Word_t pop1 = Pjlw[0] + 1;
		Pjv_t Pjv = JL_LEAFWVALUEAREA(Pjlw, pop1);

		Pjlw++;
		for (i = 0; i < pop1; i++) {
			ret = fn(ctx, Pjlw[i], (void **)(Pjv + i));
			if (ret != 0)
				return ret;
		}
		return 0;
	}

	Pjpm = P_JPM(PArray);
	return JudyWalk(&Pjpm->jpm_JP, 0, fn, ctx);
}
