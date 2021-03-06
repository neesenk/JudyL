#include <stdio.h>
#include "JudyL.h"

#define JL_SETINDEX(INDEX, State, V)			\
	(((INDEX) & ~(cJL_MASKATSTATE(State))) |	\
	(((uint8_t)(V)) << (((State) - 1) * cJL_BITSPERBYTE)))

#define JL_GETINDEX(INDEX, State)			\
	(((INDEX) >> (((State) - 1) * cJL_BITSPERBYTE)) & 0xff)

static int JudyWalkRang(Pjp_t Pjp, uint32_t prefix, uint32_t beg,
			uint32_t end, walk_fn_t fn, void *ctx)
{
	int i, mask, ret = 0, state = 0;
	uint32_t Index = 0;
	int b = 0, e = 0, v = 0;
#define J_WR(_j, _p, _b, _e) do {	\
	if ((ret = JudyWalkRang(_j,_p,_b,_e,fn,ctx)) != 0) return ret;	\
} while (0)
#define J_FN(_i, _k) do {		\
	if ((ret = fn(ctx, _i, _k)) != 0) return ret;			\
} while (0)
#define CHECK_RANG(v) do {			\
	if ((v) < b || (v) > e) return 0;	\
	if ((v) > b) beg = 0;			\
	if ((v) < e) end = ~0;			\
} while (0)
	switch (JL_JPTYPE(Pjp)) {
	case cJL_JPNULL1: case cJL_JPNULL2: case cJL_JPNULL3:
		return 0;
	case cJL_JPBRANCH_L2:
		b = JL_GETINDEX(beg, 3);
		e = JL_GETINDEX(end, 3);
		CHECK_RANG(Pjp->jp_DcdP0[0]);

		state = 2;
		prefix = JL_SETINDEX(prefix, 3, Pjp->jp_DcdP0[0]);
		goto JPBRANCH_L;
	case cJL_JPBRANCH_L3:
		state = 3;
		goto JPBRANCH_L;
	case cJL_JPBRANCH_L:
		state = cJL_ROOTSTATE;
		goto JPBRANCH_L;
	JPBRANCH_L: {
		Pjbl_t Pjbl;
		Pjbl = P_JBL(Pjp->jp_Addr);
		b = JL_GETINDEX(beg, state);
		e = JL_GETINDEX(end, state);
		for (i = 0; i < Pjbl->jbl_NumJPs; i++) {
			if (Pjbl->jbl_Expanse[i] >= b)
				break;
		}

		if (i < Pjbl->jbl_NumJPs && Pjbl->jbl_Expanse[i] == b) {
			Index = JL_SETINDEX(prefix, state, Pjbl->jbl_Expanse[i]);
			if (b == e)
				return JudyWalkRang(Pjbl->jbl_jp + i, Index, beg, end, fn, ctx);

			J_WR(Pjbl->jbl_jp + i, Index, beg, ~0);
			i++;
		}

		for (; i < Pjbl->jbl_NumJPs && Pjbl->jbl_Expanse[i] < e; i++) {
			Index = JL_SETINDEX(prefix, state, Pjbl->jbl_Expanse[i]);
			J_WR(Pjbl->jbl_jp + i, Index, 0, ~0);
		}

		if (i < Pjbl->jbl_NumJPs && Pjbl->jbl_Expanse[i] == e) {
			Index = JL_SETINDEX(prefix, state, Pjbl->jbl_Expanse[i]);
			return JudyWalkRang(Pjbl->jbl_jp + i, Index, 0, end, fn, ctx);
		}

		return 0;
	}
	case cJL_JPBRANCH_B2:
		b = JL_GETINDEX(beg, 3);
		e = JL_GETINDEX(end, 3);
		CHECK_RANG(Pjp->jp_DcdP0[0]);

		state = 2;
		prefix = JL_SETINDEX(prefix, 3, Pjp->jp_DcdP0[0]);
		goto JPBRANCH_B;
	case cJL_JPBRANCH_B3:
		state = 3;
		goto JPBRANCH_B;
	case cJL_JPBRANCH_B:
		state = cJL_ROOTSTATE;
		goto JPBRANCH_B;
	JPBRANCH_B: {
		Pjbb_t Pjbb;
		BITMAPB_t BitMap, BitMask;
		Pjp_t Pjp2, Pjp3;
		b = JL_GETINDEX(beg, state);
		e = JL_GETINDEX(end, state);

		Pjbb = P_JBB(Pjp->jp_Addr);

		i = b / cJL_BITSPERSUBEXPB;
		mask = b % cJL_BITSPERSUBEXPB;
		BitMap = JL_JBB_BITMAP(Pjbb, i);
		Pjp2 = P_JP(JL_JBB_PJP(Pjbb, i));
		BitMask = 1 << mask;
		if (BitMap & BitMask) {
			Index = JL_SETINDEX(prefix, state, b);
			Pjp3 = Pjp2 + judyCountBits(BitMap & (BitMask - 1));
			if (b == e)
				return JudyWalkRang(Pjp3, Index, beg, end, fn, ctx);
			J_WR(Pjp3, Index, beg, ~0);
		}

		for (v = b + 1; v < e; v++) {
			i = v / cJL_BITSPERSUBEXPB;
			mask = v % cJL_BITSPERSUBEXPB;
			BitMask = 1 << mask;
			BitMap = JL_JBB_BITMAP(Pjbb, i);
			Pjp2 = P_JP(JL_JBB_PJP(Pjbb, i));
			if (!(BitMap & (BitMask)))
				continue;
			Pjp3 = Pjp2 + judyCountBits(BitMap & (BitMask - 1));

			Index = JL_SETINDEX(prefix, state, v);
			J_WR(Pjp3, Index, 0, ~0);
		}

		i = e / cJL_BITSPERSUBEXPB;
		mask = e % cJL_BITSPERSUBEXPB;
		BitMap = JL_JBB_BITMAP(Pjbb, i);
		Pjp2 = P_JP(JL_JBB_PJP(Pjbb, i));
		BitMask = 1 << mask;
		if (BitMap & BitMask) {
			Pjp3 = Pjp2 + judyCountBits(BitMap & (BitMask - 1));
			Index = JL_SETINDEX(prefix, state, e);
			return JudyWalkRang(Pjp3, Index, 0, end, fn, ctx);
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
		b = JL_GETINDEX(beg, 3);
		e = JL_GETINDEX(end, 3);
		CHECK_RANG(Pjp->jp_DcdP0[0]);

		state = 2;
		prefix = JL_SETINDEX(prefix, 3, Pjp->jp_DcdP0[0]);
		goto JPBRANCH_U;
	JPBRANCH_U: {
		Pjbu_t Pjbu = P_JBU(Pjp->jp_Addr);
		b = JL_GETINDEX(beg, state);
		e = JL_GETINDEX(end, state);

		if (!(Pjbu->jbu_jp[b].jp_Type >= cJL_JPNULL1 &&
		      Pjbu->jbu_jp[b].jp_Type <= cJL_JPNULLMAX)) {
			Index = JL_SETINDEX(prefix, state, b);
			if (b == e)
				return JudyWalkRang(Pjbu->jbu_jp + b, Index, beg, end, fn, ctx);
			J_WR(Pjbu->jbu_jp + b, Index, beg, ~0);
		}

		for (i = b + 1; i < e; i++) {
			if ((Pjbu->jbu_jp[i].jp_Type) >= cJL_JPNULL1
			    && (Pjbu->jbu_jp[i].jp_Type) <= cJL_JPNULLMAX)
				continue;

			Index = JL_SETINDEX(prefix, state, i);
			J_WR(Pjbu->jbu_jp + i, Index, 0, ~0);
		}

		if (!(Pjbu->jbu_jp[e].jp_Type >= cJL_JPNULL1 &&
		      Pjbu->jbu_jp[e].jp_Type <= cJL_JPNULLMAX)) {
			Index = JL_SETINDEX(prefix, state, e);
			return JudyWalkRang(Pjbu->jbu_jp + e, Index, 0, end, fn, ctx);
		}

		return 0;
	}
	case cJL_JPLEAF1: {
		Word_t Pop1 = JL_JPLEAF_POP0(Pjp) + 1;
		uint8_t *Pjll = (uint8_t *)Pjp->jp_Addr;
		Pjv_t Pjv = JL_LEAF1VALUEAREA(Pjll, Pop1);

		v = (Pjp->jp_DcdP0[0]<<8) | Pjp->jp_DcdP0[1];
		b = (beg >> 8) & 0xffff;
		e = (end >> 8) & 0xffff;
		CHECK_RANG(v);

		b = beg & 0xff;
		e = end & 0xff;
		prefix = JL_SETINDEX(prefix, 3, Pjp->jp_DcdP0[0]);
		prefix = JL_SETINDEX(prefix, 2, Pjp->jp_DcdP0[1]);
		for (i = 0; i < Pop1; i++) {
			if (Pjll[i] < b)
				continue;
			if (Pjll[i] > e)
				break;
			Index = (prefix & 0xffffff00) | Pjll[i];
			J_FN(Index, (void **)(Pjv + i));
		}

		return 0;
	}
	case cJL_JPLEAF2: {
		Word_t Pop1 = JL_JPLEAF_POP0(Pjp) + 1;
		uint16_t *Pjll = (uint16_t *)Pjp->jp_Addr;
		Pjv_t Pjv = JL_LEAF2VALUEAREA(Pjll, Pop1);
		b = JL_GETINDEX(beg, 3);
		e = JL_GETINDEX(end, 3);
		CHECK_RANG(Pjp->jp_DcdP0[0]);

		prefix = JL_SETINDEX(prefix, 3, Pjp->jp_DcdP0[0]);
		b = beg & 0xffff;
		e = end & 0xffff;
		for (i = 0; i < Pop1; i++) {
			if (Pjll[i] < b)
				continue;
			if (Pjll[i] > e)
				break;
			Index = (prefix & 0xffff0000) | Pjll[i];
			J_FN(Index, (void **)(Pjv + i));
		}
		return 0;
	}
	case cJL_JPLEAF3: {
		Word_t Pop1 = JL_JPLEAF_POP0(Pjp) + 1;
		uint8_t *Pjll = P_JLL(Pjp->jp_Addr);
		Pjv_t Pjv = JL_LEAF3VALUEAREA(Pjll, Pop1);
		b = beg & 0xffffff;
		e = end & 0xffffff;
		for (i = 0; i < Pop1; i++, Pjll += 3) {
			uint32_t idx = 0;
			JL_COPY3_PINDEX_TO_LONG(idx, Pjll);
			if (idx < b)
				continue;
			if (idx > e)
				break;
			Index = (prefix & 0xff000000) | idx;
			J_FN(Index | idx, (void **)(Pjv + i));
		}
		return 0;
	}
	case cJL_JPLEAF_B1: {
		BITMAPB_t BitMap, BitMask;
		Pjlb_t Pjlb = P_JLB(Pjp->jp_Addr);
		Pjv_t Pjv, Pjv2;
		v = (Pjp->jp_DcdP0[0] << 8) | Pjp->jp_DcdP0[1];
		b = (beg >> 8) & 0xffff;
		e = (end >> 8) & 0xffff;
		CHECK_RANG(v);

		prefix = JL_SETINDEX(prefix, 3, Pjp->jp_DcdP0[0]);
		prefix = JL_SETINDEX(prefix, 2, Pjp->jp_DcdP0[1]);

		b = beg & 0xff;
		e = end & 0xff;

		for (v = b; v <= e; v++) {
			i = v / cJL_BITSPERSUBEXPB;
			mask = v % cJL_BITSPERSUBEXPB;
			BitMask = 1 << mask;
			BitMap = JL_JLB_BITMAP(Pjlb, i);
			Pjv = P_JV(JL_JLB_PVALUE(Pjlb, i));

			if (!(BitMap & (BitMask)))
				continue;

			Pjv2 = Pjv + judyCountBits(BitMap & (BitMask - 1));

			Index = (prefix & 0xffffff00) | v;
			J_FN(Index, (void **)Pjv2);
		}

		return 0;
	}
	case cJL_JPIMMED_1_01: case cJL_JPIMMED_2_01: case cJL_JPIMMED_3_01:
		v = JL_JPDCDPOP0(Pjp);
		b = beg & 0xffffff;
		e = end & 0xffffff;
		if (v < b || v > e)
			return 0;

		Index = (prefix & 0xff000000) | v;
		return fn(ctx, Index, (void **)(&Pjp->jp_Addr));
	case cJL_JPIMMED_1_03:
#define WALK_JPIMMED(pos) do {				\
	v = ((uint8_t *)Pjp->jp_LIndex)[pos];		\
	if (v >= b && v <= e) {				\
		Index = prefix | v;			\
		J_FN(Index, (void **)(P_JV(Pjp->jp_Addr) + pos)); \
	}						\
} while (0)
		b = beg & 0xff;
		e = end & 0xff;
		prefix &= 0xffffff00;

		WALK_JPIMMED(0);
		WALK_JPIMMED(1);
		WALK_JPIMMED(2);
		return 0;
	case cJL_JPIMMED_1_02:
		b = beg & 0xff;
		e = end & 0xff;

		prefix &= 0xffffff00;
		WALK_JPIMMED(0);
		WALK_JPIMMED(1);
		return 0;
	default:
		assert("Impossible!!!" && 0);
	}

	return 0;
}

int JudyLWalkRang(void *PArray, uint32_t beg, uint32_t end, walk_fn_t fn, void *ctx)
{
	Pjpm_t Pjpm = NULL;

	assert(beg <= end);
	if (JL_LEAFW_POP0(PArray) < cJL_LEAFW_MAXPOP1) {
		int i = 0, ret;
		Pjlw_t Pjlw = P_JLW(PArray);
		Word_t pop1 = Pjlw[0] + 1;
		Pjv_t Pjv = JL_LEAFWVALUEAREA(Pjlw, pop1);

		Pjlw++;
		for (i = 0; i < pop1; i++) {
			if (Pjlw[i] < beg)
				continue;
			if (Pjlw[i] > end)
				break;
			ret = fn(ctx, Pjlw[i], (void **)(Pjv + i));
			if (ret != 0)
				return ret;
		}
		return 0;
	}

	Pjpm = P_JPM(PArray);
	return JudyWalkRang(&Pjpm->jpm_JP, 0, beg, end, fn, ctx);
}

int JudyLWalk2(void *PArray, walk_fn_t fn, void *ctx)
{
	return JudyLWalkRang(PArray, 0, ~0, fn, ctx);
}
