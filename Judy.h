#ifndef _JUDY_INCLUDED
#define _JUDY_INCLUDED
#include <stdint.h>
#include <stdlib.h>  
#include <limits.h>  
#include <string.h>
#include <assert.h>

#ifdef __cplusplus      /* support use by C++ code */
extern "C" {
#endif

// ****************************************************************************
// DECLARE SOME BASE TYPES IN CASE THEY ARE MISSING:
//
// These base types include "const" where appropriate, but only where of
// interest to the caller.  For example, a caller cares that a variable passed
// by reference will not be modified, such as, "const void * Pindex", but not
// that the called function internally does not modify the pointer itself, such
// as, "void * const Pindex".
//
// Note that its OK to pass a Pvoid_t to a Pcvoid_t; the latter is the same,
// only constant.  Callers need to do this so they can also pass & Pvoid_t to
// PPvoid_t (non-constant).

#ifndef _PCVOID_T
#define _PCVOID_T
typedef const void * Pcvoid_t;
#endif

#ifndef _PVOID_T
#define _PVOID_T
typedef void *   Pvoid_t;
typedef void ** PPvoid_t;
#endif

#ifndef _WORD_T
#define _WORD_T
typedef unsigned long Word_t, * PWord_t;  // expect 32-bit or 64-bit words.
#endif

#ifndef NULL
#define NULL 0
#endif


// ****************************************************************************
// SUPPORT FOR ERROR HANDLING:
//
// Judy error numbers:
//
// Note:  These are an enum so theres a related typedef, but the numbers are
// spelled out so you can map a number back to its name.
typedef enum            // uint8_t -- but C does not support this type of enum.
{
// Note:  JL_ERRNO_NONE and JL_ERRNO_FULL are not real errors.  They specify
// conditions which are otherwise impossible return values from 32-bit
// Judy1Count, which has 2^32 + 1 valid returns (0..2^32) plus one error
// return.  These pseudo-errors support the return values that cannot otherwise
// be unambiguously represented in a 32-bit word, and will never occur on a
// 64-bit system.
        JL_ERRNO_NONE           = 0,
        JL_ERRNO_FULL           = 1,
        JL_ERRNO_NFMAX          = JL_ERRNO_FULL,
// JL_ERRNO_NOMEM comes from malloc(3C) when Judy cannot obtain needed memory.
// The system errno value is also set to ENOMEM.  This error can be recoverable
// if the calling application frees other memory.
        JL_ERRNO_NOMEM          = 2,
// Problems with parameters from the calling program:
// JL_ERRNO_NULLPPARRAY means PPArray was null; perhaps PArray was passed where
// &PArray was intended.  Similarly, JL_ERRNO_NULLPINDEX means PIndex was null;
// perhaps &Index was intended.  Also, JL_ERRNO_NONNULLPARRAY,
// JL_ERRNO_NULLPVALUE, and JL_ERRNO_UNSORTED, all added later (hence with
// higher numbers), mean:  A non-null array was passed in where a null pointer
// was required; PValue was null; and unsorted indexes were detected.
        JL_ERRNO_NULLPPARRAY    = 3,    // see above.
        JL_ERRNO_NONNULLPARRAY  = 10,   // see above.
        JL_ERRNO_NULLPINDEX     = 4,    // see above.
        JL_ERRNO_NULLPVALUE     = 11,   // see above.
        JL_ERRNO_NOTJUDY1       = 5,    // PArray is not to a Judy1 array.
        JL_ERRNO_NOTJUDYL       = 6,    // PArray is not to a JudyL array.
        JL_ERRNO_NOTJUDYSL      = 7,    // PArray is not to a JudySL array.
        JL_ERRNO_UNSORTED       = 12,   // see above.
// Errors below this point are not recoverable; further tries to access the
// Judy array might result in EFAULT and a core dump:
// JL_ERRNO_OVERRUN occurs when Judy detects, upon reallocation, that a block
// of memory in its own freelist was modified since being freed.
        JL_ERRNO_OVERRUN        = 8,
// JL_ERRNO_CORRUPT occurs when Judy detects an impossible value in a Judy data
// structure:
// Note:  The Judy data structure contains some redundant elements that support
// this type of checking.
        JL_ERRNO_CORRUPT        = 9
// Warning:  At least some C or C++ compilers do not tolerate a trailing comma
// above here.  At least we know of one case, in aCC; see JAGad58928.
} JL_Errno_t;

// Judy errno structure:
// WARNING:  For compatibility with possible future changes, the fields of this
// struct should not be referenced directly.  Instead use the macros supplied
// below.
// This structure should be declared on the stack in a threaded process.
typedef struct J_UDY_ERROR_STRUCT
{
        JL_Errno_t je_Errno;            // one of the enums above.
        int        je_ErrID;            // often an internal source line number.
        Word_t     je_reserved[4];      // for future backward compatibility.
} JError_t, * PJError_t;

// Related macros:
// Fields from error struct:
#define JL_ERRNO(PJError)  ((PJError)->je_Errno)
#define JL_ERRID(PJError)  ((PJError)->je_ErrID)

// For checking return values from various Judy functions:
// Note:  Define JERR as -1, not as the seemingly more portable (Word_t)
// (~0UL), to avoid a compiler "overflow in implicit constant conversion"
// warning.
#define   JERR (-1)                     /* functions returning int or Word_t */
#define  PJERR ((Pvoid_t)  (~0UL))      /* mainly for use here, see below    */
#define PPJERR ((PPvoid_t) (~0UL))      /* functions that return PPvoid_t    */

// Convenience macro for when detailed error information (PJError_t) is not
// desired by the caller; a purposely short name:
#define PJE0  ((PJError_t) NULL)

// JUDY FUNCTIONS:
// P_JE is a shorthand for use below:
#define P_JE  PJError_t PJError

// JUDYL FUNCTIONS:
extern PPvoid_t JudyLGet(Pcvoid_t, Word_t, P_JE);
extern PPvoid_t JudyLIns(PPvoid_t, Word_t, P_JE);
extern int	JudyLInsArray(PPvoid_t, Word_t, const Word_t *const, const Word_t *const, P_JE);
extern int	JudyLDel(PPvoid_t PPArray, Word_t Index, P_JE);
extern Word_t	JudyLCount(Pcvoid_t PArray, Word_t Index1, Word_t Index2, P_JE);
extern PPvoid_t JudyLByCount(Pcvoid_t PArray, Word_t Count, Word_t *PIndex, P_JE);
extern Word_t   JudyLFreeArray(PPvoid_t PPArray, P_JE);
extern Word_t   JudyLMemUsed(Pcvoid_t PArray);
extern Word_t   JudyLMemActive(Pcvoid_t PArray);
extern PPvoid_t JudyLFirst(Pcvoid_t PArray, Word_t *PIndex, P_JE);
extern PPvoid_t JudyLNext(Pcvoid_t PArray, Word_t *PIndex, P_JE);
extern PPvoid_t JudyLLast(Pcvoid_t PArray, Word_t *PIndex, P_JE);
extern PPvoid_t JudyLPrev(Pcvoid_t PArray, Word_t *PIndex, P_JE);
extern int      JudyLFirstEmpty(Pcvoid_t PArray, Word_t *PIndex, P_JE);
extern int      JudyLNextEmpty(Pcvoid_t PArray, Word_t *PIndex, P_JE);
extern int      JudyLLastEmpty(Pcvoid_t PArray, Word_t *PIndex, P_JE);
extern int      JudyLPrevEmpty(Pcvoid_t PArray, Word_t *PIndex, P_JE);

// JUDY memory interface to malloc() FUNCTIONS:
extern Word_t JudyMalloc(Word_t);               // words reqd => words allocd.
extern Word_t JudyMallocVirtual(Word_t);        // words reqd => words allocd.
extern void   JudyFree(Pvoid_t, Word_t);        // free, size in words.
extern void   JudyFreeVirtual(Pvoid_t, Word_t); // free, size in words.

extern PPvoid_t JudySLGet(       Pcvoid_t, const uint8_t * Index, P_JE);
extern PPvoid_t JudySLIns(       PPvoid_t, const uint8_t * Index, P_JE);
extern int      JudySLDel(       PPvoid_t, const uint8_t * Index, P_JE);
extern Word_t   JudySLFreeArray( PPvoid_t,                        P_JE);
extern PPvoid_t JudySLFirst(     Pcvoid_t,       uint8_t * Index, P_JE);
extern PPvoid_t JudySLNext(      Pcvoid_t,       uint8_t * Index, P_JE);
extern PPvoid_t JudySLLast(      Pcvoid_t,       uint8_t * Index, P_JE);
extern PPvoid_t JudySLPrev(      Pcvoid_t,       uint8_t * Index, P_JE);

extern PPvoid_t JudyHSGet(Pcvoid_t, void *, Word_t);
extern PPvoid_t JudyHSIns(PPvoid_t, void *, Word_t, P_JE);
extern int      JudyHSDel(PPvoid_t, void *, Word_t, P_JE);
extern Word_t   JudyHSFreeArray(PPvoid_t, P_JE);
extern uint32_t JudyHashStr(void *, Word_t);

extern Word_t JudyMalloc(Word_t);               // words reqd => words allocd.
extern Word_t JudyMallocVirtual(Word_t);        // words reqd => words allocd.
extern void   JudyFree(Pvoid_t, Word_t);        // block to free and its size in words.
extern void   JudyFreeVirtual(Pvoid_t, Word_t); // block to free and its size in words.

#define JLAP_INVALID    0x1     /* flag to mark pointer "not a Judy array" */
// MACRO EQUIVALENTS FOR JUDY FUNCTIONS:
//
// The following macros, such as J1T, are shorthands for calling Judy functions
// with parameter address-of and detailed error checking included.  Since they
// are macros, the error checking code is replicated each time the macro is
// used, but it runs fast in the normal case of no error.
//
// If the caller does not like the way the default JUDYERROR macro handles
// errors (such as an exit(1) call when out of memory), they may define their
// own before the "#include <Judy.h>".  A routine such as HandleJudyError
// could do checking on specific error numbers and print a different message
// dependent on the error.  The following is one example:
//
// Note: the back-slashes are removed because some compilers will not accept
// them in comments.
//
// void HandleJudyError(uint8_t *, int, uint8_t *, int, int);
// #define JUDYERROR(CallerFile, CallerLine, JudyFunc, JudyErrno, JudyErrID)
// {
//    HandleJudyError(CallerFile, CallerLine, JudyFunc, JudyErrno, JudyErrID);
// }
//
// The routine HandleJudyError could do checking on specific error numbers and
// print a different message dependent on the error.
//
// The macro receives five parameters that are:
//
// 1.  CallerFile:  Source filename where a Judy call returned a serious error.
// 2.  CallerLine:  Line number in that source file.
// 3.  JudyFunc:    Name of Judy function reporting the error.
// 4.  JudyErrno:   One of the JL_ERRNO* values enumerated above.
// 5.  JudyErrID:   The je_ErrID field described above.
#ifndef JUDYERROR       /* supply a default error macro */
#include <stdio.h>
#define JUDYERROR(CallerFile, CallerLine, JudyFunc, JudyErrno, JudyErrID) \
    {                                                                     \
        (void) fprintf(stderr, "File '%s', line %d: %s(), "               \
           "JL_ERRNO_* == %d, ID == %d\n",                                \
           CallerFile, CallerLine,                                        \
           JudyFunc, JudyErrno, JudyErrID);                               \
        exit(1);                                                          \
    }
#endif /* JUDYERROR */

// Internal shorthand macros for writing the J1S, etc. macros:
#define J_E(FuncName,PJE) \
        JUDYERROR(__FILE__, __LINE__, FuncName, JL_ERRNO(PJE), JL_ERRID(PJE))

#define J_SE(FuncName,Errno)                                            \
        {                                                               \
            JError_t J_Error;                                           \
            JL_ERRNO(&J_Error) = (Errno);                               \
            JL_ERRID(&J_Error) = __LINE__;                              \
            J_E(FuncName, &J_Error);                                    \
        }

// Note:  In each J_*() case below, the digit is the number of key parameters
// to the Judy*() call.  Just assign the Func result to the callers Rc value
// without a cast because none is required, and this keeps the API simpler.
// However, a family of different J_*() macros is needed to support the
// different numbers of key parameters (0,1,2) and the Func return type.
//
// In the names below, "I" = integer result; "P" = pointer result.  Note, the
// Funcs for J_*P() return PPvoid_t, but cast this to a Pvoid_t for flexible,
// error-free assignment, and then compare to PJERR.
#define J_0I(Rc,PArray,Func,FuncName)                                   \
        {                                                               \
            JError_t J_Error;                                           \
            if (((Rc) = Func(PArray, &J_Error)) == JERR)                \
                J_E(FuncName, &J_Error);                                \
        }

#define J_1I(Rc,PArray,Index,Func,FuncName)                             \
        {                                                               \
            JError_t J_Error;                                           \
            if (((Rc) = Func(PArray, Index, &J_Error)) == JERR)         \
                J_E(FuncName, &J_Error);                                \
        }

#define J_1P(Rc,PArray,Index,Func,FuncName)                             \
        {                                                               \
            JError_t J_Error;                                           \
            if (((Rc) = (Pvoid_t) Func(PArray, Index, &J_Error)) == PJERR) \
                J_E(FuncName, &J_Error);                                \
        }

#define J_2I(Rc,PArray,Index,Arg2,Func,FuncName)                        \
        {                                                               \
            JError_t J_Error;                                           \
            if (((Rc) = Func(PArray, Index, Arg2, &J_Error)) == JERR)   \
                J_E(FuncName, &J_Error);                                \
        }

// Variation for Judy*Count functions, which return 0, not JERR, for error (and
// also for other non-error cases):
//
// Note:  JL_ERRNO_NFMAX should only apply to 32-bit Judy1, but this header
// file lacks the necessary ifdefs to make it go away otherwise, so always
// check against it.

#define J_2C(Rc,PArray,Index1,Index2,Func,FuncName)                     \
        {                                                               \
            JError_t J_Error;                                           \
            if ((((Rc) = Func(PArray, Index1, Index2, &J_Error)) == 0)  \
             && (JL_ERRNO(&J_Error) > JL_ERRNO_NFMAX))                  \
            {                                                           \
                J_E(FuncName, &J_Error);                                \
            }                                                           \
        }

#define J_2P(PV,PArray,Index,Arg2,Func,FuncName)                        \
        {                                                               \
            JError_t J_Error;                                           \
            if (((PV) = (Pvoid_t) Func(PArray, Index, Arg2, &J_Error))  \
                == PJERR) J_E(FuncName, &J_Error);                      \
        }

// Variations for Judy*Set/InsArray functions:

#define J_2AI(Rc,PArray,Count,PIndex,Func,FuncName)                     \
        {                                                               \
            JError_t J_Error;                                           \
            if (((Rc) = Func(PArray, Count, PIndex, &J_Error)) == JERR) \
                J_E(FuncName, &J_Error);                                \
        }

#define J_3AI(Rc,PArray,Count,PIndex,PValue,Func,FuncName)              \
        {                                                               \
            JError_t J_Error;                                           \
            if (((Rc) = Func(PArray, Count, PIndex, PValue, &J_Error))  \
                == JERR) J_E(FuncName, &J_Error);                       \
        }

// Some of the macros are special cases that use inlined shortcuts for speed
// with root-level leaves:
// This is a slower version with current processors, but in the future...
#define J1T(Rc,PArray,Index)                                            \
    (Rc) = Judy1Test((Pvoid_t)(PArray), Index, PJE0)
#define J1S( Rc,    PArray,   Index) \
        J_1I(Rc, (&(PArray)), Index,  Judy1Set,   "Judy1Set")
#define J1SA(Rc,    PArray,   Count, PIndex) \
        J_2AI(Rc,(&(PArray)), Count, PIndex, Judy1SetArray, "Judy1SetArray")
#define J1U( Rc,    PArray,   Index) \
        J_1I(Rc, (&(PArray)), Index,  Judy1Unset, "Judy1Unset")
#define J1F( Rc,    PArray,   Index) \
        J_1I(Rc,    PArray, &(Index), Judy1First, "Judy1First")
#define J1N( Rc,    PArray,   Index) \
        J_1I(Rc,    PArray, &(Index), Judy1Next,  "Judy1Next")
#define J1L( Rc,    PArray,   Index) \
        J_1I(Rc,    PArray, &(Index), Judy1Last,  "Judy1Last")
#define J1P( Rc,    PArray,   Index) \
        J_1I(Rc,    PArray, &(Index), Judy1Prev,  "Judy1Prev")
#define J1FE(Rc,    PArray,   Index) \
        J_1I(Rc,    PArray, &(Index), Judy1FirstEmpty, "Judy1FirstEmpty")
#define J1NE(Rc,    PArray,   Index) \
        J_1I(Rc,    PArray, &(Index), Judy1NextEmpty,  "Judy1NextEmpty")
#define J1LE(Rc,    PArray,   Index) \
        J_1I(Rc,    PArray, &(Index), Judy1LastEmpty,  "Judy1LastEmpty")
#define J1PE(Rc,    PArray,   Index) \
        J_1I(Rc,    PArray, &(Index), Judy1PrevEmpty,  "Judy1PrevEmpty")
#define J1C( Rc,    PArray,   Index1,  Index2) \
        J_2C(Rc,    PArray,   Index1,  Index2, Judy1Count,   "Judy1Count")
#define J1BC(Rc,    PArray,   Count,   Index) \
        J_2I(Rc,    PArray,   Count, &(Index), Judy1ByCount, "Judy1ByCount")
#define J1FA(Rc,    PArray) \
        J_0I(Rc, (&(PArray)), Judy1FreeArray, "Judy1FreeArray")
#define J1MU(Rc,    PArray) \
        (Rc) = Judy1MemUsed(PArray)

#define JLG(PV,PArray,Index)                                            \
    (PV) = (Pvoid_t)JudyLGet((Pvoid_t)PArray, Index, PJE0)

#define JLI( PV,    PArray,   Index)                                    \
        J_1P(PV, (&(PArray)), Index,  JudyLIns,   "JudyLIns")

#define JLIA(Rc,    PArray,   Count, PIndex, PValue)                    \
        J_3AI(Rc,(&(PArray)), Count, PIndex, PValue, JudyLInsArray,     \
                                                  "JudyLInsArray")
#define JLD( Rc,    PArray,   Index)                                    \
        J_1I(Rc, (&(PArray)), Index,  JudyLDel,   "JudyLDel")

#define JLF( PV,    PArray,   Index)                                    \
        J_1P(PV,    PArray, &(Index), JudyLFirst, "JudyLFirst")

#define JLN( PV,    PArray,   Index)                                    \
        J_1P(PV,    PArray, &(Index), JudyLNext, "JudyLNext")

#define JLL( PV,    PArray,   Index)                                    \
        J_1P(PV,    PArray, &(Index), JudyLLast,  "JudyLLast")
#define JLP( PV,    PArray,   Index)                                    \
        J_1P(PV,    PArray, &(Index), JudyLPrev,  "JudyLPrev")
#define JLFE(Rc,    PArray,   Index)                                    \
        J_1I(Rc,    PArray, &(Index), JudyLFirstEmpty, "JudyLFirstEmpty")
#define JLNE(Rc,    PArray,   Index)                                    \
        J_1I(Rc,    PArray, &(Index), JudyLNextEmpty,  "JudyLNextEmpty")
#define JLLE(Rc,    PArray,   Index)                                    \
        J_1I(Rc,    PArray, &(Index), JudyLLastEmpty,  "JudyLLastEmpty")
#define JLPE(Rc,    PArray,   Index)                                    \
        J_1I(Rc,    PArray, &(Index), JudyLPrevEmpty,  "JudyLPrevEmpty")
#define JLC( Rc,    PArray,   Index1,  Index2)                          \
        J_2C(Rc,    PArray,   Index1,  Index2, JudyLCount,   "JudyLCount")
#define JLBC(PV,    PArray,   Count,   Index)                           \
        J_2P(PV,    PArray,   Count, &(Index), JudyLByCount, "JudyLByCount")
#define JLFA(Rc,    PArray)                                             \
        J_0I(Rc, (&(PArray)), JudyLFreeArray, "JudyLFreeArray")
#define JLMU(Rc,    PArray)                                             \
        (Rc) = JudyLMemUsed(PArray)

#define JHSI(PV,    PArray,   PIndex,   Count)                          \
        J_2P(PV, (&(PArray)), PIndex,   Count, JudyHSIns, "JudyHSIns")
#define JHSG(PV,    PArray,   PIndex,   Count)                          \
        (PV) = (Pvoid_t) JudyHSGet(PArray, PIndex, Count)
#define JHSD(Rc,    PArray,   PIndex,   Count)                          \
        J_2I(Rc, (&(PArray)), PIndex, Count, JudyHSDel, "JudyHSDel")
#define JHSFA(Rc,    PArray)                                            \
        J_0I(Rc, (&(PArray)), JudyHSFreeArray, "JudyHSFreeArray")

#define JSLG( PV,    PArray,   Index)                                   \
        J_1P( PV,    PArray,   Index, JudySLGet,   "JudySLGet")
#define JSLI( PV,    PArray,   Index)                                   \
        J_1P( PV, (&(PArray)), Index, JudySLIns,   "JudySLIns")
#define JSLD( Rc,    PArray,   Index)                                   \
        J_1I( Rc, (&(PArray)), Index, JudySLDel,   "JudySLDel")
#define JSLF( PV,    PArray,   Index)                                   \
        J_1P( PV,    PArray,   Index, JudySLFirst, "JudySLFirst")
#define JSLN( PV,    PArray,   Index)                                   \
        J_1P( PV,    PArray,   Index, JudySLNext,  "JudySLNext")
#define JSLL( PV,    PArray,   Index)                                   \
        J_1P( PV,    PArray,   Index, JudySLLast,  "JudySLLast")
#define JSLP( PV,    PArray,   Index)                                   \
        J_1P( PV,    PArray,   Index, JudySLPrev,  "JudySLPrev")
#define JSLFA(Rc,    PArray)                                            \
        J_0I( Rc, (&(PArray)), JudySLFreeArray, "JudySLFreeArray")

#ifdef __cplusplus
}
#endif
#endif /* ! _JUDY_INCLUDED */
