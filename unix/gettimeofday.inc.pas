{/= GetTimeOfDay syscall =============================================\}
{                                                                      }
{ Copyright (C) 2007 by MDC Max Daetwyler AG, Bleienbach, Switzerland  }
{                                                                      }
{\====================================================================/}
// Notes: Syscall (non-LibC version): {$INLINE} must be enabled.
//        LibC-version              : Define USE_LIBC before including.
//
// Also, you need to mention "BaseUnix" in your uses clause.
// Of course, for the Syscall version, the "SysCall" unit must have been
// mentioned there, too.

{$IFNDEF __GET_TIME_OF_DAY_INC_PAS}
{$DEFINE __GET_TIME_OF_DAY_INC_PAS}

{/= GetTimeOfDay =====================================================\}
{                                                                      }
{\====================================================================/}
function GetTimeOfDay (out   Time_Val : BaseUnix.tTimeVal;
                       const Dummy    : Pointer) : BaseUnix.CInt;
//
// Function prototype declaration from "man 2 gettimeofday":
//
//    int gettimeofday (struct timeval*  tv,
//                      struct timezone* tz);
//
{$IFDEF USE_LIBC}
  cdecl; external 'libc' name 'gettimeofday';
{$ELSE}
  inline;
begin
   {$HINTS OFF} // I  fucking *know* that most conversions below are not
                // portable,  so  there  is  no  need  for  the compiler
                // telling  me  that  crap.  After all,  this portion is
                // unportable code by design anyway. ;)
   exit (SysCall.Do_SysCall (SysCall.Syscall_Nr_GetTimeOfDay,
                             SysCall.tSysParam(@Time_Val),
                             SysCall.tSysParam(NIL)));
   {$HINTS ON}
end {GetTimeOfDay};
{$ENDIF USE_LIBC}

{$ENDIF __GET_TIME_OF_DAY_INC_PAS}

// $Id$
