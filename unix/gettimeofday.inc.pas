{/= GetTimeOfDay syscall =============================================\}
{                                                                      }
{ Provides the syscall to gettimeofday() either directly or via libc.  }
{ Part of the Stop-Me (STOchastic Performance MEasurement) project.    }
{                                                                      }
{ Copyright (C) 2007 by Johnny L. Fencey                               }
{                                                                      }
{ This program is free software; you can redistribute it and/or modify }
{ it under the terms of the GNU General Public License as published by }
{ the  Free  Software  Foundation; either version 2 of the License, or }
{ (at your option) any later version.                                  }
{                                                                      }
{ This  program is distributed in the hope that it will be useful, but }
{ WITHOUT  ANY  WARRANTY;   without   even  the  implied  warranty  of }
{ MERCHANTABILITY  or  FITNESS  FOR  A  PARTICULAR  PURPOSE.   See the }
{ GNU General Public License for more details.                         }
{                                                                      }
{ You  should  have  received a copy of the GNU General Public License }
{ along with this program; if not, write to the                        }
{ Free Software Foundation, Inc.                                       }
{ 51 Franklin St, Fifth Floor                                          }
{ Boston, MA  02110-1301                                               }
{ USA                                                                  }
{                                                                      }
{ The original author can be reached via                               }
{                                                                      }
{ e-mail    : c.sucks@jlfencey.com                                     }
{                                                                      }
{ snail-mail: Mr. Vinzent Hoefler                                      }
{             Drangsalengaessli 5                                      }
{             CH-3360 Herzogenbuchsee                                  }
{             Switzerland, Europe, Earth, Solar System, Milky Way      }
{                                                                      }
{ Special proprietary licenses may be available.                       }
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
