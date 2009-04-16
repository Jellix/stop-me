{/= LFSR =============================================================\}
{                                                                      }
{ Implements a Linear Feed Back Shift Register.                        }
{ Part of the Stop-Me (STOchastic Performance MEasurement) project.    }
{                                                                      }
{ Copyright (C) 2005, 2006 by Johnny L. Fencey                         }
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
{$MODE OBJFPC}

//-- @abstract(Implements a 64 bit linear feed back shift register.)
unit
   LFSR;


interface


uses
   PRNG;


const
   //-- @abstract(The default seed value.)
   //-- NOTE: due  to using XORs, 0 is not allowed as seed, because this
   //--       is  the lockup state of the LFSR based PRNG.
   DEFAULT_SEED = not (1 shl 63);    // all ones set
   //DEFAULT_SEED = $5555555555555555; // alternating (50%) bit pattern

type
   //-- @abstract(The 64 Bit LFSR object.)
   tRandom_64 = class (PRNG.tPRNG)
   public
      {= Create ======================================================\}
      {                                                                }
      {\==============================================================/}
      //-- @abstract(Creates  the LFSR based PRNG and calls @link(Reset)
      //--           on it.)
      constructor Create; override;

      {/= Next =======================================================\}
      {                                                                }
      {\==============================================================/}
      //-- @abstract(Provides the next PRN in the sequence.)
      //-- @returns(The next Pseudo Random Number of the sequence.)
      function Next : QWord; override;

      {/= Reset ======================================================\}
      {                                                                }
      {\==============================================================/}
      //-- @abstract(Resets the PRNG.)
      //-- @param(Seed   The initial seed value.)
      //-- @raises(ERangeError   When called with @code(Seed = 0).)
      procedure Reset (const Seed : QWord = DEFAULT_SEED); override;
   protected
      //-- @abstract(Stores the last sequence's value returned.)
      Last : QWord;
   end {tRandom_64};


implementation


uses
   SysUtils;

const
   MODULE_PREFIX = 'LFSR.';


{/= tRandom_64.Create ================================================\}
{                                                                      }
{\====================================================================/}
constructor tRandom_64.Create;
begin
   self.Reset;
end {tRandom_64.Create};


{/= tRandom_64.Next ==================================================\}
{                                                                      }
{\====================================================================/}
function tRandom_64.Next : QWord;
// see Xilinx XAPP052 for information how I knew what to do
var
   XOR_Taps : DWord; // nasty optimization to reduce register usage
                     // because the taps are all in the low 32 bits
                     // may fire back on 64 bit targets
begin
   XOR_Taps := self.Last and $FFFFFFFF;
   // 64 bit shift register with taps at stages 64, 63, 61, and 60
   // which are in conventional notation bits 0, 1, 3, and 4
   XOR_Taps := ((XOR_Taps      ) and 1) xor
               ((XOR_Taps shr 1) and 1) xor
               ((XOR_Taps shr 3) and 1) xor
               ((XOR_Taps shr 4) and 1);

   // shift up and in
   self.Last := (QWord(XOR_Taps) shl 63) or (self.Last shr 1);

   exit (self.Last - 1); // so returned range will be 0 .. $FF...FE
end {tRandom_64.Next};


{/= tRandom_64.Reset =================================================\}
{                                                                      }
{\====================================================================/}
procedure tRandom_64.Reset (const Seed : QWord);
const
   PROC_NAME = 'Reset';
begin
   if Seed = 0 then
      raise SysUtils.ERangeError.Create (
                  MODULE_PREFIX  +
                  self.ClassName +
                  '.'            +
                  PROC_NAME      +
                  ': Lockup state seeded!');

   self.Last := Seed;
end {tRandom_64.Reset};


end {LFSR}.

$Id$

