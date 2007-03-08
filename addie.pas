{/= Addie ============================================================\}
{                                                                      }
{ Implements an Adaptive Digital Element (ADDIE).                      }
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

//-- @abstract(Implements  the  ADDIE  as  described  in the accompanied
//--           PDF.)
unit
   Addie;


interface


uses
   PRNG;


type
   //-- @abstract(The ADDIE's possible range of counter sizes.)
   tAddie_Bits = 2 .. 32;

type
   //-- @abstract(ADDIE implementation.)
   tAddie = class (tObject)
   public
      {/= Create =====================================================\}
      {                                                                }
      {\==============================================================/}
      //-- @abstract(Creates object and initializes its internal state.)
      //-- @param(Random_Generator   Class pointer to the PRNG to use.)
      //-- @param(Bits               The   resolution  of  the  internal
      //--                           counter.@br
      //--                           The  more  bits  you  use, the more
      //--                           precision  you  will  have, but the
      //--                           more cycles you will wait until the
      //--                           values finally start resembling the
      //--                           input stream.)
      constructor Create (const Random_Generator : PRNG.PRNG_Class;
                          const Bits             : tAddie_Bits = 8);

      {/= Destroy ====================================================\}
      {                                                                }
      {\==============================================================/}
      //-- @abstract(Destroys object and all internally used ones.)
      destructor Destroy; override;

      {/= Feed =======================================================\}
      {                                                                }
      {\==============================================================/}
      //-- @abstract(Feeds a new bit into the ADDIE.)
      //-- @param(Stream_Bit   The bit to be fed to the stream.)
      procedure Feed (const Stream_Bit : Boolean); virtual;

      {/= Probability ================================================\}
      {                                                                }
      {\==============================================================/}
      //-- @abstract(Returns the current probability of the input stream
      //--           being @code(True).)
      //-- @returns(The probability between @code(0.0) and @code(1.0).)
      function Probability : Double; virtual;

      {/= Reset ======================================================\}
      {                                                                }
      {\==============================================================/}
      //-- @abstract(Resets the internal counter to prepare for new data
      //--           stream.)
      procedure Reset; virtual;

   protected
      //-- @abstract(Current (binary) count.)
      Count    : DWord;

      //-- @abstract(The bit depth.)
      Num_Bits : tAddie_Bits;

   public
      //-- @abstract(Returns the current counter.)
      //-- Which is the same as @link(Probability), but in raw internal
      //-- binary notation.
      property Counter : DWord read Count;
      
      //-- @abstract(Returns the ADDIE's bit depth.)
      property Bits : tAddie_Bits read Num_Bits;

   protected
      // Internal state keeping stuff.

      // These values are initialized according to given number of bits.
      //-- @abstract(Mask off unneeded bits.)
      Count_Mask : DWord;
      //-- @abstract(50% counter.)
      Initial_State : DWord;
      //-- @abstract(Needed  to calculate @link(Probability) from @link(
      //--           Count).)
      Scaler : Double;

      //-- @abstract(The internal random stream object.)
      PRNG : PRNG.tPRNG;
   end {tAddie};


implementation


{/= tAddie.Create ====================================================\}
{                                                                      }
{\====================================================================/}
constructor tAddie.Create (const Random_Generator : PRNG.PRNG_Class;
                           const Bits             : tAddie_Bits);
begin
   inherited Create;

   // set up internal state variables
   self.Num_Bits      := Bits;
   self.Count_Mask    := (QWord(1) shl Bits) - 1;
   self.Initial_State := (QWord(1) shl Bits) div 2; // about 50%
   self.Scaler        := 1.0 / self.Count_Mask;

   self.Reset;
   self.PRNG := Random_Generator.Create;
end {tAddie.Create};


{/= tAddie.Destroy ===================================================\}
{                                                                      }
{\====================================================================/}
destructor tAddie.Destroy;
begin
   self.PRNG.Free;
   inherited Destroy;
end {tAddie.Destroy};


{/= tAddie.Feed ======================================================\}
{                                                                      }
{ core algorithm...                                                    }
{                                                                      }
{\====================================================================/}
procedure tAddie.Feed (const Stream_Bit : Boolean);
var
   Counter_Bit : Boolean;
begin
   // compare a new value from the PRNG with the current counter
   Counter_Bit := (self.Count     and self.Count_Mask) >=
                  (self.PRNG.Next and self.Count_Mask);

   // Now xor  the  result  with the given Stream_Bit to see if they are
   // different and adjust the counter accordingly.
   if Counter_Bit xor Stream_Bit then
   begin
      if Counter_Bit then
      begin
         // Due to the fact that the counter bit is set on equality too,
         // we  have to protect for underflows, overflows can not happen
         // by definition.
         if self.Count > 0 then
            self.Count := self.Count - 1
      end {if}
      else
         self.Count := self.Count + 1;
   end {if};
end {tAddie.Feed};


{/= tAddie.Probability ================================================\}
{                                                                       }
{\=====================================================================/}
function tAddie.Probability : Double;
begin
   exit (self.Count * self.Scaler);
end {tAddie.Probability};


{/= tAddie.Reset =====================================================\}
{                                                                      }
{\====================================================================/}
procedure tAddie.Reset;
begin
   self.Count := self.Initial_State;
end {tAddie.Reset};


end {tAddie}.

$Id$

