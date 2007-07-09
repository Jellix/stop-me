{/= Letter_Count =====================================================\}
{                                                                      }
{ Usage example.                                                       }
{ Counts letters in a file and outputs their (average) probability.    }
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
{$MODE OBJFPC}

// This  example  program takes a file as argument as counts all letters
// A-Z (case-insensitive) and feeds an ADDIE with the gathered data.
// Additionally it provides information about the distribution of vocals
// in the given text.
// This  is  not  a  too senseful application,  as the output is just an
// average,  meaning,  if  the text would consist of 'A's and 'C's only,
// the  result  would  simply  converge to the same value as if the text
// would contain 'B's only.
// Still it outputs some nice looking curves. ;)
program
   Letter_Count;

uses
   Addie,
   Classes,
   FPImage,
   FPWritePNG,
   LFSR,
   Perf_Image,
   Perf_Measure;


const
   // ADDIE's resolution.  Five bits should even be enough, we have less
   // than 32 letters.
   ADDIE_BITS = 8;

type
   // Index type for chars considered.
   Letter_Index = 'A' .. 'Z';

const
   // Set constants.
   LETTERS : set of Char =
     [Low (Letter_Index) .. High (Letter_Index)];
   VOCALS  : set of Char =
     ['A', 'E', 'I', 'O', 'U'];

const
   // We  have  26  letters being counted,  so also use 26 bits only for
   // each letter counted.
   BIT_COUNT = 26;

const
   // Bit streams for letter counter. Simply a number of unique set bits
   // for each letter considered.
   // As  is,  BIT_COUNT  bits per letter.  Should be dispersed a little
   // more.
   Letter_Bits : array[Letter_Index] of LongInt =
   ($00000001, $00000003, $00000007, $0000000F, // A - D
    $0000001F, $0000003F, $0000007F, $000000FF, // E - H
    $000001FF, $000003FF, $000007FF, $00000FFF, // I - L
    $00001FFF, $00003FFF, $00007FFF, $0000FFFF, // M - P
    $0001FFFF, $0003FFFF, $0007FFFF, $000FFFFF, // Q - T
    $001FFFFF, $003FFFFF, $007FFFFF, $00FFFFFF, // U - X
    $01FFFFFF, $03FFFFFF                        // Y - Z
   );

var
   Letter_Counter : Addie.tAddie;
   Letter_Sample  : Classes.tStream;

var
   Vocal_Counter  : Addie.tAddie;
   Vocal_Sample   : Classes.tStream;

var
   Data    : Classes.tMemoryStream;
   Imager  : Perf_Image.Performance_Graph;
   Image   : FPImage.tFPCustomImage;
   Bit_Set : LongInt;
   x       : Perf_Measure.Data_Sample;
   i       : LongInt;
   j       : Byte;
   c       : Char;

begin
   Data := Classes.tMemoryStream.Create;
   Data.LoadFromFile (ParamStr (1));

   Letter_Counter := Addie.tAddie.Create (LFSR.tRandom_64, ADDIE_BITS);
   Letter_Sample  := Classes.tMemoryStream.Create;

   Vocal_Counter  := Addie.tAddie.Create (LFSR.tRandom_64, ADDIE_BITS);
   Vocal_Sample   := Classes.tMemoryStream.Create;

   try
      for i := 0 to Pred (Data.Size) do
      begin
         Data.Read (c, SizeOf (c)); // Assume 1 byte chars.
         c := UpCase (c);

         // Always  feed  32 bits.  How many bits are set depends on the
         // letter detected. Ignore other chars.
         if c in LETTERS then
         begin
            Bit_Set := Letter_Bits[c];

            for j := 0 to Pred (BIT_COUNT) do
               Letter_Counter.Feed ((Bit_Set and (1 shl j)) <> 0);

            Vocal_Counter.Feed (c in VOCALS);
         end {if};

         x := Letter_Counter.Probability;
         Letter_Sample.Write (x, SizeOf (x));

         x := Vocal_Counter.Probability;
         Vocal_Sample.Write (x, SizeOf (x));
      end {for};

      Imager := Perf_Image.Performance_Graph.Create;

      try
         Imager.Create_Image (Letter_Sample, ADDIE_BITS, Image);

         try
            Image.SaveToFile ('letters.png');
         finally
            Image.Free;
         end {try};

         Imager.Create_Image (Vocal_Sample, ADDIE_BITS, Image);

         try
            Image.SaveToFile ('vocals.png');
         finally
            Image.Free;
         end {try};
      finally
         Imager.Free;
      end {try};
   finally
      Vocal_Sample.Free;
      Vocal_Counter.Free;

      Letter_Sample.Free;
      Letter_Counter.Free;
   end {try};
end {Letter_Count}.

$Id$
