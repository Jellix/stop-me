{/= Test_Addie =======================================================\}
{                                                                      }
{ Small test application for the ADDIE.                                }
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

program
   Test_Addie;


uses
   Addie,
   LFSR,
   SysUtils;


{/= Converge_Test ====================================================\}
{                                                                      }
{ prints  out  number of cycles needed after which the addie seemed to }
{ first reach a "close to target value" state                          }
{                                                                      }
{\====================================================================/}
procedure Converge_Test (const Addie_Bits : Addie.tAddie_Bits);
var
   i : LongInt;
   A : Addie.tAddie;
begin
   A := Addie.tAddie.Create (LFSR.tRandom_64, Addie_Bits);
   A.Reset;
   i := 0;

   Write (StdOut, 'Trying to converge ADDIE...');

   while A.Probability < 0.90 do
   begin
      A.Feed (True);
      i := i + 1;
   end {while};

   A.Free;

   WriteLn (StdOut, 'converged after ', i, ' cycles.');
   ReadLn;
end {Converge_Test};


const
   DEFAULT_ADDIE_BITS = 8;

const
   P_THRESHOLD = 0.54322; // should result in p = 0.45678
//   P_THRESHOLD = 0.50000; // should result in p = 0.50000
   AVG_BLOCK   = 256;

var
   Addie_Bits : Addie.tAddie_Bits;
   A1         : Addie.tAddie;
   i          : LongInt;
   Avg        : Double; // block based average
//   M_Avg      : Double; // moving average

begin // Test_Addie
   try
      {$WARNINGS OFF} // this is what we do
      Addie_Bits := SysUtils.StrToInt (ParamStr (1));
      {$WARNINGS ON}
   except
      WriteLn (StdErr, '"' + ParamStr (1) + '"',
                       ' is not a valid bit size specification!');
      WriteLn (StdErr, 'Defaulting to ', DEFAULT_ADDIE_BITS, '.');

      Addie_Bits := DEFAULT_ADDIE_BITS;
   end {try};

   WriteLn (StdOut, 'ADDIE''s size set to ', Addie_Bits, '.');

//   Converge_Test (Addie_Bits);
   ReadLn;

   A1 := Addie.tAddie.Create (LFSR.tRandom_64, ADDIE_BITS);

   // loop around
   while True do
   begin
      Avg   := 0.0;
//      M_Avg := 0.0;

      for i := 1 to AVG_BLOCK do
      begin
         A1.Feed (Random > P_THRESHOLD);
         Avg   := Avg + A1.Probability;
//         M_Avg := M_Avg * 0.5 + A1.Probability * 0.5;
      end {for};

      Avg := Avg / AVG_BLOCK;

      Write (
               Avg            * 100.0 : 9 : 5,
//               M_Avg          * 100.0 : 9 : 5,
               A1.Probability * 100.0 : 9 : 5,
               SysUtils.IntToHex (A1.Counter, 2 * SizeOf (A1.Counter)) : 9,
               LineEnding
              );
   end {while};

   A1.Free;
end {Test_Addie}.

$Id$

