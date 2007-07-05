{/= Test_Image =======================================================\}
{                                                                      }
{ Test application for the performance graph image generation.         }
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

program
   Test_Image;

uses
   {$IFDEF UNIX} CThreads, {$ENDIF UNIX}
   Addie,
   Calendar,
   Classes,
   FPImage,
   FPWritePNG,
   LFSR,
   Perf_Image,
   SysUtils;

const
   ADDIE_BITS  = 10;
   DATA_POINTS = 1000;

procedure Run;
var
   My_Addie   : Addie.tAddie;
   Image      : FPImage.tFPCustomImage;
   Img_Writer : FPImage.tFPCustomImageWriter;
   Perf_Img   : Perf_Image.Performance_Graph;
   i          : Integer;
begin
   My_Addie := Addie.tAddie.Create (LFSR.tRandom_64, ADDIE_BITS);
   Perf_Img := Perf_Image.Performance_Graph.Create (My_Addie);

   try
      Perf_Img.Start (Calendar.Milliseconds (10));

      for i := 0 to Pred (DATA_POINTS) do
      begin
         My_Addie.Feed (Random > 0.9);
         SysUtils.Sleep (1);
      end {for};

      Perf_Img.Stop;

      // Finally create the .png image.
      Perf_Img.Create_Image (Image);
      Img_Writer := FPWritePNG.tFPWriterPNG.Create;

      try
         Image.SaveToFile ('test_image.png', Img_Writer);
      finally
         Image.Free;
         Img_Writer.Free;
      end {try};
   finally
      Perf_Img.Free;
      My_Addie.Free;
   end {try};
end {Run};

begin
   Run;
end {Test_Image}.

$Id$

