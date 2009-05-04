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
{             Ahornstrasse 14                                          }
{             D-59423 Unna                                             }
{             FR Germany, Europe, Earth, Solar System, Milky Way       }
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
   FPWriteBMP,
   FPWriteJPEG,
   FPWritePNG,
   LFSR,
   Perf_Image,
   Perf_Measure,
   SysUtils;


// Can be used for parametrization
const
   ADDIE_BITS    = 10;
   DATA_POINTS   = 1000;
   IMG_FILE_NAME = 'test_image'; // No extension here.


{/= Run ==============================================================\}
{                                                                      }
{\====================================================================/}
procedure Run;
var
   My_Addie : Addie.tAddie;
   Image    : FPImage.tFPCustomImage;
   Measure  : Perf_Measure.Data_Collector;
   Perf_Img : Perf_Image.Performance_Graph;
   i        : Integer;
begin
   My_Addie := Addie.tAddie.Create (LFSR.tRandom_64, ADDIE_BITS);
   Measure  := Perf_Measure.Data_Collector.Create (My_Addie);

   try
      Measure.Start (Calendar.Milliseconds (10));

      for i := 0 to Pred (DATA_POINTS) do
      begin
         My_Addie.Feed (Random > 0.9);
         SysUtils.Sleep (1);
      end {for};

      Measure.Stop;

      Perf_Img := Perf_Image.Performance_Graph.Create;

      try
         // Finally create the image.
         Perf_Img.Create_Image (Measure.Data_Points, ADDIE_BITS, Image);

         try
            Image.SaveToFile (IMG_FILE_NAME + '.bmp');
            Image.SaveToFile (IMG_FILE_NAME + '.jpeg');
            Image.SaveToFile (IMG_FILE_NAME + '.png');
         finally
            Image.Free;
         end {try};
      finally
         Perf_Img.Free;
      end {try};
   finally
      Measure.Free;
      My_Addie.Free;
   end {try};
end {Run};


begin // Test_Image
   Run;
end {Test_Image}.

$Id$
