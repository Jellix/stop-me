{/= Perf_Image =======================================================\}
{                                                                      }
{ Performance graph for an ADDIE.                                      }
{ Part of the Stop-Me (STOchastic Performance MEasurement) project.    }
{                                                                      }
{ Copyright (C) 2006, 2007, 2009 by Johnny L. Fencey                   }
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

//-- @abstract(Implements  some sort of performance graph to display the
//--           values fed to an ADDIE over time.)
unit
   Perf_Image;


interface


uses
   Agg2D,
   Agg_Bitmap,
   Addie,
   Classes,
   FPImage;


type
   //-- @abstract(The actual performance graph object.)
   //-- Can convert collected performance data into an image.
   Performance_Graph = class (tObject)
   public
      {/= Create =====================================================\}
      {                                                                }
      {\==============================================================/}
      //-- @abstract(The usual constructor.)
      constructor Create;

      {/= Destroy ====================================================\}
      {                                                                }
      {\==============================================================/}
      //-- @abstract(Cleans up the instance.)
      destructor Destroy; override;

      {/= Create_Image ===============================================\}
      {                                                                }
      {\==============================================================/}
      //-- @abstract(Creates a performance graph of the data gathered so
      //--           far.)
      //-- The  created  image  will  have  a width corresponding to the
      //-- number of data points and a height corresponding to the given
      //-- bit  depth,  but  will  be restricted to 32767 pixels in each
      //-- direction.
      //-- @param(Perf_Data    A      stream      containing      @link(
      //--                     Perf_Measure.Data_Sample)s.)
      //-- @param(Resolution   The  bit  depth  of  the  ADDIE  used  to
      //--                     collect  the data.  Used to determine the
      //--                     image height.)
      //-- @param(Image        The  "drawn"  image.  Must  be  freed  by
      //--                     caller.)
      procedure Create_Image (
            const Perf_Data  : Classes.tStream;
            const Resolution : Addie.tAddie_Bits;
            out   Image      : FPImage.tFPCustomImage);
   end {Performance_Graph};


implementation


uses
   Math,
   Perf_Measure,
   SysUtils,
   FPCanvas,
   FPImgCanv;


{ -- Performance_Graph - the exported performance graph class ---------}

{/= Performance_Graph.Create =========================================\}
{                                                                      }
{\====================================================================/}
constructor Performance_Graph.Create;
begin
   inherited Create;
end {Performance_Graph.Create};


{/= Performance_Graph.Destroy ========================================\}
{                                                                      }
{\====================================================================/}
destructor Performance_Graph.Destroy;
begin
   inherited Destroy;
end {Performance_Graph.Destroy};


{/= Performance_Graph.Create_Image ===================================\}
{                                                                      }
{\====================================================================/}
procedure Performance_Graph.Create_Image (
      const Perf_Data  : Classes.tStream;
      const Resolution : Addie.tAddie_Bits;
      out   Image      : FPImage.tFPCustomImage);
var
   Canvas : Agg2D.tAgg2D;

   {/= Draw_Y_Axis ===================================================\}
   {                                                                   }
   { Draws the main Y axis at 50% and some lighter ones at 10% steps.  }
   {                                                                   }
   {\=================================================================/}
   procedure Draw_Y_Axis;
   const
      LINES = 10; // 10% lines.
   var
      i : Integer;
      x : Double;
      y : Double;
   begin
      Canvas.LineColor (0, 0, 0);

      // General settings.
      // Draw 10% rules (half width lines).
      Canvas.LineWidth (0.5);

      x := 0.0;

      for i := 0 to LINES do
      begin
         y := ((1.0 * i) / LINES) * Image.Height;
         Canvas.AlignPoint (x, y);
         Canvas.Line (0.0, y, Image.Width, y);
      end {for};

      // Draw 50% rule with full line.
      Canvas.LineWidth (1.0);

      y := Image.Height / 2.0;
      Canvas.AlignPoint (x, y);
      Canvas.Line (0.0, y, Image.Width, y);

      Canvas.ResetPath;
   end {Draw_Y_Axis};

const
   // Most  image libs can not handle an image dimension > 32K, so clamp
   // the maximum supported bit depth to 15.
   MAX_BIT_DEPTH = 15;

var
   Num_Samples : LongInt;
   Sample      : Perf_Measure.Data_Sample;
   Img_Width   : Integer;
   Img_Height  : Integer;
   x           : Integer;
begin // Performance_Graph.Create_Image
   // (Current) number of samples in data stream.
   Num_Samples := Perf_Data.Size div SizeOf (Perf_Measure.Data_Sample);

   // Calculate image size with 32K - 1 restriction. This also restricts
   // the maximum memory allocated for the image to less than 4 GiBi.
   Img_Width  := Math.Min (Num_Samples,     Pred (2 ** MAX_BIT_DEPTH));
   Img_Height := Math.Min (2 ** Resolution, Pred (2 ** MAX_BIT_DEPTH));

   // Create plain empty image.
   Image := Agg_Bitmap.tAggBitmap.Create (Img_Width, Img_Height);

   // Now the basic image has been created. Do a stupid sanity check and
   // bail out if there is no sample at all.  This would create an image
   // of  width  0.  Not  sure if the image handlers can actually handle
   // that, but they surely should.
   if not Assigned (Image) or (Image.Width = 0) then
      exit;

   Canvas := Agg2D.tAgg2D.Create;
   
   try
      // Y-Axis is inverted, so flip image per default.
      Canvas.Attach (Image as Agg_Bitmap.tAggBitmap, True);

      // White "background".
      Canvas.ClearAll (255, 255, 255);

      // Set up a simple Y scale.
      Draw_Y_Axis;

      // Draw the data lines.
      Canvas.LineColor (0, 0, 0);
      Canvas.LineWidth (1);

      // If  there  were  more  than  32 Ki  samples,  only consider the
      // newest ones.
      // TODO: An  improved  version  could  split the images and create
      //       several ones in a row instead of ignoring older data.
      Perf_Data.Seek (SizeOf (Sample) * (Num_Samples - Img_Width),
                      Classes.soFromBeginning);

      // Set the initial position to that of the first sample.
      Perf_Data.Read (Sample, SizeOf (Sample));
      Canvas.MoveTo (0, Sample * Img_Height);

      // The line drawing loop.
      for x := 0 to Pred (Img_Width) do
      begin
         Perf_Data.Read (Sample, SizeOf (Sample));
         Canvas.LineTo (x, Sample * Img_Height);
      end {for};

      Canvas.LineTo (Img_Width, Sample * Img_Height);
      Canvas.DrawPath (Agg2D.AGG_StrokeOnly);
   finally
      Canvas.Free;
   end {try};
end {Performance_Graph.Create_Image};


end {Perf_Image}.

$Id$
