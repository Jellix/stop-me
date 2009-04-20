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
   Canvas : FPCanvas.tFPCustomCanvas;

   {/= Draw_Rule =====================================================\}
   {                                                                   }
   { Recursive rule drawer. Not used anymore.                          }
   {                                                                   }
   { Would   draw  [50%, [25%, [12.5%, 37.5%]], [75%, [62.5%, 87.5%]]] }
   { and so on lines in ever lighter grayscale.                        }
   {                                                                   }
   {\=================================================================/}
   procedure Draw_Rule (const Col_Index : Integer;
                        const Lower     : Integer;
                        const Upper     : Integer);
   begin
      Canvas.Pen.FPColor := Image.Palette[Col_Index];
      Canvas.Line (0,                  (Lower + Upper) div 2,
                   Pred (Image.Width), (Lower + Upper) div 2);

      // Break  out  of  recursion  if  color is too light or the pixel
      // distance between the lines becomes too small.
      if (Col_Index > 16) and ((Upper - Lower) > 8) then
      begin
         Draw_Rule (Col_Index div 2, Lower, (Lower + Upper) div 2);
         Draw_Rule (Col_Index div 2, (Lower + Upper) div 2, Upper);
      end {if};
   end {Draw_Rule};

   {/= Draw_Y_Axis ===================================================\}
   {                                                                   }
   { Draws the main Y axis at 50% and some lighter ones at 10% steps.  }
   {                                                                   }
   {\=================================================================/}
   procedure Draw_Y_Axis (Main_Axis_Color : Integer;
                          Sub_Axis_Color  : Integer);
   var
      i : Integer;
      y : Integer;
   begin
      // General settings.
      Canvas.Pen.Mode  := FPCanvas.pmCopy;
      Canvas.Pen.Style := FPCanvas.psDot;
      Canvas.Pen.Width := 1;

      // Draw 10% rules with light color.
      Canvas.Pen.fpColor := Image.Palette[Sub_Axis_Color];

      for i := 0 to 10 do
      begin
         y := Trunc ((0.1 * i) * Pred (Image.Height));
         Canvas.Line (0, y, Pred (Image.Width), y);
      end {for};

      // Draw 50% rule with medium color.
      Canvas.Pen.fpColor := Image.Palette[Main_Axis_Color];

      y := Pred (Image.Height) div 2;
      Canvas.Line (0, y, Pred (Image.Width), y);
   end {Draw_Y_Axis};

   {/= Set_Grayscale_Palette =========================================\}
   {                                                                   }
   {\=================================================================/}
   procedure Set_Grayscale_Palette (var Palette : FPImage.tFPPalette);
   var
      Col : FPImage.tFPColor;
      i   : Integer;
      Max : Integer; // Just to optimize those "Pred (Palette.Count)"s.
   begin
      Palette.Count := 256;
      Max := Pred (Palette.Count);

      for i := 0 to Max do
      begin
         Col.Red   := Round (65535.0 * i / Max);
         Col.Green := Col.Red;
         Col.Blue  := Col.Red;
         Col.Alpha := FPImage.AlphaOpaque;

         Palette[Max - i] := Col;
      end {for};
   end {Set_Grayscale_Palette};

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
   y           : Integer;
begin // Performance_Graph.Create_Image
   // (Current) number of samples in data stream.
   Num_Samples := Perf_Data.Size div SizeOf (Perf_Measure.Data_Sample);

   // Calculate image size with 32K - 1 restriction. This also restricts
   // the maximum memory allocated for the image to less than 2 GiBi.
   Img_Width  := Math.Min (Num_Samples,     Pred (2 ** MAX_BIT_DEPTH));
   Img_Height := Math.Min (2 ** Resolution, Pred (2 ** MAX_BIT_DEPTH));

   // Create plain empty image.
   Image := FPImage.tFPMemoryImage.Create (Img_Width, Img_Height);

   // Now the basic image has been created. Do a stupid sanity check and
   // bail out if there is no sample at all.  This would create an image
   // of  width  0.  Not  sure if the image handlers can actually handle
   // that, but they surely should.
   if Image.Width = 0 then
      exit;

   // Grayscale should be good enough. Of course, we could also do more
   // colorful pictures. In fact, the preprocessing should be separated
   // from the actual data line drawing.
   Set_Grayscale_Palette (Image.Palette);

   // Finally create the canvas drawing on the image.
   Canvas := FPImgCanv.tFPImageCanvas.Create (Image);

   try
      // Set up a simple Y scale.
      Draw_Y_Axis (Image.Palette.Count div 2,
                   Image.Palette.Count div 4);

      //
      // Draw the data lines.  An improved version would use antialiased
      // line drawing.
      //

      Canvas.Pen.fpColor := Image.Palette[Pred (Image.Palette.Count)];
      Canvas.Pen.Mode    := FPCanvas.pmCopy;
      Canvas.Pen.Style   := FPCanvas.psSolid;
      Canvas.Pen.Width   := 1;

      // If  there  were  more  than  32 Ki  samples,  only consider the
      // newest ones.
      // TODO: An  improved  version  could  split the images and create
      //       several ones in a row instead of ignoring older data.
      Perf_Data.Seek (SizeOf (Sample) * (Num_Samples - Img_Width),
                      Classes.soFromBeginning);

      // Set the initial position to that of the first sample.
      Perf_Data.Read (Sample, SizeOf (Sample));
      Canvas.MoveTo (0, Trunc ((1.0 - Sample) *
                               Pred (Image.Height) + 0.5));

      // The line drawing loop.
      for x := 1 to Pred (Image.Width) do
      begin
         Perf_Data.Read (Sample, SizeOf (Sample));

         // Image coordinates are top-left based, so we have to flip the
         // Y values for the 100% being at the top.
         y := Trunc ((1.0 - Sample) * Pred (Image.Height) + 0.5);
         Canvas.LineTo (x, y);
      end {for};
   finally
      Canvas.Free;
   end {try};
end {Performance_Graph.Create_Image};


end {Perf_Image}.

$Id$
