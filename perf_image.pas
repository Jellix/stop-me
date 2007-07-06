{/= Perf_Image =======================================================\}
{                                                                      }
{ Performance graph for an ADDIE.                                      }
{ Part of the Stop-Me (STOchastic Performance MEasurement) project.    }
{                                                                      }
{ Copyright (C) 2006, 2007 by Johnny L. Fencey                         }
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

//-- @abstract(Implements  some sort of performance graph to display the
//--           values fed to an ADDIE over time.)
//-- Uses  tasking,  but  has  by no means real time properties,  so the
//-- resulting graph will be much less than perfect.
unit
   Perf_Image;


interface


uses
   Addie,
   Calendar,
   Classes,
   Contnrs,
   FPImage;


type
   //-- @abstract(The actual performance graph object.)
   //-- Can  convert  collected  performance  data  and convert it to an
   //-- image.
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
      //-- Stops any running monitor thread. The ADDIE itself is @bold(
      //-- not) freed.
      destructor Destroy; override;

      {/= Create_Image ===============================================\}
      {                                                                }
      {\==============================================================/}
      //-- @abstract(Creates a performance graph of the data gathered so
      //--           far.)
      //-- @param(Perf_Data    A stream containg @code(Double)s.)
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
   procedure Set_Greyscale_Palette (out Palette : FPImage.tFPPalette);
   var
      Col : FPImage.tFPColor;
      i   : Integer;
      Max : Integer; // Just to optimize those "Pred (Palette.Count)"s.
   begin
      Palette.Count := 256;
      Max := Pred (Palette.Count);

      for i := 0 to Pred (Palette.Count) do
      begin
         Col.Red   := Round (65535.0 * i / Max);
         Col.Green := Col.Red;
         Col.Blue  := Col.Red;
         Col.Alpha := FPImage.AlphaOpaque;

         Palette[Max - i] := Col;
      end {for};
   end {Set_Grayscale_Palette};

var
   Num_Samples : Integer;
   Sample      : Double;
   x           : Integer;
   y           : Integer;
begin // Performance_Graph.Create_Image
   Num_Samples := Perf_Data.Size div SizeOf (Double);

   Image := FPImage.tFPMemoryImage.Create (Num_Samples, 2**Resolution);

   Canvas := FPImgCanv.tFPImageCanvas.Create (Image);

   try
      Set_Greyscale_Palette (Image.Palette);

      Draw_Y_Axis (Image.Palette.Count div 2,
                   Image.Palette.Count div 4);

      // Draw the data lines.  An improved version would use antialiased
      // line drawing.
      Canvas.Pen.fpColor := Image.Palette[Pred (Image.Palette.Count)];
      Canvas.Pen.Mode    := FPCanvas.pmCopy;
      Canvas.Pen.Style   := FPCanvas.psSolid;
      Canvas.Pen.Width   := 1;

      Canvas.MoveTo (0, Image.Height div 2);

      Perf_Data.Seek (0, Classes.soFromBeginning);

      for x := 0 to Pred (Num_Samples) do
      begin
         Perf_Data.Read (Sample, SizeOf (Sample));

         y := Trunc ((1.0 - Sample) * Pred (Image.Height) + 0.5);
         Canvas.LineTo (x, y);
      end {for};
   finally
      Canvas.Free;
   end {try};
end {Performance_Graph.Create_Image};


end {Perf_Image}.

$Id$
