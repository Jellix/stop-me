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
   Contnrs;


type
   //-- @abstract(The actual performance graph object.)
   //-- Values  retrieved from the connected addie are stored internally
   //-- and will be converted to a real image on request.
   Performance_Graph = class (tObject)
   public
      {/= Create =====================================================\}
      {                                                                }
      {\==============================================================/}
      //-- @abstract(The usual constructor.)
      //-- Just  "connects"  an  ADDIE  to the instance.  No performance
      //-- monitoring will happen yet.
      //-- @param(Element   The ADDIE to be monitored.)
      constructor Create (const Element : Addie.tAddie);

      {/= Destroy ====================================================\}
      {                                                                }
      {\==============================================================/}
      //-- @abstract(Cleans up the instance.)
      //-- Stops any running monitor thread. The ADDIE itself is @bold(
      //-- not) freed.
      destructor Destroy; override;

      {/= Start ======================================================\}
      {                                                                }
      {\==============================================================/}
      //-- @abstract(Starts monitoring the ADDIE's values.)
      //-- @param(Cycle_Time   The  time in ms between successive stores
      //--                     of the ADDIE's current probability.)
      procedure Start (const Cycle_Time : Calendar.Duration); virtual;

      {/= Stop =======================================================\}
      {                                                                }
      {\==============================================================/}
      //-- @abstract(Stops monitoring the ADDIE's values.)
      procedure Stop; virtual;

      {/= Create_Image ===============================================\}
      {                                                                }
      {\==============================================================/}
      //-- @abstract(Creates a performance graph of the data gathered so
      //--           far.)
      //-- @param(Img_Data   The  stream  where  the  image data will be
      //--                   written to. Complete with header etc.)
      procedure Create_Image (const Img_Data : Classes.tStream);

   protected
      //-- @abstract(Reference to the monitored ADDIE's instance.)
      My_Addie : Addie.tAddie;

      //-- @abstract(Reference to the monitoring thread.)
      Monitor : Classes.tThread;

      //-- @abstract(The list of recorded data points.)
      Perf_Data : Contnrs.tObjectList;

   end {Performance_Graph};


implementation


uses
   Math,
   SysUtils,
   FPCanvas,
   FPImage,
   FPImgCanv,
   FPWritePNG;


{ -- a single performance record point --------------------------------}

type
   Data_Point = class (tObject)
   public
      {/= Create =====================================================\}
      {                                                                }
      {\==============================================================/}
      constructor Create (const AWhat : Double);

   protected
      What : Double;
   end {Data_Point};


{/= Create ===========================================================\}
{                                                                      }
{\====================================================================/}
constructor Data_Point.Create (const AWhat : Double);
begin
   inherited Create;
   self.What := AWhat;
end {Data_Point.Create};


{ -- the monitor thread -----------------------------------------------}
type
   Monitor_Task = class (Classes.tThread)
   public
      {/= Create =====================================================\}
      {                                                                }
      {\==============================================================/}
      constructor Create (const Ref_Element : Addie.tAddie;
                          const Cycle_Time  : Calendar.Duration;
                          const Data_List   : Contnrs.tObjectList);

   protected
      {/= Execute ====================================================\}
      {                                                                }
      {\==============================================================/}
      procedure Execute; override;

      Monitored    : Addie.tAddie;
      Sample_Cycle : Calendar.Duration;
      Perf_List    : Contnrs.tObjectList;
   end {Monitor_Task};


{/= Monitor_Task.Create ==============================================\}
{                                                                      }
{\====================================================================/}
constructor Monitor_Task.Create (
      const Ref_Element : Addie.tAddie;
      const Cycle_Time  : Calendar.Duration;
      const Data_List   : Contnrs.tObjectList);
begin
   self.Monitored    := Ref_Element;
   self.Sample_Cycle := Cycle_Time;
   self.Perf_List    := Data_List;

   inherited Create (False);
end {Monitor_Task.Create};


{/= Monitor_Task.Execute =============================================\}
{                                                                      }
{\====================================================================/}
procedure Monitor_Task.Execute;
var
   Next_Time : Calendar.Time;
begin
   // Set up next time trigger first.
   Next_Time := Calendar.Clock + self.Sample_Cycle;

   while not self.Terminated do
   begin
      // Create  a new data point with ADDIE's probability and insert it
      // into the list.
      self.Perf_List.Add (
            Data_Point.Create (self.Monitored.Probability));
      Calendar.Sleep_Until (Next_Time);
      Next_Time := Next_Time + self.Sample_Cycle;
   end {while};
end {Monitor_Task.Create};


{ -- the exported performance graph class -----------------------------}

{/= Performance_Graph.Create =========================================\}
{                                                                      }
{\====================================================================/}
constructor Performance_Graph.Create (const Element : Addie.tAddie);
begin
   inherited Create;
   self.My_Addie  := Element;

   self.Perf_Data := Contnrs.tObjectList.Create;
   self.Perf_Data.OwnsObjects := True;
end {Performance_Graph.Create};


{/= Performance_Graph.Destroy ========================================\}
{                                                                      }
{\====================================================================/}
destructor Performance_Graph.Destroy;
begin
   self.Stop; // Stop possibly running monitor thread.
   self.Perf_Data.Free;
   inherited Destroy;
end {Performance_Graph.Destroy};


{/= Performance_Graph.Start ==========================================\}
{                                                                      }
{\====================================================================/}
procedure Performance_Graph.Start (
      const Cycle_Time : Calendar.Duration);
begin
   if Assigned (self.Monitor) then
      SysUtils.FreeAndNil (self.Monitor);

   self.Monitor := Monitor_Task.Create (self.My_Addie,
                                        Cycle_Time,
                                        self.Perf_Data);
end {Performance_Graph.Start};


{/= Performance_Graph.Stop ===========================================\}
{                                                                      }
{\====================================================================/}
procedure Performance_Graph.Stop;
begin
   if Assigned (self.Monitor) then
      SysUtils.FreeAndNil (self.Monitor);
end {Performance_Graph.Stop};


{/= Performance_Graph.Create_Image ===================================\}
{                                                                      }
{\====================================================================/}
procedure Performance_Graph.Create_Image (
      const Img_Data : Classes.tStream);
var
   Canvas : FPCanvas.tFPCustomCanvas;
   Img    : FPImage.tFPCustomImage;

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
      Canvas.Pen.FPColor := Img.Palette[Col_Index];
      Canvas.Line (0,                (Lower + Upper) div 2,
                   Pred (Img.Width), (Lower + Upper) div 2);

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
      Canvas.Pen.fpColor := Img.Palette[Sub_Axis_Color];

      for i := 0 to 10 do
      begin
         y := Trunc ((0.1 * i) * Pred (Img.Height));
         Canvas.Line (0, y, Pred (Img.Width), y);
      end {for};

      // Draw 50% rule with medium color.
      Canvas.Pen.fpColor := Img.Palette[Main_Axis_Color];

      y := Pred (Img.Height) div 2;
      Canvas.Line (0, y, Pred (Img.Width), y);
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
   Img_Writer  : FPImage.tFPCustomImageWriter;
   Data_Points : Integer;
   x           : Integer;
   y           : Integer;
begin // Performance_Graph.Create_Image
   Data_Points := self.Perf_Data.Count;

   Img := FPImage.tFPMemoryImage.Create (Data_Points,
                                         2**self.My_Addie.Bits);
   Canvas := FPImgCanv.tFPImageCanvas.Create (Img);

   try
      Set_Greyscale_Palette (Img.Palette);

      Draw_Y_Axis (Img.Palette.Count div 2, Img.Palette.Count div 4);

      // Draw the data lines.  An improved version would use antialiased
      // line drawing.
      Canvas.Pen.fpColor := Img.Palette[Pred (Img.Palette.Count)];
      Canvas.Pen.Mode    := FPCanvas.pmCopy;
      Canvas.Pen.Style   := FPCanvas.psSolid;
      Canvas.Pen.Width   := 1;

      Canvas.MoveTo (0, Img.Height div 2);

      for x := 0 to Pred (Data_Points) do
      begin
         y := Trunc ((1.0 - Data_Point(self.Perf_Data[x]).What) *
                     Pred (Img.Height) + 0.5);
         Canvas.LineTo (x, y);
      end {for};

      // Finally create the .png image.
      Img_Writer := FPWritePNG.tFPWriterPNG.Create;

      try
         Img.SaveToStream (Img_Data, Img_Writer);
      finally
         Img_Writer.Free;
      end {try};
   finally
      Canvas.Free;
      Img.Free;
   end {try};
end {Performance_Graph.Stop};


end {Perf_Image}.

$Id$

