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
   {\=================================================================/}
   procedure Draw_Rule (const Col_Index : Integer;
                        const Lower     : Integer;
                        const Upper     : Integer);
   begin
      Canvas.Pen.FPColor := Img.Palette[Col_Index];
      Canvas.Line (0,                (Lower + Upper) div 2,
                   Pred (Img.Width), (Lower + Upper) div 2);

      if Col_Index > 32 then
      begin
         Draw_Rule (Col_Index div 2, Lower, (Lower + Upper) div 2);
         Draw_Rule (Col_Index div 2, (Lower + Upper) div 2, Upper);
      end {if};
   end {Draw_Rule};

var
   Img_Writer  : FPImage.tFPCustomImageWriter;
   Col         : FPImage.tFPColor;
   Data_Points : Integer;
   i           : Integer;
   x           : Integer;
   Y           : Integer;
begin // Performance_Graph.Create_Image
   Data_Points := self.Perf_Data.Count;

   Img := FPImage.tFPMemoryImage.Create (Data_Points,
                                         2**self.My_Addie.Bits);
   Img.Palette.Count := 256;

   for i := 0 to Pred (Img.Palette.Count) do
   begin
      Col.Red   := Round (65535.0 * i / Pred (Img.Palette.Count));
      Col.Green := Col.Red;
      Col.Blue  := Col.Red;
      Col.Alpha := FPImage.AlphaOpaque;
      Img.Palette[Img.Palette.Count - 1 - i] := Col;
   end {for};

   Canvas := FPImgCanv.tFPImageCanvas.Create (Img);

   Canvas.Pen.Mode    := FPCanvas.pmBlack;
   Canvas.Pen.Width   := 1;

   Canvas.Pen.Style   := FPCanvas.psDot;
   Draw_Rule (Img.Palette.Count div 2, 0, Pred (Img.Height));

   Canvas.Pen.FPColor := FPImage.colBlack;
   Canvas.Pen.Style   := FPCanvas.psSolid;
   Canvas.MoveTo (0, Img.Height div 2);

   for x := 0 to Pred (Data_Points) do
   begin
      Y := Trunc ((1.0 - Data_Point(self.Perf_Data[x]).What) *
                  Pred (Img.Height) + 0.5);
      Canvas.LineTo (x, Y);
   end {for};

   Img_Writer := FPWritePNG.tFPWriterPNG.Create;
   Img.SaveToStream (Img_Data, Img_Writer);
   Img_Writer.Free;

   Canvas.Free;
   Img.Free;
end {Performance_Graph.Stop};


end {Perf_Image}.

$Id$

