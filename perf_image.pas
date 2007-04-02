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
   FPImage,
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

   {/= Line =======================================================\}
   {                                                                }
   {\==============================================================/}
   procedure Line (const Img : FPImage.tFPCustomImage;
                         X0  : Double;
                         Y0  : Double;
                         X1  : Double;
                         Y1  : Double);

      {/= Plot ====================================================\}
      {                                                             }
      {\===========================================================/}
      procedure Plot (const X : Integer;
                      const Y : Integer;
                      const C : Double);
      var
         New_C : Double;
      begin
         New_C := Math.Min (1.0, Img.Pixels[X, Y] / 255.0 + C);
         Img.Pixels[X, Y] := Round (255.0 * New_C);
      end {Plot};

   var
      DeltaX : Double;
      DeltaY : Double;
      Dir    : Integer;
   begin // Line
      // procedure WuLine(fixpt x1, fixpt y1, fixpt x2, fixpt y2)
      // variable declarations:
      // fixpt variables:
      //    grad, xd, yd, length,xm,ym
      //    xgap, ygap, xend, yend, xf, yf
      //    brightness1, brightness2
      //
      // integer variables:
      //    x, y, ix1, ix2, iy1, iy2
      //
      // byte variables:
      //    c1, c2
      //
      // code starts here:
      //
      // Width and Height of the line
      // xd = (x2-x1)
      // yd = (y2-y1)
      //
      //
      // if abs(xd) > abs(yd) then  check line gradient
      //    horizontal(ish) lines
      //
      //    if x1 > x2 then      if line is back to front
      //       swap x1 and x2    then swap it round
      //       swap y1 and y2
      //       xd = (x2-x1)      and recalc xd & yd
      //       yd = (y2-y1)
      //    end if
      //
      //    grad = yd/xd         gradient of the line
      //
      //
      //    End Point 1
      //    -----------
      //
      //    xend = trunc(x1+.5)        find nearest integer X-coordinate
      //    yend = y1 + grad*(xend-x1) and corresponding Y value
      //
      //    xgap = invfrac(x1+.5)      distance i
      //
      //    ix1  = int(xend)           calc screen coordinates
      //    iy1  = int(yend)
      //
      //    brightness1 = invfrac(yend) * xgap  calc the intensity of the other
      //    brightness2 =    frac(yend) * xgap  end point pixel pair.
      //
      //    c1 = byte(brightness1 * MaxPixelValue) calc pixel values
      //    c2 = byte(brightness2 * MaxPixelValue)
      //
      //    DrawPixel(ix1,iy1), c1     draw the pair of pixels
      //    DrawPixel(ix1,iy1+1), c2
      //
      //    yf = yend+grad    calc first Y-intersection for main loop
      //
      //    End Point 2
      //    -----------
      //
      //    xend = trunc(x2+.5)        find nearest integer X-coordinate
      //    yend = y2 + grad*(xend-x2) and corresponding Y value
      //
      //    xgap = invfrac(x2-.5)   distance i
      //
      //    ix2  = int(xend)  calc screen coordinates
      //    iy2  = int(yend)
      //
      //    brightness1 = invfrac(yend) * xgap  calc the intensity of the first
      //    brightness2 =    frac(yend) * xgap  end point pixel pair.
      //
      //    c1 = byte(brightness1 * MaxPixelValue)    calc pixel values
      //    c2 = byte(brightness2 * MaxPixelValue)
      //
      //    DrawPixel(ix2,iy2), c1     draw the pair of pixels
      //    DrawPixel(ix2,iy2+1), c2
      //
      //    MAIN LOOP
      //    ---------
      //
      //    Loop x from (ix1+1) to (ix2-1)   main loop
      //
      //       brightness1 = invfrac(yf)  calc pixel brightnesses
      //       brightness2 = frac(yf)
      //
      //       c1 = byte(brightness1 * MaxPixelValue)    calc pixel values
      //       c2 = byte(brightness2 * MaxPixelValue)
      //
      //       DrawPixel(x,int(yf)), c1   draw the pair of pixels
      //       DrawPixel(x,int(yf)+1), c2
      //
      //       yf = yf + grad    update the y-coordinate
      //
      //    end of x loop  end of loop
      //
      // else
      //    vertical(ish) lines
      //    handle the vertical(ish) lines in the
      //    ame way as the horizontal(ish) ones
      //    but swap the roles of X and Y
      // end if
      // end of procedure

      while (X0 <> X1) or (Y0 <> Y1) do
      begin
         DeltaX := Abs (X1 - X0);
         DeltaY := Abs (Y1 - Y0);

         if DeltaX > DeltaY then
         begin
            // X slope.
            if X1 < X0 then
               Dir := -1
            else
               Dir := 1;

            Plot (Trunc (X0),       Trunc (Y0), 1.0 - Frac (Y0));
            Plot (Trunc (X0) + Dir, Trunc (Y0), Frac (Y0));
            X0 := X0 + Dir;

            if Dir < 0 then
            begin
               if X0 < X1 then
                  X0 := X1;
            end {if}
            else
            begin
               if X0 > X1 then
                  X0 := X1;
            end {else};
         end {if}
         else
         begin
            // Y slope.
            if Y1 < Y0 then
               Dir := -1
            else
               Dir := 1;

            Plot (Trunc (X0), Trunc (Y0),       1.0 - Frac (X0));
            Plot (Trunc (X0), Trunc (Y0) + Dir, Frac (X0));
            Y0 := Y0 + Dir;

            if Dir < 0 then
            begin
               if Y0 < Y1 then
                  Y0 := Y1;
            end {if}
            else
            begin
               if Y0 > Y1 then
                  Y0 := Y1;
            end {else};
         end {else};
      end {while};
   end {Line};

var
   Img         : FPImage.tFPCustomImage;
   Img_Writer  : FPImage.tFPCustomImageWriter;
   Col         : FPImage.tFPColor;
   Data_Points : Integer;
   i           : Integer;
   x           : Integer;
   Prev_Y      : Double;
   Cur_Y       : Double;
begin
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

   // Draw horizontal line.
   for i := 0 to Img.Width - 1 do
      Img.Pixels[i, Succ (Img.Height) div 2] := 255;

   Prev_Y := (1.0 - Data_Point(self.Perf_Data[0]).What) *
                   Pred (Img.Height);

   for x := 0 to Pred (Data_Points) do
   begin
      Cur_Y := (1.0 - Data_Point(self.Perf_Data[x]).What) *
               Pred (Img.Height);
      Line (Img, Max (0, Pred (x)), Prev_Y, x, Cur_Y);
      Prev_Y := Cur_Y;
   end {for};

   Img_Writer := FPWritePNG.tFPWriterPNG.Create;
   Img.SaveToStream (Img_Data, Img_Writer);
   Img_Writer.Free;

   Img.Free;
end {Performance_Graph.Stop};


end {Perf_Image}.

$Id$

