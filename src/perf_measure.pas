{/= Perf_Measure =====================================================\}
{                                                                      }
{ The  actual performance measurement.  Takes samples from an ADDIE at }
{ regular intervals.                                                   }
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

//-- @abstract(Implements  the  collecting of performance information of
//--           values fed to an ADDIE over time.)
//-- Uses  tasking,  but  has  by no means real time properties,  so any
//-- resulting graph will be much less than perfect.
unit
   Perf_Measure;


interface


uses
   Addie,
   Calendar,
   Classes;


type
   //-- @abstract(A single sample.)
   //-- As  the  range  of  a  sample  is  always between @code(0.0) and
   //-- @code(1.0)  and  the original data has no better resolution than
   //-- 32 bits  anyway,  we use the less memory consuming @code(Single)
   //-- type here.
   Data_Sample = type Single;

type
   //-- @abstract(The actual performance measure object.)
   //-- Values  retrieved from the connected addie are stored internally
   //-- and can be retrieved on request.
   Data_Collector = class (tObject)
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
      //-- @param(Cycle_Time   The time between successive stores of the
      //--                     ADDIE's current probability.)
      procedure Start (const Cycle_Time : Calendar.Duration); virtual;

      {/= Stop =======================================================\}
      {                                                                }
      {\==============================================================/}
      //-- @abstract(Stops monitoring the ADDIE's values.)
      procedure Stop; virtual;

   protected
      //-- @abstract(The list of recorded data points.)
      //-- Each  point  is  of  type  @link(Data_Sample),   so  use  the
      //-- appropriate  @code(SizeOf (Data_Sample))  when  accessing the
      //-- data.
      Perf_Data : Classes.tMemoryStream;

   public
      //-- @abstract(Provides   external   access   to   the   collected
      //--           performance data.)
      property Data_Points : Classes.tMemoryStream read Perf_Data;

   protected
      //-- @abstract(Reference to the monitored ADDIE's instance.)
      My_Addie : Addie.tAddie;

      //-- @abstract(Reference to the monitoring thread.)
      Monitor : Classes.tThread;

   end {Data_Collector};


implementation


uses
   SysUtils;


{ -- Monitor_Task - the monitor thread --------------------------------}
type
   Monitor_Task = class (Classes.tThread)
   public
      {/= Create =====================================================\}
      {                                                                }
      {\==============================================================/}
      constructor Create (const Ref_Element : Addie.tAddie;
                          const Cycle_Time  : Calendar.Duration;
                          const Data_List   : Classes.tStream);

   protected
      {/= Execute ====================================================\}
      {                                                                }
      {\==============================================================/}
      procedure Execute; override;

      Monitored    : Addie.tAddie;
      Sample_Cycle : Calendar.Duration;
      Perf_List    : Classes.tStream;
   end {Monitor_Task};


{/= Monitor_Task.Create ==============================================\}
{                                                                      }
{\====================================================================/}
constructor Monitor_Task.Create (const Ref_Element : Addie.tAddie;
                                 const Cycle_Time  : Calendar.Duration;
                                 const Data_List   : Classes.tStream);
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
   Sample    : Data_Sample;
begin
   // Set up next time trigger first.
   Next_Time := Calendar.Clock + self.Sample_Cycle;

   while not self.Terminated do
   begin
      // Write the current ADDIE's probability to the target stream.
      Sample := self.Monitored.Probability;
      self.Perf_List.Write (Sample, SizeOf (Sample));

      // Wait...
      Calendar.Sleep_Until (Next_Time);
      Next_Time := Next_Time + self.Sample_Cycle;
   end {while};
end {Monitor_Task.Create};


{ -- Data_Collector - The exported performance measuring class --------}

{/= Data_Collector.Create ============================================\}
{                                                                      }
{\====================================================================/}
constructor Data_Collector.Create (const Element : Addie.tAddie);
begin
   inherited Create;

   self.My_Addie  := Element;
   self.Perf_Data := Classes.tMemoryStream.Create;
end {Data_Collector.Create};


{/= Data_Collector.Destroy ===========================================\}
{                                                                      }
{\====================================================================/}
destructor Data_Collector.Destroy;
begin
   self.Stop; // Stop possibly running monitor thread.
   self.Perf_Data.Free;

   inherited Destroy;
end {Data_Collector.Destroy};


{/= Data_Collector.Start =============================================\}
{                                                                      }
{\====================================================================/}
procedure Data_Collector.Start (const Cycle_Time : Calendar.Duration);
begin
   // Stop any running monitor.
   if Assigned (self.Monitor) then
      SysUtils.FreeAndNil (self.Monitor);

   // Clear old data.
   self.Perf_Data.Clear;

   // And start monitoring the values.
   self.Monitor := Monitor_Task.Create (self.My_Addie,
                                        Cycle_Time,
                                        self.Perf_Data);
end {Data_Collector.Start};


{/= Data_Collector.Stop ==============================================\}
{                                                                      }
{\====================================================================/}
procedure Data_Collector.Stop;
begin
   // Just stop the monitor.
   if Assigned (self.Monitor) then
      SysUtils.FreeAndNil (self.Monitor);
end {Data_Collector.Stop};


end {Perf_Measure}.

$Id$
