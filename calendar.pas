{/= Calendar =========================================================\}
{                                                                      }
{ Some duration functions based on integer math.                       }
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

//-- @abstract(Provides some stuff regarding relative times (duration).)
unit
   Calendar;


interface


uses
   SysUtils;


// seconds based time definitions
const
   //-- Milliseconds per unity second.
   MS_PER_SECOND      = SysUtils.MSecsPerSec;
   //-- Number of unity seconds in a minute.
   SECONDS_PER_MINUTE = SysUtils.SecsPerMin;
   //-- Number of minutes in an hour.
   MINUTES_PER_HOUR   = SysUtils.MinsPerHour;
   //-- Number of hours in a whole day.
   HOURS_PER_DAY      = SysUtils.HoursPerDay;
   //-- Number of unity seconds in an hour.
   SECONDS_PER_HOUR   = MINUTES_PER_HOUR * SECONDS_PER_MINUTE;
   //-- Number of unity seconds in a whole day.
   SECONDS_PER_DAY    = SysUtils.SecsPerDay;
   //-- Number of minutes in a whole day.
   MINUTES_PER_DAY    = SysUtils.MinsPerDay;

type
   //-- @abstract(The duration type.)
   //-- Should be handled as an opaque type.
   //-- To avoid drift issues and such stuff make it an integer.
   Duration      = type Int64;
   //-- @abstract(A discrete time type.)
   //-- It  is  mainly  here  to  provide  some  more  clear  subroutine
   //-- interfaces.
   Discrete_Time = Int64;

const
   //-- @abstract(A constant denoting a null duration.)
   NULL_DURATION = Duration(0);
   //-- @abstract(The maximum duration representable.)
   MAX_DURATION = Duration(High (Duration));

const
   //-- @abstract(Absolute beginning of time. Big bang to say so.)
   BIG_BANG = tDateTime(0.0);

type
   //-- @abstract(The   accuracy  needed  or  used  in  time  conversion
   //--           subroutines.)
   Accuracy = (//-- Accuracy rounded to full days.
               acDays,
               //-- Accuracy rounded to full hours.
               acHours,
               //-- Accuracy rounded to full minutes.
               acMinutes,
               //-- Accuracy rounded to full seconds.
               acSeconds,
               //-- Accuracy is in milliseconds.
               acMilliseconds);

{ -- Base subroutines ------------------------------------------------ }

{/= Clock ============================================================\}
{                                                                      }
{\====================================================================/}
//-- @abstract(Provides another system clock.)
//-- @returns(The ticks gone since the @link(BIG_BANG).)
//-- Due  to  the  mapping  on the @code(SysUtils.Now) stuff this is not
//-- guaranteed to be very precise, but it may come in handy.
function Clock : Duration;

{/= Duration_Since ===================================================\}
{                                                                      }
{\====================================================================/}
//-- @abstract(Returns  the  duration  between  the  given  past and the
//--           current system time.)
//-- @param(Past_Time   Some  point in time supposed to have happened in
//--                    the past.)
//-- @returns(The duration between the given time and right now.)
//-- Currently the returned duration will always be positive.
function Duration_Since (const Past_Time : tDateTime) : Duration;

{/= Sleep_Until ======================================================\}
{                                                                      }
{\====================================================================/}
//-- @abstract(A "simple" @italic(@code(delay until)) implementation.)
//-- @param(Next       The  absolute  point  in  time  to  sleep  until.
//--                   Usually   initialized  with  @code(Duration_Since
//--                   (BIG_BANG) + Some_Interval).)
//-- @bold(NOTE): To be semantically correct, the whole operation should
//--              be uninterruptible.  As this is close to impossible to
//--              implement, do not trust the semantics too far.
//--              Still,  using this implementation should eliminate any
//--              large cumulative drift on looped delays.
//--
//-- Some example of usage:
//--
//-- @longcode(
//-- procedure Absolute_Delay;
//-- var
//--    i        : 1 .. 1000;
//--    Next     : Calendar.Duration;
//--    Interval : Calendar.Duration;
//-- begin
//--    Interval := Calendar.Milliseconds (10);
//--    Next     := Calendar.Duration_Since (Calendar.BIG_BANG) + Interval;
//--
//--    for i := Low (i) to High (i) do
//--    begin
//--       Calendar.Sleep_Until (Next);
//--       Next := Next + Interval;
//--    end {for};
//-- end {Absolute_Delay};
//-- )
procedure Sleep_Until (const Next : Duration);


{ -- Subroutines providing "constants" ------------------------------- }

{/= Days =============================================================\}
{                                                                      }
{\====================================================================/}
//-- @abstract(Translates  a  given  number  of  days  into our duration
//--           type.)
//-- @param(Num_Days   The  number of days to be converted to a duration
//--                   type.)
//-- @returns(The duration of given number of days.)
function Days (const Num_Days : Discrete_Time) : Duration;

{/= Hours ============================================================\}
{                                                                      }
{\====================================================================/}
//-- @abstract(Translates  a  given  number  of  hours into our duration
//--           type.)
//-- @param(Num_Hours   The  number  of  hours  to  be  converted  to  a
//--                    duration type.)
//-- @returns(The duration of given number of hours.)
function Hours (const Num_Hours : Discrete_Time) : Duration;

{/= Minutes ==========================================================\}
{                                                                      }
{\====================================================================/}
//-- @abstract(Translates  a  given  number of minutes into our duration
//--           type.)
//-- @param(Num_Minutes   The  number  of  minutes  to be converted to a
//--                      duration type.)
//-- @returns(The duration of given number of minutes.)
function Minutes (const Num_Minutes : Discrete_Time) : Duration;

{/= Seconds ==========================================================\}
{                                                                      }
{\====================================================================/}
//-- @abstract(Translates  a  given  number of seconds into our duration
//--           type.)
//-- @param(Num_Seconds   The  number  of  seconds  to be converted to a
//--                      duration type.)
//-- @returns(The duration of given number of seconds.)
function Seconds (const Num_Seconds : Discrete_Time) : Duration;

{/= Milliseconds =====================================================\}
{                                                                      }
{\====================================================================/}
//-- @abstract(Translates  a  given  number  of  milliseconds  into  our
//--           duration type.)
//-- @param(Num_MSecs   The number of days to be converted to a duration
//--                    type.)
//-- @returns(The duration of given number of milliseconds.)
function Milliseconds (const Num_MSecs : Discrete_Time) : Duration;


{ -- Conversion subroutines ------------------------------------------ }

{/= To_Duration ======================================================\}
{                                                                      }
{\====================================================================/}
//-- @abstract(Converts a @code(tDateTime) value into a duration.)
//-- @param(Date_Time   The value to convert.)
//-- @returns(The "normalized" duration.)
function To_Duration (const Date_Time : tDateTime) : Duration;

{/= To_Time_Unit =====================================================\}
{                                                                      }
{\====================================================================/}
//-- @abstract(Converts  a  duration  into  a  rounded integer number in
//--           given time unit.)
//-- @param(Time_Span   The duration to convert.)
//-- @param(Time_Unit   The needed accuracy of the output.)
//-- @returns(The  rounded  number  of  the units as specified in @code(
//--          Time_Unit) for the given @code(Time_Span).)
function To_Time_Unit (const Time_Span : Duration;
                       const Time_Unit : Accuracy) : Discrete_Time;

{/= To_Days ==========================================================\}
{                                                                      }
{\====================================================================/}
//-- @abstract(Converts a duration into rounded number of days.)
//-- @param(Time_Span   The duration to convert.)
//-- @returns(Number of days specified by the @code(Time_Span).)
//-- This   is   a   convinience   function   using  @link(To_Time_Unit)
//-- internally.
function To_Days (const Time_Span : Duration) : Discrete_Time;

{/= To_Hours =========================================================\}
{                                                                      }
{\====================================================================/}
//-- @abstract(Converts a duration into rounded number of hours.)
//-- @param(Time_Span   The duration to convert.)
//-- @returns(Number of hours specified by the @code(Time_Span).)
//-- This   is   a   convinience   function   using  @link(To_Time_Unit)
//-- internally.
function To_Hours (const Time_Span : Duration) : Discrete_Time;

{/= To_Minutes =======================================================\}
{                                                                      }
{\====================================================================/}
//-- @abstract(Converts a duration into rounded number of minutes.)
//-- @param(Time_Span   The duration to convert.)
//-- @returns(Number of minutes specified by the @code(Time_Span).)
//-- This   is   a   convinience   function   using  @link(To_Time_Unit)
//-- internally.
function To_Minutes (const Time_Span : Duration) : Discrete_Time;

{/= To_Seconds =======================================================\}
{                                                                      }
{\====================================================================/}
//-- @abstract(Converts a duration into rounded number of seconds.)
//-- @param(Time_Span   The duration to convert.)
//-- @returns(Number of seconds specified by the @code(Time_Span).)
//-- This   is   a   convinience   function   using  @link(To_Time_Unit)
//-- internally.
function To_Seconds (const Time_Span : Duration) : Discrete_Time;

{/= To_Milliseconds ==================================================\}
{                                                                      }
{\====================================================================/}
//-- @abstract(Converts a duration into rounded number of milliseconds.)
//-- @param(Time_Span   The duration to convert.)
//-- @returns(Number of milliseconds specified by the @code(Time_Span).)
//-- This   is   a   convinience   function   using  @link(To_Time_Unit)
//-- internally.
function To_Milliseconds (const Time_Span : Duration) : Discrete_Time;


{ -- String (display) subroutines ------------------------------------ }

{/= Image ============================================================\}
{                                                                      }
{\====================================================================/}
//-- @abstract(Converts  a  given  number  of milliseconds into a string
//--           representation with a given accuracy.)
//-- @param(Time_Span   The  duration  to  convert  to  a human readable
//--                    representation.)
//-- @param(Time_Unit   The  needed  unit (accuracy) of the output.  The
//--                    number  of hours and minutes is always included,
//--                    days  are only included if it turns out to be at
//--                    least one. So @code(Accuracy)s other than @code(
//--                    acMinutes),     @code(acSeconds)    or    @code(
//--                    acMilliseconds) are not useful in that context.)
//-- @returns(A  human  readable  string  representation  of  the  given
//--          duration.)
function Image (const Time_Span : Duration;
                const Time_Unit : Accuracy = acSeconds) : String;


implementation


uses
   DateUtils;

//const
//   MODULE_PREFIX = 'Calendar.';


const
   // The ticks constant for the conversions.
   // Ticks per milliseconds shall define our base resolution.
   TICKS_PER_MILLISECOND = 10; // 100 µs resolution shall be enough.
   TICKS_PER_SECOND      = MS_PER_SECOND      * TICKS_PER_MILLISECOND;
   TICKS_PER_MINUTE      = SECONDS_PER_MINUTE * TICKS_PER_SECOND;
   TICKS_PER_HOUR        = MINUTES_PER_HOUR   * TICKS_PER_MINUTE;
   TICKS_PER_DAY         = HOURS_PER_DAY      * TICKS_PER_HOUR;


const
   Durations : array[Accuracy] of Duration =
      ({ Days        } TICKS_PER_DAY,
       { Hours       } TICKS_PER_HOUR,
       { Minutes     } TICKS_PER_MINUTE,
       { Seconds     } TICKS_PER_SECOND,
       { Millisecond } TICKS_PER_MILLISECOND);


{ -- Base subroutines ------------------------------------------------ }

{/= Clock ============================================================\}
{                                                                      }
{\====================================================================/}
function Clock : Duration;
begin
   exit (Duration_Since (BIG_BANG));
end {Clock};


{/= Duration_Since ===================================================\}
{                                                                      }
{\====================================================================/}
function Duration_Since (const Past_Time : tDateTime) : Duration;
begin
   exit (TICKS_PER_MILLISECOND *
         DateUtils.MillisecondsBetween (Past_Time, SysUtils.Now));
end {Duration_Since};


{/= Sleep_Until ======================================================\}
{                                                                      }
{\====================================================================/}
procedure Sleep_Until (const Next : Duration);
var
   Right_Now : Duration;
begin
   Right_Now := Clock;

   if Right_Now < Next then
      SysUtils.Sleep (To_Milliseconds (Next - Right_Now));
end {Sleep_Until};


{ -- Subroutines providing "constants" ------------------------------- }

{/= Days =============================================================\}
{                                                                      }
{\====================================================================/}
function Days (const Num_Days : Discrete_Time) : Duration;
begin
   exit (Num_Days * TICKS_PER_DAY);
end {Days};


{/= Hours ============================================================\}
{                                                                      }
{\====================================================================/}
function Hours (const Num_Hours : Discrete_Time) : Duration;
begin
   exit (Num_Hours * TICKS_PER_HOUR);
end {Hours};


{/= Minutes ==========================================================\}
{                                                                      }
{\====================================================================/}
function Minutes (const Num_Minutes : Discrete_Time) : Duration;
begin
   exit (Num_Minutes * TICKS_PER_MINUTE);
end {Minutes};


{/= Seconds ==========================================================\}
{                                                                      }
{\====================================================================/}
function Seconds (const Num_Seconds : Discrete_Time) : Duration;
begin
   exit (Num_Seconds * TICKS_PER_SECOND);
end {Seconds};


{/= Milliseconds =====================================================\}
{                                                                      }
{\====================================================================/}
function Milliseconds (const Num_MSecs : Discrete_Time) : Duration;
begin
   exit (Num_MSecs * TICKS_PER_MILLISECOND);
end {Milliseconds};


{ -- Conversion subroutines ------------------------------------------ }

{/= To_Duration ======================================================\}
{                                                                      }
{\====================================================================/}
//-- @abstract(Converts a @code(tDateTime) value into a duration.)
//-- @param(Date_Time   The value to convert.)
//-- @returns(The "normalized" duration.)
function To_Duration (const Date_Time : tDateTime) : Duration;
begin
   exit (Round (Date_Time * TICKS_PER_DAY));
end {To_Duration};


{/= To_Time_Unit =====================================================\}
{                                                                      }
{\====================================================================/}
function To_Time_Unit (const Time_Span : Duration;
                       const Time_Unit : Accuracy) : Discrete_Time;
begin
   exit (Round (1.0 * Time_Span / Durations[Time_Unit]));
end {To_Time_Unit};


{/= To_Days ==========================================================\}
{                                                                      }
{\====================================================================/}
function To_Days (const Time_Span : Duration) : Discrete_Time;
begin
   exit (To_Time_Unit (Time_Span, acDays));
end {To_Hours};


{/= To_Hours =========================================================\}
{                                                                      }
{\====================================================================/}
function To_Hours (const Time_Span : Duration) : Discrete_Time;
begin
   exit (To_Time_Unit (Time_Span, acHours));
end {To_Hours};


{/= To_Minutes =======================================================\}
{                                                                      }
{\====================================================================/}
function To_Minutes (const Time_Span : Duration) : Discrete_Time;
begin
   exit (To_Time_Unit (Time_Span, acMinutes));
end {To_Minutes};


{/= To_Seconds =======================================================\}
{                                                                      }
{\====================================================================/}
function To_Seconds (const Time_Span : Duration) : Discrete_Time;
begin
   exit (To_Time_Unit (Time_Span, acSeconds));
end {To_Seconds};


{/= To_Milliseconds ==================================================\}
{                                                                      }
{\====================================================================/}
function To_Milliseconds (const Time_Span : Duration) : Discrete_Time;
begin
   exit (To_Time_Unit (Time_Span, acMilliseconds));
end {To_Milliseconds};


{ -- String (display) subroutines ------------------------------------ }

{/= Image ============================================================\}
{                                                                      }
{\====================================================================/}
function Image (const Time_Span : Duration;
                const Time_Unit : Accuracy) : String;
//const
//   PROC_PREFIX = MODULE_PREFIX + 'Image';
const
   HALF_A_MINUTE = DateUtils.OneMinute / 2.0;
   HALF_A_SECOND = DateUtils.OneSecond / 2.0;
   ZERO_TIME     = 0.0;
const
   Round_Adjust : array[Accuracy] of tDateTime =
     ({Days        } HALF_A_MINUTE,
      {Hours       } HALF_A_MINUTE,
      {Minutes     } HALF_A_MINUTE,
      {Seconds     } HALF_A_SECOND,
      {Milliseconds} ZERO_TIME);
var
   Days        : Integer;
   My_Duration : tDateTime;
begin
   // Convert it to tDateTime for convinience and adjust for rounding.
   My_Duration := Abs (1.0 * Time_Span / TICKS_PER_DAY) +
                  Round_Adjust[Time_Unit];

   // Days are the largest stuff we support.
   Days := DateUtils.DaysBetween (BIG_BANG, My_Duration);

   if Days > 0 then
      Result := SysUtils.Format ('%D d, ', [Days])
   else
      Result := '';

   Result := Result + SysUtils.Format (
                         '%.2D:%.2D',
                         [DateUtils.HourOf (My_Duration),
                          DateUtils.MinuteOf (My_Duration)]);

   if Time_Unit >= acSeconds then
      Result := Result + SysUtils.Format (
                               ':%.2D',
                               [DateUtils.SecondOf (My_Duration)]);

   if Time_Unit >= acMilliseconds then
      Result := Result + SysUtils.Format (
                               '.%.3D',
                               [DateUtils.MillisecondOf (My_Duration)]);
end {Image};


end {Calendar}.

$Id$

