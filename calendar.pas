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
{$INLINE ON}

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
   Duration =
   record
      Value : Int64;
   end {Duration};

type
   //-- @abstract(The absolute time type.)
   //-- Should be handled as an opaque type.
   Time =
   record
      Value : Int64;
   end {Time};

type
   //-- @abstract(A general discrete time type.)
   //-- It  is  mainly  here  to  provide  some  more  clear  subroutine
   //-- interfaces.
   Discrete_Time = Int64;

const
   //-- @abstract(The maximum duration representable.)
   MAX_DURATION = High (Int64);

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


{ -- Operators to convert between the different types ---------------- }

{/= "=" ==============================================================\}
{                                                                      }
{\====================================================================/}
//-- @abstract(Compares two durations for equality.)
//-- @returns(@code(True)  if  both  durations denote the same length in
//--          time.)
operator = (const Left  : Duration;
            const Right : Duration) : Boolean; inline;

{/= "=" ==============================================================\}
{                                                                      }
{\====================================================================/}
//-- @abstract(Compares two times for equality.)
//-- @returns(@code(True)  if  the  arguments  denote  the same point in
//--          time.)
operator = (const Left  : Time;
            const Right : Time) : Boolean; inline;

{/= "<" ==============================================================\}
{                                                                      }
{\====================================================================/}
//-- @abstract(Compares two durations.)
//-- @returns(@code(True) if the left operand denotes a shorter duration
//--          than the right operand.)
operator < (const Left  : Duration;
            const Right : Duration) : Boolean; inline;

{/= "<" ==============================================================\}
{                                                                      }
{\====================================================================/}
//-- @abstract(Compares two times.)
//-- @returns(@code(True)  if  the  left operand denotes a point in time
//--          earlier than the one of the right operand.)
operator < (const Left  : Time;
            const Right : Time) : Boolean; inline;

{/= "<=" =============================================================\}
{                                                                      }
{\====================================================================/}
//-- @abstract(Compares two durations.)
//-- @returns(@code(True)  if  @code(Left)  denotes  an equal or shorter
//--          duration than @code(Right).)
operator <= (const Left  : Duration;
             const Right : Duration) : Boolean; inline;

{/= "<=" =============================================================\}
{                                                                      }
{\====================================================================/}
//-- @abstract(Compares two times.)
//-- @returns(@code(True) if the @code(Left) denotes a point in time not
//--          later than the one in @code(Right).)
operator <= (const Left  : Time;
             const Right : Time) : Boolean; inline;

{/= ">" ==============================================================\}
{                                                                      }
{\====================================================================/}
//-- @abstract(Compares two durations.)
//-- @returns(@code(True)  if the left operand denotes a longer duration
//--          than the right operand.)
operator > (const Left  : Duration;
            const Right : Duration) : Boolean; inline;

{/= ">" ==============================================================\}
{                                                                      }
{\====================================================================/}
//-- @abstract(Compares two times.)
//-- @returns(@code(True)  if  the  left operand denotes a point in time
//--          later than the one of the right operand.)
operator > (const Left  : Time;
            const Right : Time) : Boolean; inline;

{/= ">=" =============================================================\}
{                                                                      }
{\====================================================================/}
//-- @abstract(Compares two durations.)
//-- @returns(@code(True) if the left operand denotes an equal or longer
//--          duration than the right operand.)
operator >= (const Left  : Duration;
             const Right : Duration) : Boolean; inline;

{/= ">=" =============================================================\}
{                                                                      }
{\====================================================================/}
//-- @abstract(Compares two times.)
//-- @returns(@code(True)  if  the  left operand denotes a point in time
//--          not earlier than the one of the right operand.)
operator >= (const Left  : Time;
             const Right : Time) : Boolean; inline;

{/= "+" ==============================================================\}
{                                                                      }
{\====================================================================/}
//-- @abstract(Adds a duration to a point in time.)
//-- @returns(A  new point in time with @code(Right) duration apart from
//--          @code(Left).)
operator + (const Left  : Time;
            const Right : Duration) : Time; inline;

{/= "+" ==============================================================\}
{                                                                      }
{\====================================================================/}
//-- @abstract(Adds a duration to a point in time.)
//-- @returns(A  new  point in time with @code(Left) duration apart from
//--          @code(Right).)
operator + (const Left  : Duration;
            const Right : Time) : Time; inline;

{/= "+" ==============================================================\}
{                                                                      }
{\====================================================================/}
//-- @abstract(Adds up two durations.)
//-- @returns(The sum of both operands as a single duration.)
operator + (const Left  : Duration;
            const Right : Duration) : Duration; inline;

{/= "-" ==============================================================\}
{                                                                      }
{\====================================================================/}
//-- @abstract(Evaluates a duration between two times.)
//-- @returns(The time distance between both operands.)
operator - (const Left  : Time;
            const Right : Time) : Duration; inline;

{/= "-" ==============================================================\}
{                                                                      }
{\====================================================================/}
//-- @abstract(Subtracts a duration from a point in time.)
//-- @returns(A  new point in time with @code(Right) duration apart from
//--          @code(Left).)
operator - (const Left  : Time;
            const Right : Duration) : Time; inline;

{/= "*" ==============================================================\}
{                                                                      }
{\====================================================================/}
//-- @abstract(Multiplies a single duration a given number of times.)
//-- @returns(The duration @code(Left) multiplied by @code(Right).)
operator * (const Left  : Int64;
            const Right : Duration) : Duration; inline;

{/= "/" ==============================================================\}
{                                                                      }
{\====================================================================/}
//-- @abstract(Divides a single duration by a given number of times.)
//-- Despite  its name @code(/) this is an integer operation like @code(
//-- div),  so  it  may  be  inaccurate.  Especially the equation @code(
//-- Duration / x * x = Duration) may not hold true.
//-- @returns(The duration @code(Left) divided by @code(Right).)
operator / (const Left  : Duration;
            const Right : Int64) : Duration; inline;


{ -- Base subroutines ------------------------------------------------ }

{/= Clock ============================================================\}
{                                                                      }
{\====================================================================/}
//-- @abstract(Provides another system clock.)
//-- @returns(The ticks gone since the @link(BIG_BANG).)
//-- Due to its current mapping on the @code(SysUtils.Now) stuff this is
//-- not guaranteed to be very precise.
function Clock : Time;

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
//--                   Usually    initialized    with    @code(Clock   +
//--                   @italic(Some_Interval)).)
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
//--    Next     : Calendar.Time;
//--    Interval : Calendar.Duration;
//-- begin
//--    Interval := Calendar.Milliseconds (10);
//--    Next     := Calendar.Clock + Interval;
//--
//--    for i := Low (i) to High (i) do
//--    begin
//--       Calendar.Sleep_Until (Next);
//--       Next := Next + Interval;
//--    end {for};
//-- end {Absolute_Delay};
//-- )
procedure Sleep_Until (const Next : Time);


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
  overload;

{/= Hours ============================================================\}
{                                                                      }
{\====================================================================/}
//-- @abstract(Translates  a  given  number  of  hours into our duration
//--           type.)
//-- @param(Num_Hours   The  number  of  hours  to  be  converted  to  a
//--                    duration type.)
//-- @returns(The duration of given number of hours.)
function Hours (const Num_Hours : Discrete_Time) : Duration;
  overload;

{/= Minutes ==========================================================\}
{                                                                      }
{\====================================================================/}
//-- @abstract(Translates  a  given  number of minutes into our duration
//--           type.)
//-- @param(Num_Minutes   The  number  of  minutes  to be converted to a
//--                      duration type.)
//-- @returns(The duration of given number of minutes.)
function Minutes (const Num_Minutes : Discrete_Time) : Duration;
  overload;

{/= Seconds ==========================================================\}
{                                                                      }
{\====================================================================/}
//-- @abstract(Translates  a  given  number of seconds into our duration
//--           type.)
//-- @param(Num_Seconds   The  number  of  seconds  to be converted to a
//--                      duration type.)
//-- @returns(The duration of given number of seconds.)
function Seconds (const Num_Seconds : Discrete_Time) : Duration;
  overload;

{/= Milliseconds =====================================================\}
{                                                                      }
{\====================================================================/}
//-- @abstract(Translates  a  given  number  of  milliseconds  into  our
//--           duration type.)
//-- @param(Num_MSecs   The number of days to be converted to a duration
//--                    type.)
//-- @returns(The duration of given number of milliseconds.)
function Milliseconds (const Num_MSecs : Discrete_Time) : Duration;
  overload;


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

{/= Days =============================================================\}
{                                                                      }
{\====================================================================/}
//-- @abstract(Converts a duration into rounded number of days.)
//-- @param(Time_Span   The duration to convert.)
//-- @returns(Number of days specified by the @code(Time_Span).)
//-- This   is   a   convinience   function   using  @link(To_Time_Unit)
//-- internally.
function Days (const Time_Span : Duration) : Discrete_Time;
  overload;

{/= Hours ============================================================\}
{                                                                      }
{\====================================================================/}
//-- @abstract(Converts a duration into rounded number of hours.)
//-- @param(Time_Span   The duration to convert.)
//-- @returns(Number of hours specified by the @code(Time_Span).)
//-- This   is   a   convinience   function   using  @link(To_Time_Unit)
//-- internally.
function Hours (const Time_Span : Duration) : Discrete_Time;
  overload;

{/= Minutes ==========================================================\}
{                                                                      }
{\====================================================================/}
//-- @abstract(Converts a duration into rounded number of minutes.)
//-- @param(Time_Span   The duration to convert.)
//-- @returns(Number of minutes specified by the @code(Time_Span).)
//-- This   is   a   convinience   function   using  @link(To_Time_Unit)
//-- internally.
function Minutes (const Time_Span : Duration) : Discrete_Time;
  overload;


{/= Seconds ==========================================================\}
{                                                                      }
{\====================================================================/}
//-- @abstract(Converts a duration into rounded number of seconds.)
//-- @param(Time_Span   The duration to convert.)
//-- @returns(Number of seconds specified by the @code(Time_Span).)
//-- This   is   a   convinience   function   using  @link(To_Time_Unit)
//-- internally.
function Seconds (const Time_Span : Duration) : Discrete_Time;
  overload;

{/= Milliseconds =====================================================\}
{                                                                      }
{\====================================================================/}
//-- @abstract(Converts a duration into rounded number of milliseconds.)
//-- @param(Time_Span   The duration to convert.)
//-- @returns(Number of milliseconds specified by the @code(Time_Span).)
//-- This   is   a   convinience   function   using  @link(To_Time_Unit)
//-- internally.
function Milliseconds (const Time_Span : Duration) : Discrete_Time;
  overload;


{/= Nanoseconds ======================================================\}
{                                                                      }
{\====================================================================/}
//-- @abstract(Converts a duration into number of nanoseconds.)
//-- @param(Time_Span   The duration to convert.)
//-- @returns(Number of nanoseconds specified by the @code(Time_Span).)
//-- This  is  a  special  function to easify conversion.  It is neither
//-- accurate,  because  nanoseconds  are  already  subresolution of the
//-- @link(Duration) type, nor does there exist the usual counterpart of
//-- converting nanoseconds into a duration type.
function Nanoseconds (const Time_Span : Duration) : Discrete_Time;


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
      ({ Days        } (Value : TICKS_PER_DAY),
       { Hours       } (Value : TICKS_PER_HOUR),
       { Minutes     } (Value : TICKS_PER_MINUTE),
       { Seconds     } (Value : TICKS_PER_SECOND),
       { Millisecond } (Value : TICKS_PER_MILLISECOND));


{ -- Operators ------------------------------------------------------- }

{/= "=" ==============================================================\}
{                                                                      }
{\====================================================================/}
operator = (const Left  : Duration;
            const Right : Duration) : Boolean; inline;
begin
   exit (Left.Value = Right.Value);
end {"="};

operator = (const Left  : Time;
            const Right : Time) : Boolean; inline;
begin
   exit (Left.Value = Right.Value);
end {"="};


{/= "<" ==============================================================\}
{                                                                      }
{\====================================================================/}
operator < (const Left  : Time;
            const Right : Time) : Boolean; inline;
begin
   exit (Left.Value < Right.Value);
end {"<"};

operator < (const Left  : Duration;
            const Right : Duration) : Boolean; inline;
begin
   exit (Left.Value < Right.Value);
end {"<"};


{/= "<=" =============================================================\}
{                                                                      }
{\====================================================================/}
operator <= (const Left  : Duration;
             const Right : Duration) : Boolean; inline;
begin
   exit (Left.Value <= Right.Value);
end {"<="};

operator <= (const Left  : Time;
             const Right : Time) : Boolean; inline;
begin
   exit (Left.Value <= Right.Value);
end {"<="};


{/= ">" ==============================================================\}
{                                                                      }
{\====================================================================/}
operator > (const Left  : Duration;
            const Right : Duration) : Boolean; inline;
begin
   exit (Left.Value > Right.Value);
end {">"};

operator > (const Left  : Time;
            const Right : Time) : Boolean; inline;
begin
   exit (Left.Value > Right.Value);
end {">"};


{/= ">=" =============================================================\}
{                                                                      }
{\====================================================================/}
operator >= (const Left  : Duration;
             const Right : Duration) : Boolean; inline;
begin
   exit (Left.Value >= Right.Value);
end {">="};

operator >= (const Left  : Time;
             const Right : Time) : Boolean; inline;
begin
   exit (Left.Value >= Right.Value);
end {">="};


{/= "+" ==============================================================\}
{                                                                      }
{\====================================================================/}
operator + (const Left  : Time;
            const Right : Duration) : Time; inline;
begin
   Result.Value := Left.Value + Right.Value;
end {"+"};

operator + (const Left  : Duration;
            const Right : Time) : Time; inline;
begin
   Result.Value := Left.Value + Right.Value;
end {"+"};

operator + (const Left  : Duration;
            const Right : Duration) : Duration; inline;
begin
   Result.Value := Left.Value + Right.Value;
end {"+"};


{/= "-" ==============================================================\}
{                                                                      }
{\====================================================================/}
operator - (const Left  : Time;
            const Right : Time) : Duration; inline;
begin
   Result.Value := Left.Value - Right.Value;
end {"-"};

operator - (const Left  : Time;
            const Right : Duration) : Time; inline;
begin
   Result.Value := Left.Value - Right.Value;
end {"-"};


{/= "*" ==============================================================\}
{                                                                      }
{\====================================================================/}
operator * (const Left  : Int64;
            const Right : Duration) : Duration; inline;
begin
   Result.Value := Left * Right.Value;
end {"*"};


{/= "/" ==============================================================\}
{                                                                      }
{\====================================================================/}
operator / (const Left  : Duration;
            const Right : Int64) : Duration; inline;
begin
   Result.Value := Left.Value div Right;
end {"/"};


{ -- Base subroutines ------------------------------------------------ }

{/= Clock ============================================================\}
{                                                                      }
{\====================================================================/}
function Clock : Time;
begin
   exit (Time(Duration_Since (BIG_BANG)));
end {Clock};


{/= Duration_Since ===================================================\}
{                                                                      }
{\====================================================================/}
function Duration_Since (const Past_Time : tDateTime) : Duration;
begin
   Result.Value := TICKS_PER_MILLISECOND *
                   DateUtils.MillisecondsBetween (Past_Time,
                                                  SysUtils.Now);
end {Duration_Since};


{/= Sleep_Until ======================================================\}
{                                                                      }
{\====================================================================/}
procedure Sleep_Until (const Next : Time);
var
   Right_Now : Time;
begin
   Right_Now := Clock;

   if Right_Now < Next then
      SysUtils.Sleep (Milliseconds (Next - Right_Now));
end {Sleep_Until};


{ -- Subroutines providing "constants" ------------------------------- }

{/= Days =============================================================\}
{                                                                      }
{\====================================================================/}
function Days (const Num_Days : Discrete_Time) : Duration;
begin
   Result.Value := Num_Days * TICKS_PER_DAY;
end {Days};


{/= Hours ============================================================\}
{                                                                      }
{\====================================================================/}
function Hours (const Num_Hours : Discrete_Time) : Duration;
begin
   Result.Value := Num_Hours * TICKS_PER_HOUR;
end {Hours};


{/= Minutes ==========================================================\}
{                                                                      }
{\====================================================================/}
function Minutes (const Num_Minutes : Discrete_Time) : Duration;
begin
   Result.Value := Num_Minutes * TICKS_PER_MINUTE;
end {Minutes};


{/= Seconds ==========================================================\}
{                                                                      }
{\====================================================================/}
function Seconds (const Num_Seconds : Discrete_Time) : Duration;
begin
   Result.Value := Num_Seconds * TICKS_PER_SECOND;
end {Seconds};


{/= Milliseconds =====================================================\}
{                                                                      }
{\====================================================================/}
function Milliseconds (const Num_MSecs : Discrete_Time) : Duration;
begin
   Result.Value := Num_MSecs * TICKS_PER_MILLISECOND;
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
   Result.Value := Round (Date_Time * TICKS_PER_DAY);
end {To_Duration};


{/= To_Time_Unit =====================================================\}
{                                                                      }
{\====================================================================/}
function To_Time_Unit (const Time_Span : Duration;
                       const Time_Unit : Accuracy) : Discrete_Time;
begin
   exit (Round (1.0 * Time_Span.Value / Durations[Time_Unit].Value));
end {To_Time_Unit};


{/= Days =============================================================\}
{                                                                      }
{\====================================================================/}
function Days (const Time_Span : Duration) : Discrete_Time;
begin
   exit (To_Time_Unit (Time_Span, acDays));
end {Days};


{/= Hours ============================================================\}
{                                                                      }
{\====================================================================/}
function Hours (const Time_Span : Duration) : Discrete_Time;
begin
   exit (To_Time_Unit (Time_Span, acHours));
end {Hours};


{/= Minutes ==========================================================\}
{                                                                      }
{\====================================================================/}
function Minutes (const Time_Span : Duration) : Discrete_Time;
begin
   exit (To_Time_Unit (Time_Span, acMinutes));
end {Minutes};


{/= Seconds ==========================================================\}
{                                                                      }
{\====================================================================/}
function Seconds (const Time_Span : Duration) : Discrete_Time;
begin
   exit (To_Time_Unit (Time_Span, acSeconds));
end {Seconds};


{/= Milliseconds =====================================================\}
{                                                                      }
{\====================================================================/}
function Milliseconds (const Time_Span : Duration) : Discrete_Time;
begin
   exit (To_Time_Unit (Time_Span, acMilliseconds));
end {Milliseconds};


{/= Nanoseconds ======================================================\}
{                                                                      }
{\====================================================================/}
function Nanoseconds (const Time_Span : Duration) : Discrete_Time;
begin
   exit (Time_Span.Value * (1000 div TICKS_PER_MILLISECOND) * 1000);
end {Nanoseconds};


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
   My_Duration := Abs (1.0 * Time_Span.Value / TICKS_PER_DAY) +
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
