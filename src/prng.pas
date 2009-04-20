{/= PRNG =============================================================\}
{                                                                      }
{ Generic interface to Pseudo Random Number Generators.                }
{ Part of the Stop-Me (STOchastic Performance MEasurement) project.    }
{                                                                      }
{ Copyright (C) 2005, 2006, 2009 by Johnny L. Fencey                   }
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

//-- @abstract(Provides   the   abstract   class   interface   for   the
//--           implementation of Pseudo Random Number Generators.)
unit
   PRNG;


interface


type
   //-- @abstract(Abstract PRNG class (interface).)
   tPRNG = class (tObject)
      {/= Create =====================================================\}
      {                                                                }
      {\==============================================================/}
      //-- @abstract(Supposed to set up internal state etc.)
      constructor Create; virtual; abstract;

      {/= Next =======================================================\}
      {                                                                }
      {\==============================================================/}
      //-- @abstract(Supposed to provide the next PRN in the sequence.)
      //-- @returns(The next pseudo random number.)
      function Next : QWord; virtual; abstract;

      {/= Reset ======================================================\}
      {                                                                }
      {\==============================================================/}
      //-- @abstract(Resets the PRNG.)
      //-- @param(Seed   The initial seed value for the PRNG.)
      procedure Reset (const Seed : QWord); virtual; abstract;
   end {tPRNG};

type
   //-- @abstract(Class  pointer to be used for dynamic instantiation of
   //--           derived classes.)
   PRNG_Class = class of tPRNG;


implementation


end {PRNG}.

$Id$
