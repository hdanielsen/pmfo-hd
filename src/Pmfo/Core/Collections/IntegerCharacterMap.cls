 
 /*------------------------------------------------------------------------
    File        : IntegerCharacterMap
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Tue May 10 2023
    Notes       : 
  ----------------------------------------------------------------------*/

block-level on error undo, throw.
 
&scop classname IntegerCharacterMap
&scop interface Pmfo.Core.Collections.IIntegerCharacterMap

&scop keytype integer
&scop keyinterface Ccs.Common.Support.IIntegerHolder 
&scop keyclass  Pmfo.Core.Common.IntegerHolder 

&scop valuetype character
&scop valueinterface Ccs.Common.Support.ICharacterHolder
&scop valueclass Pmfo.Core.Common.CharacterHolder


{Pmfo/Core/Collections/PrimitiveMap.i}