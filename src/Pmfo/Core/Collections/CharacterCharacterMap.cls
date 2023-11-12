/************************************************
Copyright (c) 2014, 2019-2021, 2023 by Progress Software Corporation. All rights reserved.
*************************************************/
 /*------------------------------------------------------------------------
    File        : CharacterCharacterMap
    Purpose     : Map of character values 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Fri Apr 21 17:34:16 EDT 2023
    Notes       : 
  ----------------------------------------------------------------------*/


block-level on error undo, throw.

&scop classname CharacterCharacterMap
&scop interface Pmfo.Core.Collections.ICharacterCharacterMap

&scop keytype character
&scop keyinterface Ccs.Common.Support.ICharacterHolder 
&scop keyclass Pmfo.Core.Common.CharacterHolder 

&scop valuetype character
&scop valueinterface Ccs.Common.Support.ICharacterHolder
&scop valueclass Pmfo.Core.Common.CharacterHolder

{Pmfo/Core/Collections/PrimitiveMap.i}

 