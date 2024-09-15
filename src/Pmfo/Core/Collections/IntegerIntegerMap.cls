/************************************************
Copyright (c) 2014, 2019-2021, 2023 by Progress Software Corporation. All rights reserved.
*************************************************/
 /*------------------------------------------------------------------------
    File        : IntegerIntegerMap
    Purpose     : Map of integer and integer values 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Fri Apr 21 17:34:16 EDT 2023
    Notes       : 
  ----------------------------------------------------------------------*/


block-level on error undo, throw.

&scop classname IntegerIntegerMap
&scop interface Pmfo.Core.Collections.IIntegerIntegerMap

&scop keytype integer
&scop keyinterface Ccs.Common.Support.IIntegerHolder 
&scop keyclass Pmfo.Core.Common.IntegerHolder 

&scop valuetype Integer
&scop valueinterface Ccs.Common.Support.IIntegerHolder
&scop valueclass Pmfo.Core.Common.IntegerHolder

{Pmfo/Core/Collections/PrimitiveMap.i}

 