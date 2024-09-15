/************************************************
Copyright (c) 2014, 2019-2021, 2023 by Progress Software Corporation. All rights reserved.
*************************************************/
 /*------------------------------------------------------------------------
    File        : IntegerDecimaIMap
    Purpose     : Map of integer and decimal values 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : 3/2/2024 
    Notes       : 
  ----------------------------------------------------------------------*/


block-level on error undo, throw.

&scop classname IntegerDecimalMap
&scop interface Pmfo.Core.Collections.IIntegerDecimalMap

&scop keytype integer
&scop keyinterface Ccs.Common.Support.IIntegerHolder 
&scop keyclass Pmfo.Core.Common.IntegerHolder 

&scop valuetype Decimal 
&scop valueinterface Ccs.Common.Support.IDecimalHolder
&scop valueclass OpenEdge.Core.Decimal

{Pmfo/Core/Collections/PrimitiveMap.i}

 