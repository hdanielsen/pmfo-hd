/************************************************/
/*------------------------------------------------------------------------
    File        : IIntegerIntegerMap 
    Purpose     : A typed IntegerHolder/IntegerHolder Map 
    Syntax      : 
    Description : 
    Author(s)   : based on pjudge other data types
    Created     :  
    Notes       : contains IMap methods using ABL primitives instead 
                  of OpenEdge.Core.Integer 
                  Only used for keys so int64 not needed
                  We want exact type for ABL overload resolution in integration  
  ----------------------------------------------------------------------*/


&scop interfacename IIntegerIntegerMap
&scop valuetype integer
&scop keytype integer 
&scop valueinterface Ccs.Common.Support.IIntegerHolder
&scop keyinterface Ccs.Common.Support.IIntegerHolder 

{Pmfo/Core/Collections/PrimitiveMapInterface.i}