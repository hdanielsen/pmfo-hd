/************************************************/
/*------------------------------------------------------------------------
    File        : IIntegerDecimalMap 
    Purpose     : A typed IntegerHolder/DecimalHolder Map 
    Syntax      : 
    Description : 
    Author(s)   : based on pjudge other data types
    Created     :  
    Notes       : contains IMap methods using ABL primitives instead 
                  of OpenEdge.Core.Decimal
                  We want exact type for ABL overload resolution in integration  
  ----------------------------------------------------------------------*/


&scop interfacename IIntegerDecimalMap
&scop valuetype decimal 
&scop keytype integer 
&scop valueinterface Ccs.Common.Support.IDecimalHolder
&scop keyinterface Ccs.Common.Support.IIntegerHolder 

{Pmfo/Core/Collections/PrimitiveMapInterface.i}