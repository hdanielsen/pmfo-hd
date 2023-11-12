 
 /*------------------------------------------------------------------------
    File        : IIntegerCharacterMap
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Tue May 09 09:19:34 EDT 2023
    Notes       : 
  ----------------------------------------------------------------------*/

block-level on error undo, throw.

&scop interfacename IIntegerCharacterMap
&scop valuetype character
&scop keytype integer 
&scop valueinterface Ccs.Common.Support.ICharacterHolder
&scop keyinterface Ccs.Common.Support.IIntegerHolder 

{Pmfo/Core/Collections/PrimitiveMapInterface.i}
 