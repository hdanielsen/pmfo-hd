/************************************************/
/*------------------------------------------------------------------------
    File        : ICharacterCharacterMap 
    Purpose     : A typed CharacterHolder/CharacterHolder Map 
    Syntax      : 
    Description : 
    Author(s)   : pjudge
    Created     : Wed Dec 18 13:58:44 EST 2013
    Notes       : contains IMap methods using ABL primitives instead 
                  of OpenEdge.Core.String  
  ----------------------------------------------------------------------*/


&scop interfacename ICharacterCharacterMap
&scop valuetype character
&scop keytype character 
&scop valueinterface Ccs.Common.Support.ICharacterHolder
&scop keyinterface Ccs.Common.Support.ICharacterHolder 

{Pmfo/Core/Collections/PrimitiveMapInterface.i}
 
 