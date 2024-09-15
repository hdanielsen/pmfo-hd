/*------------------------------------------------------------------------
  This Software is licensed by Progress Software Corporation (licensor)
  under the Progress Software Common Component Specification Project
  Release License Agreement available at
  https://community.progress.com/products/directions/common_component/p/releaselicenseagreement

  ----------------------------------------------------------------------*/
/*------------------------------------------------------------------------
    File        : ModuloQueryOperatorEnum
    Purpose     : Enum for values of the modulo Operator property of the
                  ModuloQueryPredicate 

    Syntax      :
    Description :
    Author(s)   :  
    Created     : Sun May 22 19:44:59 CEST 2016
    Notes       : Cannot use the QueryoperatorEnum since it is used to indentyfy 
                  equals matches and range matches and this is not backed by index  
  ----------------------------------------------------------------------*/

enum Pmfo.Core.Common.ModuloQueryOperatorEnum:

    define enum /* Operators requiring a single value */
                Eq
                Ge
                Gt
                Le
                Lt
                Ne      // ad-hoc added
                .
end enum .
