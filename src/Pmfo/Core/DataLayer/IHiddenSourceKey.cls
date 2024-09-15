
/*------------------------------------------------------------------------
    File        : IHiddenSourceKey
    Purpose     : Extends ISourceField to allow key assignment to fields not exposed to BL
                  This is used for keys that need to be assigned to the source at create (from sequence or similar)
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Sat Dec 03 09:34:20 EST 2022
    Notes       : This is split from IConstantField sisnce it is to be used to assign a new key value on create.
                  BUT this means that it cannot be used for a key that is defined in the UpdateTarget GetDataSourceKeys
                  since there is no way to use it in a unique find since we cannot build a find where 
                  from values in the temp-table record's before values
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using Pmfo.Core.DataLayer.ISourceField.

interface Pmfo.Core.DataLayer.IHiddenSourceKey inherits ISourceField:  
    method public logical Prepare(phDbfield as handle).
     
end interface.