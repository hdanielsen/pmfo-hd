 
 /*------------------------------------------------------------------------
    File        : DatasetErrorProcessor
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Sat Apr 25 17:35:44 EDT 2020
    Notes       : 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using Progress.Json.ObjectModel.JsonObject from propath.
using Pmfo.Core.Service.JsonDatasetErrorProcessor from propath.

block-level on error undo, throw.

class Pmfo.Core.BusinessLogic.DatasetErrorProcessor:
    
    method public void AssertDataset(phdataset as handle ):
        define variable oError as Error no-undo.
        oError = CreateError(phdataset).
        if valid-object(oError) then
            undo, throw oError.
    end method.
     
    method public Error CreateError(phdataset as handle ):
        define variable ojson      as JsonObject no-undo.
        define variable oErrorProcessor as JsonDatasetErrorProcessor no-undo.
        if phDataset:Error then
        do:
            oJson = new JsonObject(). 
            phDataset:write-json ("JsonObject",ojson,no,?,no,yes, yes).
            if oJson:has("prods:errors") then
            do:
                oErrorProcessor = new JsonDatasetErrorProcessor().
                return oErrorProcessor:CreateSimpleError(ojson,phDataset).
            end.
        end.
        return ?. 
    end method.

end class.