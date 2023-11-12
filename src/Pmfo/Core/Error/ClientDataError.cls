 
 /*------------------------------------------------------------------------
    File        : ClientDataError
    Purpose     : Stores before-buffer errors as Json in the error message 
                  in order to keep track of field names and ordinal position in the Json    
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Sat Jun 15 14:44:08 EDT 2019
    Notes       : The JsonDatasetErrorProcessor will convert all before buffer errors to this
                  Error, adding a message for each error. 
                  (the server may add multiple messages to one buffer)   
                  The DOH or error handler/writer will convert this to regular json.
                  The GetMessage override also ensures that the message looks like 
                  a regular message if catched in ABL (the field info will not be there) 
  ----------------------------------------------------------------------*/
block-level on error undo, throw.

using OpenEdge.Net.HTTP.StatusCodeEnum from propath.
using Pmfo.Core.Error.ApplicationError from propath.
using Pmfo.Util.JsonUtil from propath.
using Progress.Json.ObjectModel.JsonObject from propath.

class Pmfo.Core.Error.ClientDataError inherits ApplicationError: 
     
    constructor public ClientDataError ( input pmsg as character ):
        super (input pmsg).
    end constructor.

    constructor public ClientDataError ( input pmsg as character, input piNum as integer ):
        super (input pmsg, input piNum).
    end constructor.
    
    method override public JsonObject GetError( input pindex as integer ):
        define variable oJson as JsonObject no-undo.
        define variable cMsg as character no-undo. 
        do on error undo, throw:
           cMsg = super:GetMessage(pindex).
           oJson = JsonUtil:ReadJson(cMsg).
           return oJson.
           catch e as Progress.Lang.Error :
            // ignore - resort to super below
           end catch.
        end.
        return super:GetError(pindex).
    end method.      
      
    method override public character GetMessage( input pindex as integer ):
        define variable oJson as JsonObject no-undo.
        define variable cmsg as character no-undo.
        
        cMsg = super:GetMessage(pindex).
        do on error undo,throw:
            ojson = JsonUtil:ReadJson(cMsg).
            cMsg = ojson:GetCharacter(MessageNodeName).
            return cMsg.
            catch e as Progress.Lang.Error :
              // nothing return plain message below  
            end catch.
        end.    
        return cMsg.
    end method.

    method override protected StatusCodeEnum GetStatusCodeEnum(  ):
        return StatusCodeEnum:BadRequest.
    end method.    
        
end class.