 
 /*------------------------------------------------------------------------
    File        : JsonBlobReadHandler
    Purpose     : Manage image data where the image may have leading data: 
                  like "data:image/jpeg;base64," before data
                    
    Syntax      : 
    Description : The IJsonReadHandler interface that this implements is currently 
                  supported and used by the JsonReader  
                  The BusinessEntityOperationHandler will do the required checks to 
                  see if its defined by a BusinessEntity  
                  The details of this bootstrapping is in flux and likely to change
                  and thus not documented here...
    Author(s)   : hdaniels
    Created     : Sep 2020
    Notes       : Currently only manages image/  and  base64 in meta data
  ----------------------------------------------------------------------*/


block-level on error undo, throw.

using Ccs.Common.Application from propath.
using Pmfo.Core.BusinessLogic.IJsonReadHandler from propath.
using Pmfo.Core.BusinessLogic.RowStateEnum from propath.
using Pmfo.Core.Common.IMimeTypes from propath.
using Pmfo.Core.Error.BadRequestError from propath.
using Pmfo.Core.Error.UnprocessableEntityError from propath.
using Pmfo.Core.Error.UnsupportedOperationError from propath.
using Pmfo.Util.BufferUtil from propath.
using Pmfo.Util.StringUtil from propath.
using Progress.Json.ObjectModel.JsonObject from propath.

class Pmfo.Core.BusinessLogic.JsonBlobReadHandler implements IJsonReadHandler: 
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    define public    property ImageFieldName          as character no-undo get. protected set.
    define public    property FileTypeFieldName       as character no-undo get. protected set.
    define public    property TableSerializeName as character no-undo get. protected set.
    define public    property StoreMimeTypeExtension  as logical   no-undo get. protected set.
    
    define public    property MimeTypes              as IMimeTypes   no-undo 
         get():
             if not valid-object (MimeTypes) then 
             do:
                 MimeTypes = cast(Application:ServiceManager:getService(get-class(IMimeTypes)),IMimeTypes).
             end.    
             return MimeTypes.     
         end.     
         protected set.
   
    define protected property BufferHandle            as handle    no-undo get. set.
         
    constructor public JsonBlobReadHandler (pcTableSerializeName as character, pcImageFieldName as character, pcFileTypeFieldName as character ):
        super ().
        assign
            ImageFieldName     = pcImageFieldName
            FileTypeFieldName  = pcFileTypeFieldName
            TableSerializeName = pcTableSerializeName
            .
    end constructor.
    
    
    // prepare for load of buffer (for example load support data needed for ResdField) and  
    // return true if read only table (skip mark state)
    method public logical CheckReadOnlyAndPrepareBuffer( phBuffer as handle, pState as RowStateEnum).
         
        if phbuffer:table-handle:serialize-name = TableSerializeName then 
            BufferHandle = phBuffer.
        else
            BufferHandle = ?.       
        return false.
    end method.

    method public logical ReadField( pjson as JsonObject,phfld as handle):
        define variable lError       as logical  no-undo.
        define variable cLong        as longchar no-undo.
        define variable iFirstComma  as integer no-undo.
        define variable cImageString as character no-undo.
        define variable cFileType    as character no-undo.
        define variable hFileType    as handle no-undo.
         
        if valid-handle(BufferHandle) then
        do: 
           
            // return true to signal that it will be read here
            //  filetype is always managed when we read the imagefieldnam sionce we need to check 
            //  if it has inline image or not )
            if phfld:name = FileTypeFieldName then
                return true.
            
            else if phfld:name = ImageFieldName then
            do:
                cLong = pjson:Getlongchar(phFld:serialize-name).
                
                hFileType = BufferHandle:buffer-field(FileTypeFieldName).
                if pjson:Has(hFileType:serialize-name) then
                do:
                    cFileType = pjson:GetCharacter(hFileType:serialize-name).
                    AssertFileType(cFileType).
                end.
                if cLong begins "data:" then 
                do:
                    if cFileType > "" then
                        undo, throw new BadRequestError(subst("&1 should not be specified as a property when it is provided inline in the &2 data",
                                                              BufferUtil:GetFieldLabel(hFileType),
                                                              BufferUtil:GetFieldLabel(phfld))
                                                        ).
                   
                    assign 
                        iFirstComma  = index(cLong,",") 
                        cImageString = substr(cLong,1,iFirstComma - 1)
                        cLong        = substr(cLong,iFirstComma + 1)
                        cFileType    = GetFileType(cImageString) 
                        .
                    
                end.
                else if cFileType = "" then
                    undo, throw new BadRequestError(subst("&1 should be specified for image (unless media type is provided inline in the &2 data)",
                                                         BufferUtil:GetFieldLabel(hFileType),
                                                         BufferUtil:GetFieldLabel(phfld))
                                                         ).
                
                phfld:buffer-value = base64-decode(cLong).
                hFileType:buffer-value = cFiletype.
                return true. 
            end. // read image */
        end.
        return false.
        catch e1 as UnsupportedOperationError :
            undo, throw new UnprocessableEntityError(subst("Unsupported media type provided in &1",BufferUtil:GetFieldLabel(BufferHandle:buffer-field(ImageFieldName))),e1).
        end catch.
        
        catch e2 as Progress.Lang.Error :
            undo, throw e2.    
        end catch.
          
    end method.   
    
    method protected void AssertFileType(pcFileType as character):
        if StoreMimeTypeExtension = false then // if we store the entire  mimetype assert it
            AssertMimeType(pcFileType, ""). // second parameter is just for error message 
       // else if we only store the extension check if it is valid
        else do:
            if not MimeTypes:IsValidFileExtension(pcfileType) then
                undo, throw new BadRequestError(subst("File type '&1' is not supported. The supported file types are &2.",
                                                       pcFileType,
                                                       StringUtil:GetMessageFromEntries(MimeTypes:ValidExtensions)
                                                       )
                                                ).
        end.    
    end method.
    
    // we allow delete and put without image so read the filetype from JSON  whenever we find that image is not in json  
    method public void HandleFieldNotInJson( pjson as JsonObject, phfld as handle).
        if phfld:name = ImageFieldName then 
        do:
            AssignFileTypeFromJson(pJson). 
        end.
    end method.
    
    method protected void AssignFileTypeFromJson (pJson as JsonObject) :
        define variable cFileType as character no-undo. 
        define variable hFileType as handle no-undo.
        
        hFileType = BufferHandle:buffer-field(FileTypeFieldName).
        if pjson:Has(hFileType:serialize-name) then
        do:
            cFileType = pjson:GetCharacter(hFileType:serialize-name).
            AssertFileType(cFileType).  
            hFileType:buffer-value = cFiletype.    
        end.
    end method.    
    
    // pcTypeString is optional for the case when this is extracted from the blob 
    method protected void AssertMimeType(pcMimeType as character,pcTypeString as character):
        if not MimeTypes:IsValidMimeType(pcMimeType) then 
             undo, throw new UnsupportedOperationError(subst("Mime type '&1' &2is not supported. Only &3 are supported.'",
                                                             pcMimeType,
                                                             if pcTypeString > "" then subst("in '&1' ",pctypeString) else "", 
                                                             StringUtil:GetMessageFromEntries(MimeTypes:ValidMimeTypes)
                                                             )
                                                       ).
         
    end method.     
    
    method protected character GetFileType(pcMimeTypestring as character  ):
        /*------------------------------------------------------------------------------
        Purpose:
        Notes:
        ------------------------------------------------------------------------------*/    
        define variable cFiletype as character no-undo.
        define variable cEncode as character no-undo.
        define variable cMimeType as character no-undo.
        
        assign    
            cMimeType = entry(2,pcMimeTypestring,":")
            cEncode    = entry(2,cMimeType,";")
            cMimeType = entry(1,cMimeType,";").
        
        AssertMimeType(cMimeType, pcMimeTypestring).
        
        assign    
            // @TODO - this file extension logoc does not does not work for all cases
            cFileType  = if StoreMimeTypeExtension then MimeTypes:GetMimeTypeFileExtension(cMimeType) 
                         else cMimeType
                         .             
             
         if cEncode <> "base64" then
             undo, throw new UnsupportedOperationError(subst("Image encode value '&1' in '&2' is not supported. Expected value is 'base64'.",cencode,pcMimeTypestring)).
         
        return cfileType.
    end method.   


end class.