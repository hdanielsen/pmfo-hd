 
 /*------------------------------------------------------------------------
    File        : ImageTypes
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Fri Jun 04 12:21:45 EDT 2021
    Notes       : 
  ----------------------------------------------------------------------*/

 

block-level on error undo, throw.

using OpenEdge.Core.Collections.ICollection from propath.
using OpenEdge.Core.Collections.IIterator from propath.
using OpenEdge.Core.Collections.IStringStringMap from propath.
using OpenEdge.Core.Collections.StringStringMap from propath.
using OpenEdge.Core.String from propath.
using OpenEdge.DataAdmin.Error.IllegalOperationError from propath.
using Pmfo.Core.Common.IMimeTypes from propath.

class Pmfo.Core.Common.MimeTypes implements IMimeTypes: 
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    // allow old windowa 3 char filetypes   
    define private property OldExtensions as character init "jpg,htm,pgm" no-undo get.
    
    define private property ImageMap as IStringStringMap no-undo 
        get():
            if not valid-object (ImageMap) then
                ImageMap = new StringStringMap().
            return ImageMap.
        end.        
        set.  
    
    define property ValidMimeTypes  as character 
        get():
            if ValidMimeTypes = "" then 
               ValidMimeTypes = GetList(ImageMap:KeySet).
            return ValidMimeTypes.   
        end.    
        protected set.
    
    define property ValidExtensions as character 
        get():
            if ValidExtensions = "" then
            do: 
                ValidExtensions = GetList(ImageMap:Values) + "," + OldExtensions.
            end.
            return ValidExtensions.   
        end.    
        protected set.
     
        
    constructor public MimeTypes (  ):
        super ().
    end constructor.

    method public character GetMimeTypeFileExtension( input pcMimetype as character ):
        define variable cExtension as character no-undo.
        cExtension = string(ImageMap:Get(pcMimeType)).
        if cExtension = ? then 
            undo, throw new IllegalOperationError(subst("Mime Type '&1' is not defined. Cannot get file extension.",pcMimetype)).
        return cExtension.  
    end method.

    method public logical IsValidFileExtension( input pcExtension as character ):
        define variable lOk as logical no-undo.
        lok = ImageMap:ContainsValue(pcExtension).
        if not lok then 
            return lookup(pcExtension,OldExtensions) > 0.
        else 
            return true.    
    end method.

    method public logical IsValidMimeType( input pcType as character ):
        return ImageMap:ContainsKey(pcType).
    end method.
    
    method protected character GetList(pCollection as ICollection):
        define variable oIterator as IIterator no-undo.
        define variable oString   as String no-undo.
        define variable cDlm      as character no-undo.
        define variable cList    as character no-undo.
        oIterator = pCollection:Iterator().
        do while oIterator:HasNext():
            oString = cast(oIterator:Next(),String).
            cList = cList + cdlm + string(oString:Value).
            cDlm = ",".
        end.     
        return cList. 
    end method.
    
    method public void Initialize().
        ImageMap:put("application/vnd.ms-powerpoint.addin.macroEnabled.12","ppam"). 
        ImageMap:put("application/vnd.ms-powerpoint.slideshow.macroEnabled.12","ppsm"). 
        ImageMap:put("application/vnd.ms-powerpoint.presentation.macroEnabled.12","pptm").
        ImageMap:put("application/vnd.ms-powerpoint.template.macroEapplication/vnd.ed.12","potm").
        ImageMap:put("application/pdf","pdf").
        ImageMap:put("application/vnd.ms-excel.addin.macroEnabled.12","xlam").
        ImageMap:put("application/vnd.ms-excel.sheet.binary.macroEnabled.12","xlsb").
        ImageMap:put("application/vnd.ms-excel.sheet.macroEnabled.12","xlsm").
        ImageMap:put("application/vnd.ms-excel.template.macroEnabled.12","xltm").
        ImageMap:put("application/vnd.ms-word.document.macroEnabled.12","docm").
        ImageMap:put("application/vnd.ms-word.template.macroEnabled.12","dotm").
        ImageMap:put("application/vnd.openxmlformats-officedocument.wordprocessingml.document","docx").
        ImageMap:put("application/vnd.openxmlformats-officedocument.wordprocessingml.template","dotx").
        ImageMap:put("application/vnd.openxmlformats-officedocument.presentationml.presentation","pptx").
        ImageMap:put("application/vnd.openxmlformats-officedocument.presentationml.template","potx").
        ImageMap:put("application/vnd.openxmlformats-officedocument.presentationml.slideshow","ppsx").
        ImageMap:put("application/vnd.ms-excel","xls").
        ImageMap:put("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet","xlsx").
        ImageMap:put("application/vnd.openxmlformats-officedocument.spreadsheetml.template","xltx").
        ImageMap:put("audio/mpeg","mp3").
        ImageMap:put("image/bmp","bmp").
        ImageMap:put("image/jpeg","jpeg").
        ImageMap:put("image/gif","gif").
        ImageMap:put("image/png","png").
        ImageMap:put("image/vnd.microsoft.icon","ico").
        ImageMap:put("text/html","html").
        ImageMap:put("text/plain","txt").
        ImageMap:put("video/mp4","mp4").
        ImageMap:put("video/quicktime","mov").
        ImageMap:put("video/x-msvideo","avi").
        ImageMap:put("video/x-ms-wmv","wmv").
    end method.
    
    method public void Destroy().
       
    end method.
    
    destructor public MimeTypes ( ):
        Destroy().  
    end destructor.

    

end class.