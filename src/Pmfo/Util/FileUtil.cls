 
 /*------------------------------------------------------------------------
    File        : FileUtil
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Mon Mar 25 08:26:35 EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/


block-level on error undo, throw.

using OpenEdge.Core.Session from propath.
using OpenEdge.Core.StringConstant from propath.

class Pmfo.Util.FileUtil: 
    
    
    method static public character AppendToPath(pcPath as char, pcAppend as char):
        define variable cNewPath as character no-undo.
        assign
            pcPath   = replace(pcPath,StringConstant:BACKSLASH,"/")
            pcAppend = replace(pcAppend,StringConstant:BACKSLASH,"/")
            .
        return right-trim(pcPath,"/") + "/" + left-trim(pcAppend,"/").
    end method.    

    method static public logical IsDirectory(pcPath as char):
        file-info:file-name = pcPath.
        return file-info:full-pathname <> ? and file-info:file-type matches "*D*".
    end method.
    
    method static public datetime-tz ModTime(pcFile as char):
        file-info:file-name = pcFile.
        if file-info:full-pathname <> ? then
            return datetime-tz(file-info:file-mod-date,file-info:file-mod-time * 1000). 
        else 
            return ?.     
    end method.   
    
    method static public character GetFileName(pFixedName as character, pcExtension as character):
         return GetFileName(session:temp-dir, pFixedName, pcExtension).
    end method. 
    
    method static public character GetFileName(pcDirectory as character,pFixedName as character, pcExtension as character):
        define variable cBase           as integer.
        define variable cCheckName      as character.
        define variable cName           as character no-undo.
        
        pcDirectory = replace(pcDirectory,StringConstant:BACKSLASH,"/"). 
        /*
         * Loop until we find a name that hasn't been used. In theory, if the
         * temp directory gets filled, this could be an infinite loop. But, the
         * likelihood of that is low.
         */
        cCheckName = "something".
        do while cCheckName <> ?:
            /* Take the lowest 5 digits (change the format so that everything works out to have exactly 5  characters. */
            assign
                cbase = ( time * 1000 + etime ) modulo 100000
                cName = string(cbase,"99999":U)
                cName = right-trim(pcDirectory,"/")+ "/"  + pFixedName + iso-date(today) + "-" + cName + "." + pcExtension
                cCheckName = search(cName)
                .
        end.
        return cName.
    end method.    
 
    
end class.