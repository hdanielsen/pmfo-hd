/*------------------------------------------------------------------------
    File        : File.cls
    Purpose     : Common file manipulation class.

    Author(s)   : Dustin Grau
    Created     : Fri Jun 29 08:00:00 EDT 2012
    Notes       :
  ----------------------------------------------------------------------*/

&GLOBAL-DEFINE THROW ON ERROR UNDO, THROW

block-level on error undo, throw.
 
using Progress.Lang.*.

class Pmfo.Util.FileIO:

    /************************ Properties and Class Methods ************************/

{Pmfo/Util/filedata.i}
    
    /*************************** Public Static Methods ***************************/


    method public static logical fileExists ( input pcFileName as character ):
        assign file-info:file-name = pcFileName.
        if file-info:full-pathname eq ? then
            return false.
        else
            return true.
    end method. /* fileExists */


    method public static character getFileName ( input pcFilePath as character ):
        /* Strips and returns the actual filename from a file path. */
        if num-entries(pcFilePath, "~\") < 1 then
            return "".
        else
            return entry(num-entries(pcFilePath, "~\"), pcFilePath, "~\").
    end method. /* getFileName */


    method public static character getFilePath ( input pcFilePath as character ):
        /* Strips the actual filename and returns the remaining path. */
        if pcFilePath gt "" then
            return replace(pcFilePath, getFileName(pcFilePath), "").
        else
            return pcFilePath.
    end method. /* getFilePath*/


    method public static character getExt ( input cFileName as character ):
        /* Strips the file extension from a file name/path. */
        if num-entries(cFileName, "~.") < 2 then
            return "".
        else
            return entry(num-entries(cFileName, "~."), cFileName, "~.").
    end method. /* getExt */


    method public static character getMimeType ( input pcExt as character ):
        define variable cExtList  as character no-undo.
        define variable cTypeList as character no-undo.
        define variable cIndex    as integer   no-undo.

        assign /* Supported or known MIME types for this application. */
            cExtList  = "htm,html,shtml,css,js,txt,csv,xml,json"
                      + ",gif,jpg,jpeg,png,tif,tiff,bmp"
                      + ",pdf,doc,xls,ppt,odt,ods,odc,odp"
                      + ",aac,ogg,mp4,mp3,mpeg,mpg,mpe,avi,mov"
                      + ",swf"
            cTypeList = "text/html,text/html,text/html,text/css,text/javascript,text/plain,text/csv,text/xml,text/json"
                      + ",image/gif,image/jpeg,image/jpeg,image/png,image/tiff,image/tiff,image/bmp"
                      + ",application/pdf,application/msword,application/vnd.ms-excel,application/vnd.ms-powerpoint"
                      + ",application/vnd.oasis.opendocument.text,application/vnd.oasis.opendocument.spreadsheet"
                      + ",application/vnd.oasis.opendocument.chart,application/vnd.oasis.opendocument.presentation"
                      + ",audio/aac,audio/ogg,video/mp4"
                      + ",audio/mpeg,video/mpeg,video/mpeg,video/mpeg,video/avi,video/mov"
                      + ",application/x-shockwave-flash"
            .

        assign cIndex = lookup(pcExt, cExtList).

        if cIndex gt 0 then
            return entry(cIndex, cTypeList).
        else
            return "".
    end method. /* getMimeType */


    method public static logical isImageType ( input pcExt as character ):
        define variable cExtList as character no-undo.

        assign cExtList = "gif,jpg,jpeg,png,tif,tiff,bmp".

        if can-do(cExtList, pcExt) then
            return true.
        else
            return false.
    end method. /* isImageType */


    method public static logical isDocumentType ( input pcExt as character ):
        define variable cExtList as character no-undo.

        assign cExtList = "pdf,doc,docx,xls,xlsx,ppt,pptx,odt,ods,odc,odp".
        if can-do(cExtList, pcExt) then
            return true.
        else
            return false.
    end method. /* isDocumentType */


    method public static logical isTextType ( input pcExt as character ):
        define variable cExtList as character no-undo.

        assign cExtList = "htm,html,shtml,css,js,txt,csv,xml,json".
        if can-do(cExtList, pcExt) then
            return true.
        else
            return false.
    end method. /* isTextType */


    method public static logical isImportType ( input pcExt as character ):
        define variable cExtList as character no-undo.

        assign cExtList = "txt,csv,xml,json".
        if can-do(cExtList, pcExt) then
            return true.
        else
            return false.
    end method. /* isImportType */


    method public static logical isMediaType ( input pcExt as character ):
        define variable cExtList as character no-undo.

        assign cExtList = "aac,ogg,mp3,mpeg,mpg,mpe,mp4,avi,mov,swf".
        if can-do(cExtList, pcExt) then
            return true.
        else
            return false.
    end method. /* isMediaType */


    /************************ Overrides and Local Methods *************************/


    method public int64 getFiles ( input  pcDirectory as character,
                                  output TABLE for fileData ):
        empty temp-table fileData.                             
        return getFiles( input  pcDirectory,
                         input  false,
                         input  "",
                          output TABLE fileData by-reference).
    end method. /* getFiles */


    method public int64 getFiles ( input  pcDirectory as character,
                                  input  plRecursive as logical,
                                  input  pcOmitDirs  as character,
                                  output TABLE for fileData ):
        define variable cFileName  as character no-undo.
        define variable cFilePath  as character no-undo.
        define variable cFileType  as character no-undo.
        define variable cTempPath  as character no-undo.
        define variable dFileDate  as date      no-undo.
        define variable iFileTime  as integer   no-undo.
        define variable iCount as int64 no-undo. 
        DIRBLOCK:
        do on error undo DIRBLOCK, leave DIRBLOCK
           on stop  undo DIRBLOCK, leave DIRBLOCK:
            if not fileExists(pcDirectory) then leave DIRBLOCK.
            input FROM OS-DIR(pcDirectory).
            repeat:
                import cFileName cFilePath cFileType no-error.
                if error-status:error then leave DIRBLOCK.

                /* Grab the file's modified date. */
                file-info:file-name = cFilePath.
                assign
                    dFileDate = file-info:file-mod-date
                    iFileTime = file-info:file-mod-time
                    .

                /* Skip directories, hidden files, symlinks, etc. */
                if cFileName eq ".." or
                   cFileName eq "." or
                   cFileName eq " " or
                   cFileName eq "" or
                   cFileName begins "."
                   then next.

                if not (cFileType matches "*F*") then do {&THROW}:
                    /* If not a file, must be directory (or symlink) to follow recursively. */
                    /* Only proceeds if flag is set and top-level directory is not omitted. */
                    if plRecursive and not CAN-DO(pcOmitDirs, cFileName) then do {&THROW}:
                        assign cTempPath = substitute("&1&2&3",
                                                      pcDirectory,
                                                      (if opsys eq "WIN32" then "~\" else "~/"),
                                                      cFileName).
                       iCount = iCount + getFiles( input  cTempPath,
                                                   input  plRecursive,
                                                   input  "",
                                                   output TABLE fileData by-reference ).
                    end. /* plRecursive */
                end. /* D (Not F) */
                else do {&THROW}:
                    create fileData.
                    assign
                        fileData.fullPath = cFilePath
                        fileData.fullName = cFileName
                        fileData.fileExt  = getExt(cFileName)
                        fileData.fileType = getMimeType(getExt(cFileName))
                        fileData.fileTime = datetime-tz(dFileDate,ifiletime * 1000)
                        iCount = iCount + 1
                        . 
                    release fileData no-error.
                    
                end. /* F */
            end. /* REPEAT */
            return iCount.
            finally: 
                input CLOSE.
            end.
        end. /* DIRBLOCK */
    end method. /* getFiles */

end class.