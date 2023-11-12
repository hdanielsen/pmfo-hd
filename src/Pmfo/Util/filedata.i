
/*------------------------------------------------------------------------
    File        : filedata.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : hdaniels
    Created     : Fri Feb 15 12:06:28 EST 2019
    Notes       :
  ----------------------------------------------------------------------*/
 define temp-table fileData  no-undo {1} before-table beforeFileData  
    field fullPath    as character   label "File Path"
    field fullName    as character   label "File Name"
    field fileExt     as character   label "Extension"
    field fileType    as character   label "File Type"
    field fileTime    as datetime-tz label "Modified Time"
    index idxFileDate is primary fullPath fullName fileTime
 .
