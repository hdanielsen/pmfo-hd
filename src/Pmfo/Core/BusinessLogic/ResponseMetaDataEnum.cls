
/*------------------------------------------------------------------------
    File        : ResponseProcessingEnum
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Sat Jun 01 20:45:39 EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/

enum Pmfo.Core.BusinessLogic.ResponseMetaDataEnum flags :  

    define enum None = 0
                Id 
                Minimal = Id
                ReadLink
                EditLink 
                RowLinks = ReadLink,EditLink
                NavigationLinks 
                Links = RowLinks,NavigationLinks
                Full = Minimal,RowLinks,Links
                .

end enum.