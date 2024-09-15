
/*------------------------------------------------------------------------
    File        : OpenAPISchemaNameEnum
    Purpose     : Names to sue for the various schema variaitons added to the 
                  OpenAPI components/schemas
                  This allows reuse when writing requestBody for the BEs 
    Syntax      : Not used directly, but as paramter to method that also has parameter for serialize-name (possibly buffer handle) 
    Description : 
    Author(s)   : hdaniels
    Created     : Thu Jun 13 06:20:50 EDT 2024
    Notes       : It is possible that it is better to have these in a level below 
                  the schemas,  but it is a bit more complicated    
    --------------------------------------------------------------------*/

enum Pmfo.Core.Service.OpenAPISchemaNameEnum :  

    define enum Properties // plain list of properties
                PropertiesInArray // properties inside an array  
                ProdsProperties  // list of properties with Progress prods:rowState and rowid  
                ObjectBody  // wraps properties in serialize-name
                ArrayBody   // wraps propertiesInarray in serialize-name
                // not in use  
                // ObjectModifiedBody   // wraps modifiedProperties in serialize-name both at top and in before image section
                ArrayModifiedBody   // wraps modifiedProperties in serialize-name both at top and in before image section
                .

end enum.