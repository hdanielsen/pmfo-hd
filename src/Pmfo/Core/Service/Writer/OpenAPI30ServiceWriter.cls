 
 /*------------------------------------------------------------------------
    File        : OpenAPI30ServiceWriter
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Sun Feb 27 10:41:04 EST 2022
    Notes       : 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using OpenEdge.Web.DataObject.Writer.OpenAPI30ServiceWriter.
using Progress.Json.ObjectModel.JsonArray from propath.
using Progress.Json.ObjectModel.JsonObject from propath.

block-level on error undo, throw.

class Pmfo.Core.Service.Writer.OpenAPI30ServiceWriter inherits OpenAPI30ServiceWriter: 
    define protected property description as character no-undo
        init "This is the iGenesis server.  You can find out more about Swagger at [http://swagger.io](http://swagger.io) or on [irc.freenode.net, #swagger](http://swagger.io/irc/)." 
        get.   
    
    define protected property termsOfservice as character no-undo
        init "http://swagger.io/terms/"  
        get.   
    
    
    
    method protected override JsonObject WriteServiceRoot():
        define variable svcJson as JsonObject no-undo.
        define variable jsonData as JsonObject no-undo.
        define variable childData as JsonObject no-undo.
        define variable arrayData as JsonArray no-undo.
        define variable varData as JsonObject no-undo.
        
        assign svcJson   = new JsonObject()
               jsonData  = new JsonObject()
               arrayData = new JsonArray()
               childData = new JsonObject()
               .
        
        svcJson:Add('openapi':u,    string(OPEN_API_VERSION)).      //REQUIRED. This string MUST be the semantic version number of the OpenAPI Specification version that the OpenAPI document uses. The openapi field SHOULD be used by tooling specifications and clients to interpret the OpenAPI document. This is not related to the API info.version string.
        svcJson:Add('info':u,       WriteInfo()).
        svcJson:Add('externalDocs':u, WriteExternalDocs()).                 //Additional external documentation.                              
        svcJson:Add('servers':u,    WriteDefaultServers()).         //An array of Server Objects, which provide connectivity information to a target server. If the servers property is not provided, or is an empty array, the default value would be a Server Object with a url value of /.
        svcJson:Add('paths':u,      WritePath()).              //REQUIRED. The available paths and operations for the API.
        
        assign childData = new JsonObject().
        svcJson:Add('components':u,    WriteComponents()).              //An element to hold various schemas for the specification.
       //   childData:Add('schemas':u, WriteDefaultSchemas()). 
            childData:AddNull('securitySchemes':u).
        
        svcJson:Add('security':u,      new JsonArray()).   //A declaration of which security mechanisms can be used across the API. The list of values includes alternative security requirement objects that can be used. Only one of the security requirement objects need to be satisfied to authorize a request. Individual operations can override this definition.
        svcJson:Add('tags':u,          new JsonArray()).   //A list of tags used by the specification with additional metadata. The order of the tags can be used to reflect on their order by the parsing tools. Not all tags that are used by the Operation Object must be declared. The tags that are not declared MAY be organized randomly or based on the tools' logic. Each tag name in the list MUST be unique.
        svcJson:Add('cccsecurity':u,      new JsonArray()).   //A declaration of which security mechanisms can be used across the API. The list of values includes alternative security requirement objects that can be used. Only one of the security requirement objects need to be satisfied to authorize a request. Individual operations can override this definition.
        
        return svcJson.
    end method.
    
    method protected override JsonObject WriteInfo():
        define variable infoData as JsonObject no-undo.
        
        assign infoData = new JsonObject().
        //string  Required. The title of the application.
        infoData:Add('title':u, 'IGensis API':u).
        //string  A short description of the application. GFM syntax can be used for rich text representation.
        infoData:Add('description':u, description).
        
        //string  The Terms of Service for the API.
        infoData:Add('termsOfService':u, termsOfservice).
        
        //Object  The contact information for the exposed API.
        infoData:Add('contact':u, WriteContact()).
        
        //Object  The license information for the exposed API.
        infoData:Add('license':u, GetLicense()).
       
        // The version of the OpenAPI document (which is distinct from the OpenAPI Specification
        // version or the API implementation version).
        infoData:Add('version':u, '1.0.0':u).   
        
        return infoData.
    end method.
    
    method protected JsonObject WriteContact():
        define variable oJson as JsonObject no-undo.
        oJson = new JsonObject().
        oJson:Add('name':u, 'Precision Strip':u).
        oJson:Add('url':u, 'https://www.precision-strip.com':u).
        oJson:Add('email':u, 'IT_Help@precision-strip.com':u).
        return oJson.
    end method.    
    
    method protected JsonObject WriteComponents():
        define variable oJson as JsonObject no-undo.
        oJson = new JsonObject().
        oJson:add('schemas',WriteSchemas()).
        oJson:Add('securitySchemes':u, WriteSecuritySchemes()).
        return oJson.
    end method.    
    
    method protected JsonObject WriteSchemas():
        return new JsonObject(). 
    end method.    
    
    method protected JsonObject WriteSecuritySchemes():
        define variable oJson as JsonObject no-undo.
        oJson = new JsonObject().
        oJson:Add('APiKeyAuth':u, WriteApiKeyAuth()).
        return oJson.
    end method.    
    
    method protected JsonObject WriteApiKeyAuth():
        define variable oJson as JsonObject no-undo.
        oJson = new JsonObject().
        oJson:Add('type':u, 'apiKey':u).
        oJson:Add('name':u, 'X-PSI-Authorization':u).
        oJson:Add('in':u, 'header':u).
        return oJson.
    end method.    
    
    method protected JsonObject WriteExternalDocs():
        define variable oJson as JsonObject no-undo.
        oJson = new JsonObject().
        oJson:Add('description':u, 'Find out more about Swagger':u).
        oJson:Add('url':u, 'http://swagger.io':u).
        return oJson.
    end method.    
    
    method protected JsonObject GetLicense():
        define variable oJson as JsonObject no-undo.
        oJson = new JsonObject().
        oJson:Add('name':u, 'Apache 2.0':u).
        oJson:Add('url':u, 'http://www.apache.org/licenses/LICENSE-2.0.html':u).
        return oJson.
    end method.    
   
    method protected JsonObject WritePath():
        return new JsonObject().
    end method.     
   
    method protected JsonObject GetServer(pcName as char) :
    
    end method.
    
    method protected JsonObject GetWebApps(pcServerName as char) :
        define variable oJsonArray as JsonArray no-undo.
        oJsonArray = new JsonArray().
        
    end method.
    
    
    method protected override JsonArray WriteDefaultServers():
        define variable servers as JsonArray no-undo.
        define variable arrayData as JsonArray no-undo.
        define variable defaultServer as JsonObject no-undo.
        define variable varData as JsonObject no-undo.
        define variable childData as JsonObject no-undo.
        
        assign servers       = new JsonArray()
               defaultServer = new JsonObject()
               childData     = new JsonObject()
               .
        // abs path
        servers:Add(defaultServer).
        
        defaultServer:Add('url':u, '~{scheme}://~{host}:~{port}~{ctx}/web~{basePath}':u).
        defaultServer:Add('description':u, 'Default server configuration':u).
        defaultServer:Add('variables':u, childData).
        
        assign varData   = new JsonObject()
               arrayData = new JsonArray()
               .
        childData:Add('scheme':u, varData).
            varData:Add('enum':u, arrayData).
                arrayData:Add('http':u).
                arrayData:Add('https':u).
            varData:Add('default':u, 'http':u).
        
        assign varData   = new JsonObject()
               arrayData = new JsonArray()
               .
        childData:Add('host':u, varData).
            varData:Add('default':u, 'localhost':u).
        
        assign varData   = new JsonObject()
               arrayData = new JsonArray()
               .
        childData:Add('port':u, varData).
            varData:Add('default':u, '8810':u).
        
        assign varData   = new JsonObject()
               arrayData = new JsonArray()
               .
        childData:Add('ctx':u, varData).
            varData:Add('description':u, 'The webapp/context for this service. Empty/blank indicates ROOT':u).
            varData:Add('default':u, '':u).
        
        assign varData = new JsonObject().
        childData:Add('basePath':u, varData).
            varData:Add('default':u, '/pdo':u).
        
        // relative path. will work relative to the OpenApi doc location 
        assign defaultServer = new JsonObject()
               childData     = new JsonObject()
               .
        servers:Add(defaultServer).
        
        defaultServer:Add('url':u, '~{ctx}/web~{basePath}':u).
        defaultServer:Add('description':u, 'Default relative-path server configuration':u).
        defaultServer:Add('variables':u, childData).
        
        assign varData   = new JsonObject()
               arrayData = new JsonArray()
               .
        childData:Add('ctx':u, varData).
            varData:Add('description':u, 'The webapp/context for this service. Empty/blank indicates ROOT':u).
            varData:Add('default':u, '':u).
        
        assign varData = new JsonObject().
        childData:Add('basePath':u, varData).
            varData:Add('default':u, '/pdo':u).
        
        return servers.
    end method.
end class.