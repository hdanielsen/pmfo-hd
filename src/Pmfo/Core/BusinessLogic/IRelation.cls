
/*------------------------------------------------------------------------
    File        : IRelation
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Wed Apr 10 10:58:27 EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/

using Pmfo.Core.BusinessLogic.IJoinPredicate from propath.
using Pmfo.Core.BusinessLogic.IRelationDefinition from propath.
using Pmfo.Core.BusinessLogic.RelationTypeEnum from propath.
using Progress.Json.ObjectModel.JsonObject from propath.
//using Pmfo.Core.BusinessLogic.RelationValidationEnum from propath.

interface Pmfo.Core.BusinessLogic.IRelation inherits IRelationDefinition:
    /* the resource name on the other end */
    define public property Name           as character no-undo get.   
    /* allow an alias - important if more than one relation to the same entity (not for onetomany) */
    define public property AliasName      as character no-undo get.  
    
    define public property Public         as logical   no-undo get. set.
    
    // set to true if link should be populated with blank char or 0 number in single value property    
    define public property LinkToEmpty    as logical no-undo get. set. 
    
    define public property LookupFunction as character no-undo get. set. 

    // set at creation to tell link routers to call CreateDynamicBusinessEntity
    // with parent buffer and this relation as context when starting the BE  
    // The BE must implement IDynamicBusinessEntity
    define public property IsDynamic      as logical no-undo get. set. 

    // use foreign key for many-to-one to entity with single primary key
    // when there is only one key then we can use just the foreign value in the URI
    // and there is no need to know the schema of the other object 
    // for multiple fields we need the other object's field name so we manage it 
    // by storing it as predicate and requesting PairsList, which will invoke 
    // code necessary to get the schems of the other object 
    define public property ForeignKey    as character      no-undo get.
    
    // set to true when the entity is to be managed as a hidden xref table
    // This requires that the (Alias) PuplicName is set to the serialize-name of the entity in the other end
    // and that the xref entity that the name is refering to has a relation or link to the other 
    // end of the Xref with the same PublicName or serialize-name as the (Alias) PuplicName of this.
    // This JsonGetResponseProcessor will then hide the table on read inclduing after an update
    // The JsonReader has logic to load data to a  child buffer with the same name as the derialize-name
    // when there is no data for it in the json 
       
    define public property XRef          as logical        no-undo get. set.
    
    // for all relations that cannot be defined with ForeignKey 
    define public property JoinPredicates as IJoinPredicate no-undo  extent get.
    
    // How to validate relation for ManyToOne and OneToOne relations  
  //  define public property ValidationRule as RelationValidationEnum get. set.
    
    // returns the join as parentfield,childfield[,parentfield,childfield]
    // this operation require schema information about the other object 
    method public character PairsList(). 
    
    // extract parent values from json and return expression with child values 
    // and child field names
    method public character GetPublicLink(pJson as JsonObject,phSchema as handle).
    // generate a key template with @p number for values 
    method public character GetPublicKeyTemplate(phSchema as handle). 
    
    // The Can and Readonly properties can be set to control what is allowed when requesting parentName/publicname 
    // when the parent table is not in the JSON body or in the JSON response
    define public property CanDelete      as logical        no-undo get. set. 
    define public property CanCreate      as logical        no-undo get. set.
    define public property CanUpdate      as logical        no-undo get. set.
    define public property ReadOnly       as logical        no-undo get. set.     
    
        
end interface.