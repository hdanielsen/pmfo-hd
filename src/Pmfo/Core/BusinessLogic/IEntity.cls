
/*------------------------------------------------------------------------
    File        : IEntity
    Purpose     : Basic access properties for entities
    Syntax      : 
    Description : These defines the entity and are cached in the Entity resource
                  data that the ServiceManager uses and all, except ClientOnly
                  which is internal, are used to generate the Catalog list that
                  only shows the name and ther access (allowed methods)
               -  Change of public will require a refresh of resources and should
                  only be done from the construtor.
                  This refresh shappens automatically when the class is changed
                  if ServerConfig CheckForNewResources is true.
               -  The OperationHandler uses the actual entity to check the 
                  ReadOnly and Can* properties after the object has been
                  initialized, so those can possibly be changed in initialize
                  (in the future). 
    Author(s)   : hdaniels
    Created     : Mon May 27 08:25:42 EDT 2019
    Last Comment: Aug 24 2020 - The ClientOnly behavior is under constant
                  improvement and refinement (See also ClientBusinessEntity)   
    Notes       : The implementation will typically ensure that most of these 
                  only can be set from constructor.
                  These restriction can be lifted, but with utter care
                  The public property should always be constructor set only
                  or read-only 
  ----------------------------------------------------------------------*/

using Ccs.Common.IService from propath.
using Pmfo.Core.Common.IResource from propath.
using Pmfo.Core.Common.ServerTypeEnum from propath.

interface Pmfo.Core.BusinessLogic.IEntity inherits IService, IResource:  
    define public property Public         as logical        no-undo get.  
    define public property ReadOnly       as logical        no-undo get.  
    define public property CanDelete      as logical        no-undo get.
    define public property CanCreate      as logical        no-undo get.
    define public property CanUpdate      as logical        no-undo get.
    define public property CanRead        as logical        no-undo get.  
    define public property ClientOnly     as logical        no-undo get.
    define public property ServerType     as ServerTypeEnum no-undo get. 
     
end interface.