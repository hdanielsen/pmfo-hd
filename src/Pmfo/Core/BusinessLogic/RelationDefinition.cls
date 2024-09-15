
/*------------------------------------------------------------------------
    File        : RelationDefinition
    Purpose     : Light weight class that is created from IRelation to be 
                  used in query request building  
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Wed Apr 10 10:58:27 EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/

using OpenEdge.Core.Assert from propath.
using Pmfo.Core.BusinessLogic.IRelation from propath.
using Pmfo.Core.BusinessLogic.IRelationDefinition from propath.
using Pmfo.Core.BusinessLogic.RelationDefinition from propath.
using Pmfo.Core.BusinessLogic.RelationTypeEnum from propath.
using Progress.Lang.Object from propath.

class Pmfo.Core.BusinessLogic.RelationDefinition implements IRelationDefinition :
    /* the resource name of the entity that defines the relation */ 
    define public property ParentName     as character no-undo get. protected set.  
    /* the resource nameo on the other end */
    define public property PublicName     as character no-undo get. public set. // not public in interface - linkexpander uses this to correct  
    define public property Cardinality     as RelationTypeEnum no-undo get. set . 
    define public property XRef            as logical no-undo get. set.
         
    constructor public RelationDefinition (pcParent as character, pcPublicname as character, pCardinality as RelationTypeEnum ):
        super ().
        Assert:NotNullOrEmpty(pcParent,"Parent name").
        Assert:NotNullOrEmpty(pcPublicname,"Public name").
        Assert:NotNull(pCardinality,"Cardinality").
        ParentName = pcParent.
        PublicName = pcPublicName.
        Cardinality = pCardinality.
    end constructor.
    
    constructor public RelationDefinition (pRelation  as IRelation):
        super ().
        Assert:NotNull(pRelation,"Relation").
        ParentName = pRelation:ParentName.
        PublicName = pRelation:PublicName.
        Cardinality = pRelation:Cardinality.
    end constructor.
    
    constructor public RelationDefinition (pParent as character, pPublicName as character):
        ParentName = pParent.
        PublicName = pPublicName.
    end constructor.
     
    method public override logical Equals(pObject as Object):
        define variable oCompare as IRelationDefinition no-undo.
        if super:Equals(pObject) then
           return true.
        
        if type-of(pObject,IRelationDefinition) then
        do:
            oCompare = cast(pObject,IRelationDefinition).
            return oCompare:ParentName  = ParentName 
               and oCompare:PublicName  = PublicName
               and oCompare:Cardinality = Cardinality.
        end.       
        
        return false. 
           
    end method.
    
    method public override character ToString():
        define variable cJoin   as character no-undo.
        if valid-object(Cardinality) then
        do: 
            case Cardinality:
                when RelationTypeEnum:ManyToOne then
                    cJoin = ">-".
                when RelationTypeEnum:OneToMany then
                    cJoin = "-<".
                when RelationTypeEnum:OneToOne then
                    cJoin = "--".
               
            end.
        end.
        return subst("&1&2&3",ParentName,if cJoin = "" then " " else cJoin,PublicName). 
        
    end method.    
    
    method public override Object Clone():
        define variable oClone as IRelationDefinition no-undo.
        oClone = new RelationDefinition(ParentName,PublicName).
        if valid-object (Cardinality) then
           oClone:Cardinality = Cardinality.
        return oClone.    
    end method.    
    
end class.