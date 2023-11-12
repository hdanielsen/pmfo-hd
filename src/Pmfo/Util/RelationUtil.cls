 
 /*------------------------------------------------------------------------
    File        : RelationUtil
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Tue Sep 08 20:26:53 EDT 2020
    Notes       : 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using Pmfo.Core.BusinessLogic.IRelation from propath.
using OpenEdge.Core.Collections.ICollection from propath.
using OpenEdge.Core.Collections.IIterator from propath.

block-level on error undo, throw.

class Pmfo.Util.RelationUtil: 
    
    method public static IRelation FindRelationByPublicName(pCollection as ICollection ,pcParent as character, pcPublicName as character ):
        return FindRelation(pCollection,pcParent,pcPublicName,true).
    end method.     

    method public static IRelation FindRelationByName(pCollection as ICollection ,pcParent as character, pcName as character ):
        return FindRelation(pCollection,pcParent,pcName,false).
    end method.     
    
    method private static IRelation FindRelation(pCollection as ICollection ,pcParent as character, pcName as character, plPublic as logical ):
        define variable oIterator as IIterator no-undo.  
        define variable oRelation       as IRelation no-undo.
        if valid-object(pCollection) then
        do:
            oIterator = pCollection:Iterator().
            do while oIterator:HasNext():
                oRelation = cast(oIterator:Next(),IRelation).
                if oRelation:ParentName  = pcParent
                and (if plPublic then oRelation:PublicName = pcName else  oRelation:Name = pcName) then
                    return oRelation.
            end.       
        end.
        return ?.
    end method. 
end class.