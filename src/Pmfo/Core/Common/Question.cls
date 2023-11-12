 
 /*------------------------------------------------------------------------
    File        : Question
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Mon Aug 19 11:59:05 EDT 2019
    Notes       : Registered in a BE or DS with unique ids
                  The BE or DS must have APIs that adds a question that 
                  uses the Id and requires the Id to be registered 
                - The Id is returned in the error/question JSON and
                  intended to be passed as query parameters when confirmed
                - The IUpdateDataRequest passes the list of confirmed ids          
----------------------------------------------------------------------*/

using Progress.Lang.*.
using Pmfo.Core.Common.IQuestion from propath.
using OpenEdge.Core.Assert from propath.

block-level on error undo, throw.

class Pmfo.Core.Common.Question implements IQuestion: 
    define public    property Id      as integer   no-undo get. protected set.
    define public    property Message as character no-undo get. protected set.
    /*--------------------------------------------------------------------------
     Purpose:
     Notes:
    --------------------------------------------------------------------------*/
    constructor public Question (pId as integer ):
        super ().
        Assert:NotNull(pId,"Id").
        Assert:NotZero(pId,"Id").
        Id = pid.
    end constructor.
    
    constructor public Question (pId as integer, pMessage as character ):
        this-object (pId).
        Assert:NotEmpty(pMessage,"Title").
        Assert:NotNull(pMessage,"Title").
        this-object:Message = pMessage. 
    end constructor.
     
    // equals if Id is same to allow duplicate check in collection   */
    method override public logical Equals( input objectRef as Progress.Lang.Object ):
        define variable lEquals as logical no-undo.
        lEquals = super:Equals(input objectRef).
        if not lEquals then
        do:
            if type-of(objectRef,IQuestion) then
                return cast(objectRef,IQuestion):Id = Id.
        end.
        return false.

    end method.
     
end class.