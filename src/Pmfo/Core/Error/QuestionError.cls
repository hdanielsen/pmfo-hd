 
 /*------------------------------------------------------------------------
    File        : QuestionError
    Purpose     : Extend ValidatioinError for questions to allow questions 
                  from other datasets ttan the update BE and no dataset to be processed  
    Syntax      : 
    Description : 
    Author(s)   : Havardd
    Created     : Wed Oct 16 10:31:14 EDT 2024
    Notes       : 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using Pmfo.Core.Error.ValidationError.

block-level on error undo, throw.


class Pmfo.Core.Error.QuestionError inherits ValidationError: 

    define public property EntityName as character no-undo get. protected set.
    define public property QuestionId as integer   no-undo get. protected set.
    define public property Question   as character no-undo get. protected set.
    
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    constructor public QuestionError (pcEntityName as character, pcQuestionId as integer, pcQuestion as character ):
        super ().
        EntityName = pcEntityName.
        QuestionId = pcQuestionId.
        this-object:Question   = pcQuestion.
    end constructor.
    
 
end class.