 
 /*------------------------------------------------------------------------
    File        : PasswordRules
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Sun Oct 27 09:46:12 EDT 2024
    Notes       : 
  ----------------------------------------------------------------------*/

using Ccs.Common.Application from propath.
using OpenEdge.Core.StringConstant from propath.
using Psi.Core.Manager.ISessionManager from propath.
using Pmfo.Util.StringUtil from propath.
 
block-level on error undo, throw.

interface Pmfo.Core.Common.IPasswordRules: 
        
    define public property MinLength               as integer no-undo get.
    define public property MaxLength               as integer no-undo get. 
    define public property MinALphabeticCharacters as integer no-undo get. 
    define public property MinNumericCharacters    as integer no-undo get. 
    define public property MinSpecialCharacters    as integer no-undo get. 
    define public property MinUpperCaseCharacters  as integer no-undo get.
    define public property MinLowerCaseCharacters  as integer no-undo get. 
    define public property EnforcePasswordHistory  as integer no-undo get. 
    define public property MaxAgeInDays            as integer no-undo get. 
    define public property MinNumberOfRules        as integer no-undo get. 
    
    // properties that returns text and value
    define public property MinLengthText                as character no-undo get. 
    define public property MaxLengthText                as character no-undo get. 
    define public property MinALphabeticCharactersText  as character no-undo get. 
    define public property MinNumericCharactersText     as character no-undo get. 
    define public property MinSpecialCharactersText     as character no-undo get.
    define public property MinUpperCaseCharactersText   as character no-undo get.
    define public property MinLowerCaseCharactersText   as character no-undo get.
    define public property EnforcePasswordHistoryText   as character no-undo get. 
    define public property MinNumberOfRulesText         as character no-undo get. 
    
    // Summary of all active rules - (values gt 0 ) 
    method public character GetPasswordRulesAsText(). 
   
    // Summary of all active rules with bullit - really dash..  
    method public character GetPasswordRulesAsTextBulleted().
    
    // Summary of all active rules with numbers   
    method public character GetPasswordRulesAsTextNumbered(): 
        
    
end interface.