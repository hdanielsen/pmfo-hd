
/*------------------------------------------------------------------------
    File        : ITheme
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Fri Mar 29 12:33:48 EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.

interface Pmfo.Tools.Util.ITheme:  
    define public property DisabledRowBg as integer no-undo get.
    define public property DisabledRowFg as integer no-undo get.
    define public property InfoBg as integer no-undo get.
    define public property InfoFg as integer no-undo get.
    define public property ErrorBg       as integer no-undo get.
    define public property ErrorFg       as integer no-undo get.
    define public property WarningBg     as integer no-undo get.
    define public property WarningFg     as integer no-undo get.
    define public property PublicBg      as integer no-undo get.
    define public property PublicFg      as integer no-undo get.
    define public property ProtectedBg   as integer no-undo get.
    define public property ProtectedFg   as integer no-undo get.
    define public property ExistsBg      as integer no-undo get.
    define public property ExistsFg      as integer no-undo get.
    define public property PublicDisabledBg      as integer no-undo get.
    define public property PublicDisabledFg      as integer no-undo get.
    define public property ProtectedDisabledBg   as integer no-undo get.
    define public property ProtectedDisabledFg   as integer no-undo get.
    define public property ExistsDisabledBg      as integer no-undo get.
    define public property ExistsDisabledFg      as integer no-undo get.
end interface.