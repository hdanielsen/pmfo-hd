 
 /*------------------------------------------------------------------------
    File        : Theme
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Fri Mar 29 12:32:43 EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/
 
using Pmfo.Tools.Util.ColorManager from propath.
using Pmfo.Tools.Util.ITheme from propath.

block-level on error undo, throw.

class Pmfo.Tools.Util.DefaultTheme implements ITheme: 
    
    define public property DisabledRowBg as integer no-undo 
        get():
            return ColorManager:WhiteSmoke. 
        end.
       
    // black is 0
    define public property DisabledRowFg as integer no-undo 
        get():
            return ColorManager:Gray. 
        end.
       
    define public property InfoBg as integer no-undo 
        get():
            return ColorManager:LightGrey. 
        end.
       
    // black is 0
    define public property InfoFg as integer no-undo get.
    
    define public property ErrorBg       as integer no-undo
        get():
            return ColorManager:LavenderBlush.
        end.
    define public property ErrorFg       as integer no-undo 
        get():
            return ColorManager:DarkRed.
        end.
    
    define public property WarningBg       as integer no-undo
        get():
            return ColorManager:LightYellow.
        end.
    define public property WarningFg       as integer no-undo 
        get():
            return ColorManager:Black.
        end.
    
    define public property PublicFg      as integer no-undo 
        get():
            return ColorManager:Black.
        end.
    
    define public property PublicBg      as integer no-undo
        get():
            return ColorManager:LightGreen.
        end.
     
    define public property PublicDisabledFg      as integer no-undo 
        get():
            return ColorManager:Gray.
        end.
    
    define public property PublicDisabledBg      as integer no-undo
        get():
            return ColorManager:LightGreen.
        end.
                    
        
    define public property ProtectedFg   as integer no-undo get.
    define public property ProtectedBg   as integer no-undo 
        get():
            return ColorManager:Khaki.
        end.
    
    define public property ProtectedDisabledFg   as integer no-undo 
        get():
            return ColorManager:Gray.
        end.
    
    define public property ProtectedDisabledBg   as integer no-undo 
        get():
            return ColorManager:Khaki.
        end.
    
        
    define public property ExistsFg   as integer no-undo get.
    define public property ExistsBg   as integer no-undo 
        get():
            return ColorManager:PowderBlue.
        end.
    
    define public property ExistsDisabledFg   as integer no-undo 
        get():
            return ColorManager:Gray.
        end.
    
    define public property ExistsDisabledBg   as integer no-undo 
        get():
            return ColorManager:PowderBlue.
        end.
    
        
             
end class.