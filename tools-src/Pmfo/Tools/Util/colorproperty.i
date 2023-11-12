
/*------------------------------------------------------------------------
    File        : colorproperty.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : hdaniels
    Created     : Wed Mar 27 09:07:52 EDT 2019
    Notes       :
  ----------------------------------------------------------------------*/
 define static public property {1}  as integer no-undo init ? 
       get():
           if {1} = ? then 
           do:
               {1} = AddColor({2},{3}, {4}).
           end.    
           return {1}.
       end. 
       set.