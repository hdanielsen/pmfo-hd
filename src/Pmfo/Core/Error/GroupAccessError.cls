 
using Pmfo.Core.Error.ForbiddenRequestError.

block-level on error undo, throw.

 
class Pmfo.Core.Error.GroupAccessError inherits ForbiddenRequestError:
    constructor public GroupAccessError ():
        super ("User not member of group to access resource").
    end constructor.         
end class.