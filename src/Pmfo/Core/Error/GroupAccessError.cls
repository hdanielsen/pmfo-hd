using Progress.Lang.*.
using Pmfo.Core.Error.UnauthorizedError.

block-level on error undo, throw.

class Pmfo.Core.Error.GroupAccessError inherits UnauthorizedError:
    constructor public GroupAccessError ():
        super ("User not member of group to access resource").
    end constructor.         
end class.