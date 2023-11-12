/**********************************************************************
THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE. 
***********************************************************************/
/*------------------------------------------------------------------------
    File        : DatasetProcessor
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Havard Danielsen, Progress Software Corporation
    Created     : 2020
    Notes       :  
----------------------------------------------------------------------*/
block-level on error undo, throw.

class Pmfo.Core.Common.DatasetParser : 
     
    define public event BeforeFirstRow signature void (input phBuffer as handle). 
    define public event AfterLastRow signature void (input phBuffer as handle).
    define public event BeforeRow signature void (input phBuffer as handle). 
    define public event AfterRow signature void (input phBuffer as handle).  
    define public event EmptyRow signature void (input phBuffer as handle). 
    
    constructor public DatasetParser ():
        super ().
    end constructor.
     
    method public void ParseDataset(phDataset as handle):
        define variable iLoop as integer no-undo.
        define variable hBuffer as handle no-undo.
        define variable hRelation as handle no-undo.
      
        do iLoop = 1 to phDataset:num-top-buffers:
            hBuffer = phDataset:get-top-buffer(iLoop).
            ProcessBuffer(hBuffer).
        end.
        do iLoop = 1 to phDataset:num-relations:
            hRelation = phDataset:get-relation (iLoop).
            if hRelation:nested = false then
                ProcessBuffer(hRelation:child-buffer).
        end.
      
        
    end method.     
    
    method protected void ProcessBuffer(phBuffer as handle):
        define variable hQuery  as handle no-undo.
        define variable lCreated as logical no-undo.
        define variable lData as logical no-undo.
        
        if valid-handle(phBuffer:parent-relation) then 
        do:
            if phBuffer:parent-relation:nested then 
               hQuery = phBuffer:parent-relation:query.
            else do:
                create query hquery.
                hQuery:add-buffer(phBuffer).
                hQuery:query-prepare(subst("for each &1",phbuffer:name)).
                lCreated = true. 
            end.
        end.
        else     
            hQuery = phBuffer:dataset:top-nav-query(phbuffer:name).
    
        hQuery:query-open().
        hQuery:get-first.
        if phBuffer:avail then
            BeforeFirstRow:Publish(phBuffer).
        do while phBuffer:avail:
            BeforeRow:publish (phBuffer).
            ProcessChildren(phBuffer).
            AfterRow:publish (phBuffer).
            lData = true.
            hQuery:get-next. 
        end. 
        if lData then
            AfterLastRow:Publish(phBuffer).
        else
            EmptyRow:publish(phBuffer).
        
        finally:
            if lCreated and valid-handle(hQuery) then
                delete object hQuery.
        end.         
    end method.
    
    method protected void ProcessChildren(phBuffer as handle):
        define variable hRelation as handle no-undo.
        define variable i as integer no-undo.
        do i = 1 to phBuffer:num-child-relations:
            hRelation = phbuffer:get-child-relation(i).
            if hRelation:nested then
                ProcessBuffer(hRelation:child-buffer).
        end.     
    end method.
      
end class.