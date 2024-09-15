 
 /*------------------------------------------------------------------------
    File        : FileIOSource
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Fri Mar 22 10:58:08 EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/

 
block-level on error undo, throw.

using Ccs.Common.Application from propath.
using Pmfo.Core.DataLayer.DataSource from propath.
using Pmfo.Core.Error.ApplicationError from propath.
using Pmfo.Core.Error.IllegalOperationError from propath.
using Pmfo.Core.Error.NotImplementedError from propath.
using Pmfo.Core.Manager.IServiceManager from propath.
using Pmfo.Util.FileIO from propath.
using Progress.Lang.Error from propath.
using Progress.Lang.Object from propath.
using Progress.Lang.ParameterList from propath.
using Progress.Reflect.Constructor from propath.
using OpenEdge.Core.StringConstant from propath.

class Pmfo.Repository.DataSource.ResourceSource abstract inherits DataSource  : 
    {Pmfo/Util/filedata.i}
    
    define protected property ServiceManager as IServiceManager no-undo
        get():
            return cast(Application:ServiceManager,IServiceManager).
        end.   
    
    define protected property FileIo as FileIO no-undo 
        get():
            if not valid-object(FileIO) then
                FileIO =  new FileIO().
            return FileIO.    
        end.
        set.
    
    constructor public ResourceSource (  ):
        super ().
    end constructor.
 
    method protected abstract void FillFromObject(poObject as Object).
    
    method protected abstract char GetFileDirectory().
    
    method public abstract void Cleanup(input table-handle phTable).
   
    method override protected void PrepareBuffer( input pBuffer as handle ):
        pBuffer:set-callback("Before-fill","BeforeFill").
        super:PrepareBuffer(input pBuffer).
    end method.
        
    method public void BeforeFill(input dataset-handle hDataset):
        define variable cClass       as character no-undo.
        define variable oObject      as Object no-undo.
        define variable oType        as Progress.Lang.Class no-undo.
        define variable oParamList   as ParameterList no-undo.
        define variable oConstructor as Constructor no-undo.
        define variable lStopped     as logical no-undo.
        file-Info:file-name = GetFileDirectory().
        if file-info:full-pathname <> ? then
        do:
            fileio:getFiles(file-info:full-pathname, output table fileData).
           
            for each filedata where filedata.fileExt = "cls":
                cClass = filedata.fullName.
                entry(num-entries(cClass,"."),cClass,".") = "".
                cClass = right-trim(cClass,".").      
                cClass = substitute("&1.&2",GetFileDirectory(),cClass).
                oType = Progress.Lang.Class:GetClass(cClass).
                if not oType:IsAbstract() then 
                do: 
                    oParamList = new ParameterList(0).
                    oConstructor = oType:GetConstructor(oParamList).
                    if valid-object(oConstructor) then 
                    do on error undo, leave: 
                        oObject = dynamic-new cClass( ).
                        if valid-object(oObject) then 
                        do:
                            FillFromObject(oObject).
                        end.
                        catch e as Progress.Lang.Error :
                            undo, throw new ApplicationError(subst("Start of &1 failed with error - &2",cClass,e:GetMessage(1)),e).
                        end catch. 
                    end.
                end.       
            end.
        end.     
        else 
            undo, throw new IllegalOperationError(subst("Source code directory '&1' not in propath",GetFileDirectory())). 
            
        catch e as Progress.Lang.Error :
            message "throwing fill error" e:GetMessage(1) skip 
             e:CallStack
            view-as alert-box.
            ThrowFillError(e).    
        end catch.                  
    end method.
   
    method protected character GetCodeCreateErrorMessage(pShortType as char,pcExplain as char,pcClassName as char,pError as Error):
        return subst("Could not add &1 &2 to catalog. " + 
                     "The class &3 will not be reachable as a Service from the ServiceManager &4", 
                     pShortType,
                     pcExplain,
                     pcClassName,
                     if valid-object(pError) then  StringConstant:LF + "- Error: " + pError:GetMessage(1) else "").
    
    end.     
    
    method protected character ReservedResourceMessage(pcShortType as char,pcClass as char, pcPropName as char,pcPropValue as char):
        return subst("&1 &2 with &3 '&4' was not added. The name is reserved and the class is managed directly by the ServiceManager",pcShortType,pcClass,pcPropName,pcPropValue).
    end method. 
   
/*    method public logical Prepare( input phBuffer as handle, input pRequest as Ccs.BusinessLogic.IGetDataTableRequest, input pParentRequest as Ccs.BusinessLogic.IGetDataTableRequest ):*/
/*        DataBuffer = phBuffer.                                                                                                                                                          */
/*        PrepareBuffer(phbuffer).                                                                                                                                                        */
/*    end method.                                                                                                                                                                         */
/*                                                                                                                                                                                        */
/*    method public logical Prepare( input phBuffer as handle, input pRequest as Ccs.BusinessLogic.IGetDataTableRequest ):                                                                */
/*        return Prepare(phBuffer,pRequest,?).                                                                                                                                            */
/*    end method.                                                                                                                                                                         */

    method public override logical Prepare(  ):
        undo, throw new NotImplementedError("Prepare").
    end method.

    
    method public override logical ResetQuery(  ):
        undo, throw new NotImplementedError("ResetQuery").
    end method.

/*    method public override void TransactionValidateChildBuffers( input phBuffer as handle, input poRequest as Ccs.BusinessLogic.IUpdateDataRequest, input pDataSources as OpenEdge.Core.Collections.IMap, input transeventType as Pmfo.Core.DataLayer.TransactionHookEventType ):*/
/*    end method.                                                                                                                                                                                                                                                                 */

end class.