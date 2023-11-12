/*------------------------------------------------------------------------------
  Purpose: Base query 
  Notes:   
------------------------------------------------------------------------------*/
routine-level on error undo, throw.   
 
using Pmfo.Core.DataLayer.IDbQuery from propath.
using Pmfo.Core.Error.UnsupportedOperationError from propath.
using Pmfo.Core.Error.IllegalOperationError from propath.
using Pmfo.Core.Error.NotImplementedError from propath.
using OpenEdge.Core.Assert from propath.
using Pmfo.Core.Error.IllegalStateError from propath.
 
class Pmfo.Core.DataLayer.DbQuery abstract implements IDbQuery :
   
 
    define public property BufferHandle      as handle no-undo get. set.
    
    define public    property BufferName        as char    no-undo 
        get():
            return BufferHandle:name.
        end.  
    
    define protected property NoConnectNeeded   as logical no-undo get. set.
    
    define public    property PhysicalTableName as char    no-undo
        get():
            return BufferHandle:table.
        end.  
    
    define public property BufferHandles     as handle  extent no-undo get. protected set.
    
    define public property BufferNames       as character no-undo 
        get():
            define variable cNames as character no-undo.
            define variable cDlm   as character no-undo.
            define variable i      as integer    no-undo.
            do i = 1 to extent(BufferHandles):
                assign 
                    cNames = cNames + cDlm + BufferHandles[i]:name
                    cDlm = ","
                    . 
            end.    
            return cNames.
        end.     
     
    define public property BaseQuery  as character no-undo 
        get():
            AssertBufferHandles("Get BaseQuery").
         
            if BaseQuery = "" or BaseQuery = ? then do:
                BaseQuery = GetBaseQuery().
                if BaseQuery = "" or BaseQuery = ? then
                    undo, throw new IllegalStateError(subst("&1.GetBaseQuery() returns unknown or blank",getClass():TypeName)). 
            end.    
         return BaseQuery.
    end. 
    set.
     
    define public    property KeyFields      as character no-undo get. set.    
   
    define protected property QueryHandle    as handle    no-undo  get. set.
    
    define public property QueryString  as character no-undo 
       get():
           if valid-handle(QueryHandle) then 
               return QueryHandle:prepare-string.
           return "".     
       end.
    define protected property QueryLock      as char no-undo 
        get. 
        set.
    
    /* the main table handle is mandatory (It can be anywhere in the query although it typically would be first) */  
    constructor public DbQuery(phBuffer as handle): 
        super(). 
        Assert:NotNull(phBuffer, "DBQuery buffer passed to constructor").
        BufferHandle = phBuffer.
    end constructor.
  
    constructor protected DbQuery(): 
        super(). 
        NoConnectNeeded = true.
    end constructor.
    
    method protected void CreateQuery():
        define variable iBuffer    as integer no-undo.
        define variable hBuffer    as handle  no-undo.
        define variable cPhysTable as character no-undo.
      
        AssertBufferHandles("CreateQuery()").
        DeleteQuery().
      
        create query QueryHandle.
        do iBuffer = 1 to extent(BufferHandles):
            QueryHandle:add-buffer(BufferHandles[iBuffer]).
        end.
    end method.
  
    method protected void DeleteQuery():
        /* delete the handles  */
        define variable iBuffer as integer    no-undo.
        define variable h       as handle     no-undo extent.
        define variable iNum    as integer    no-undo.
        if valid-handle(QueryHandle) then
        do:
            iNum = QueryHandle:num-buffers.
            if iNum > 0 then
            do:
                /* we loose num-buffers on first delete */
                extent(h) = iNum.
                do iBuffer = 1 to iNum:
                    h[iBuffer] = QueryHandle:get-buffer-handle(iBuffer).
                end.
          
                do iBuffer = 1 to iNum: 
                    delete object h[iBuffer]  no-error.
                end.
            end.
            delete object QueryHandle no-error.
        end.
      
    end method.
  
    method public logical Prepare ():
        BufferHandles = GetBufferHandles().
        return QueryHandle:query-prepare(currentQuery()).     
    end.
  
    method public logical ResetQuery ():
        return QueryHandle:query-prepare(defaultQuery()).     
    end.
    
    /* override to provide multiple buffers to the query */
    method protected handle extent GetBufferHandles():
        define variable hBuffer as handle extent 1 no-undo.
        hBuffer[1] = BufferHandle. 
        return hBuffer.
    end method.
  
    /* 
    Internal logic for BaseQuery property
    Default assumes one buffer. Override to create base query based on various buffers */
    method protected character GetBaseQuery():
        if extent(BufferHandles) = 1 then 
            return "for each " + BufferHandles[1]:name + " no-lock indexed-reposition".
        undo, throw new IllegalOperationError("Default GetBaseQuery called with more than 1 buffer").      
    end.    
  
    /*------------------------------------------------------------------------------
      Purpose: Return the current query for query manipulation and prepare.
               returns the default if not yet prepared.
     ------------------------------------------------------------------------------*/
    method protected character CurrentQuery ():
        define variable cQueryString as character  no-undo.
    
        if QueryHandle:prepare-string > '' then
            return QueryHandle:prepare-string.

        return DefaultQuery().

    end method. 
  
    method public character extent GetCurrentRowKey():
        undo, throw new NotImplementedError("GetCurrentRowKey").
    end method.    
  
    method public character DefaultQuery ():
  /*------------------------------------------------------------------------------
    Purpose:     Return the BaseQuery or build a default query. 
  ------------------------------------------------------------------------------*/
        define variable iBuffer     as integer    no-undo.
        define variable cPrepare    as character  no-undo.
        define variable cBuffer     as character  no-undo.
        define variable cParent     as character  no-undo.
        define variable cMode       as character  no-undo.
      
        if BaseQuery > '' then
            return BaseQuery.
        
        
        /* If there's more than one buffer than add them, just assuming that
           an OF relationship to the first table in tables will properly relate them. */
        cPrepare = "for each " + QueryHandle:get-buffer-handle(1):name 
                 + (if QueryLock > "" then " " + QueryLock else "").
        do iBuffer = 2 to QueryHandle:num-buffers:
    
            assign 
                cBuffer  = QueryHandle:get-buffer-handle(iBuffer):name
                cParent  = if BufferName = cBuffer then 
                             QueryHandle:get-buffer-handle(1):name
                           else  
                             QueryHandle:get-buffer-handle(BufferName):name
                cMode    = if BufferName = cBuffer then 'each' else 'first'
                cPrepare = cPrepare 
                         + ", " + cMode + " " + cBuffer + " of " +  cParent 
                         + (if QueryLock > "" then " " + QueryLock else "").
    
        end.   /* DO iBuffer = 2 */
        cPrepare = cPrepare + ' indexed-reposition'.  
        return cPrepare.
    end.

  method public character ColumnValue (pcColumn as char):
    define variable cBuffer as character  no-undo.
    define variable cColumn as character  no-undo.
    define variable cValue  as character  no-undo.
    define variable hBuffer as handle     no-undo.

    assign
      cBuffer = entry(1,pcColumn,'.')
      cColumn = entry(2,pcColumn,'.')  
      hBuffer = QueryHandle:get-buffer-handle(cBuffer).
    if hBuffer:AVAIL then
    do:
      /* extent support could be added by <field>[i] as param, 
        but this used for keys though */
      cValue = hBuffer:buffer-field(cColumn):BUFFER-VALUE(0). 
      /* this string is for transport of values, so return unknown as string
       (assuming '?' never is a value in a progress DB...)*/        
      return if cValue <> ? then cValue else '?'.
    end.
    return ?. /* not avail*/ 
  end method.

  /* order neutral position that is safe if tables changes order 
     passed back to setPosition  
     note that one need all rowids when the unique table is not 
     the first  */
  method public character extent GetPosition ():
     define variable iBuffer as integer    no-undo.
     define variable cBuffer as character  no-undo.
     define variable cPosition as character extent no-undo.
     
     extent(cPosition) = extent(BufferHandles).
     
     do iBuffer = 1 to extent(BufferHandles):
       assign
         cPosition[iBuffer] = string(BufferHandles[iBuffer]:rowid).
     end.
     
 
     return cPosition.
  end method.
 
  /* set position as returned from GetPosition */
  method public logical SetPosition (prPosition as rowid extent  ):
      define variable lOk as logical no-undo.
      define variable iNumExtents as integer no-undo.
      iNumExtents = extent(prPosition).
      case iNumExtents:
          /* workaround for 2 issues: extent 1 old - many extents new */
          when 1 then
              lOk = QueryHandle:reposition-to-rowid(prPosition[1]) no-error.
          when 2 then
               lOk = QueryHandle:reposition-to-rowid(prPosition[1],prPosition[2]) no-error.
          when 3 then
               lOk = QueryHandle:reposition-to-rowid(prPosition[1],prPosition[2],prPosition[3]) no-error.
          when 4 then
               lOk = QueryHandle:reposition-to-rowid(prPosition[1],prPosition[2],prPosition[3],prPosition[4]) no-error.
          when 5 then
               lOk = QueryHandle:reposition-to-rowid(prPosition[1],prPosition[2],prPosition[3],prPosition[4],prPosition[5]) no-error.
          when 6 then
               lOk = QueryHandle:reposition-to-rowid(prPosition[1],prPosition[2],prPosition[3],prPosition[4],prPosition[5],prPosition[6]) no-error.
          when 7 then
               lOk = QueryHandle:reposition-to-rowid(prPosition[1],prPosition[2],prPosition[3],prPosition[4],prPosition[5],prPosition[6],prPosition[7]) no-error.
          otherwise 
              undo, throw new UnsupportedOperationError("More than 7 buffers in SetPosition.").    
 
      end case.
      if lOk then 
          QueryHandle:get-next.
      return lOk.  
  end method. 
  
   /* We may clone this query in order to get row keys from a result without
       causing the 'real' query to reposition. This may be used when performing
       multi-select operations in the UI, where we don't want to move off the
       current record. Note that these actions may be expensive, because of the
       cost of creating, opening, etc the query. */
   method protected handle CloneQuery(input phSource as handle):
       define variable hQueryClone as handle no-undo.        
       define variable iLoop as integer no-undo.
       define variable iMax as integer no-undo.
     
       create query hQueryClone.
       iMax = phSource:num-buffers.
    
       do iLoop  = 1 to iMax:
           hQueryClone:add-buffer(phSource:get-buffer-handle(iLoop)).
       end.
       hQueryClone:query-prepare(phSource:prepare-string).

       return hQueryClone.
   end method. 
  
   method protected void RemoveQueryBuffers(phQuery as handle):
       define variable iLoop as integer no-undo.
       define variable iMax as integer no-undo.
        
        iMax = phQuery:num-buffers. 
        
        /* Clean up after ourselves. Note that this only removes the
           named buffer used for the query, and not the underlying buffer
           itself. */
        do iLoop = 1 to iMax:
            delete object phQuery:get-buffer-handle(iLoop).
        end.
   end method.
        
   method public logical SetPosition (pKeyWhere as char):
       define variable tableNum   as integer no-undo.
       define variable tokenNum   as integer no-undo.
       define variable fieldName  as character no-undo.
       define variable fieldValue as character no-undo.
       define variable tableName as character no-undo.
       define variable keyWhere   as character extent no-undo.
       undo, throw new NotImplementedError("SetPosition (char)"). 
/*       extent(keyWhere) = extent(BufferHandles).                                                 */
/*       /* remove double blanks */                                                                */
/*       do while index(pKeyWhere,"  ") > 0:                                                       */
/*           pKeyWhere = replace(pKeyWhere,"  "," ").                                              */
/*       end.                                                                                      */
/*                                                                                                 */
/*       /* allow string passed without "where" and trim blanks front/end */                       */
/*       pKeyWhere = (if entry(1,pKeyWhere," ") <> "where" then "where " else "")                  */
/*                 + trim(pKeyWhere," ").                                                          */
/*                                                                                                 */
/*       do tokenNum = 1 to num-entries(pKeyWhere," ") by 3:                                       */
/*           if tokenNum > 1 and entry(tokenNum,pKeyWhere," ") <> "and" then                       */
/*               undo, throw new AppError("Illegal format of key where: "  + pKeyWhere).           */
/*                                                                                                 */
/*           assign                                                                                */
/*               fieldName  = entry(tokenNum + 1,pKeyWhere," ")                                    */
/*               fieldValue = entry(tokenNum + 2,pKeyWhere," ").                                   */
/*                                                                                                 */
/*           if num-entries(fieldName,".") = 1 then                                                */
/*           do:                                                                                   */
/*               if extent(BufferHandles) = 1 then                                                 */
/*                   tableName = BufferName.                                                       */
/*               else                                                                              */
/*                  undo, throw new AppError("Illegal unqualified field reference: "  + fieldName).*/
/*          end.                                                                                   */
/*          else if num-entries(fieldName,".") = 2 then                                            */
/*              assign                                                                             */
/*                  tableName = entry(1,fieldName,".")                                             */
/*                  fieldName  = entry(2,fieldName,".").                                           */
/*          else do:                                                                               */
/*              undo, throw new AppError("Too many qualifiers in field reference: "  + fieldName). */
/*          end.                                                                                   */
/*                                                                                                 */
/*          assign                                                                                 */
/*              tableNum = lookup(bufferName,Tables)                                               */
/*              keyWhere[tableNum] = (if keyWhere[tableNum] > ""                                   */
/*                                    then keyWhere[tableNum] + " and "                            */
/*                                    else "where ")                                               */
/*                                  +  bufferName + "." + fieldName                                */
/*                                  + " = "                                                        */
/*                                  + if not fieldValue begins "'" and not fieldValue begins '"'   */
/*                                    then quoter(fieldValue)                                      */
/*                                    else fieldValue.                                             */
/*                                                                                                 */
/*       end.                                                                                      */
   //    SetPosition(KeyWhere).
   end method.
  
   /* remove for or preselect from query - duplicated in DataAdminContext - belongs in utility  */
   method protected char TrimQuery(pcQuery as char):
       define variable i as integer no-undo.
       if pcQuery begins "for " then 
           return trim(substr(pcQuery,5)).  
       if pcQuery begins "preselect " then 
           return trim(substr(pcQuery,11)). 
       return pcQuery .
   end. 
    
   /* set position as returned from GetPosition */
    method public logical SetPosition (pcPosition as char extent):
        define variable hBuffer    as handle  no-undo.
        define variable iBuffer    as integer no-undo.
        define variable iTable     as integer no-undo.
        define variable rPosition  as rowid   extent no-undo.
        define variable lOk        as logical no-undo.
        undo, throw new NotImplementedError("SetPosition (char extent)"). 
/*       extent(rPosition) = QueryHandle:num-buffers.                            */
/*       do iBuffer = 1 to QueryHandle:num-buffers:                              */
/*           iTable = lookup(QueryHandle:get-buffer-handle(iBuffer):NAME,Tables).*/
/*           do:                                                                 */
/*               hBuffer = QueryHandle:get-buffer-handle(iTable).                */
/*               hBuffer:find-unique("where " + pcPosition[iTable]) no-error.    */
/*               rPosition[iBuffer] = hBuffer:rowid.                             */
/*           end.                                                                */
/*       end.                                                                    */
/*                                                                               */
      // return SetPosition(rPosition).
    end method.
 
    method protected void AssertBufferHandles(pmsg as char):
        if extent(BufferHandles) = ? then 
            undo, throw new IllegalOperationError(pMsg + " with undefined BufferHandles. BufferHandles is defined in prepare()").
       
    end method.    
  
    destructor public DbQuery () :
        DeleteQuery().
    end destructor.
end.

