 
 /*------------------------------------------------------------------------
    File        : Relation
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Wed Apr 10 11:04:51 EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/


block-level on error undo, throw.

using Ccs.Common.Application from propath.
using OpenEdge.Core.Assert from propath.
using Progress.Lang.Error from propath.
using Pmfo.Core.BusinessLogic.IBusinessEntity from propath.
using Pmfo.Core.BusinessLogic.IJoinPredicate from propath.
using Pmfo.Core.BusinessLogic.IRelation from propath.
using Pmfo.Core.BusinessLogic.JoinPredicate from propath.
using Pmfo.Core.BusinessLogic.RelationTypeEnum from propath.
using Pmfo.Core.Error.ApplicationError from propath.
using Pmfo.Core.Error.IllegalArgumentError from propath.
using Pmfo.Core.Manager.IServiceManager from propath.
using Pmfo.Util.Array from propath.
using Pmfo.Util.BufferUtil from propath.
using Pmfo.Util.DatasetUtil from propath.
using Progress.Lang.Object from propath.
using Pmfo.Core.BusinessLogic.Relation from propath.
using Progress.Json.ObjectModel.JsonObject from propath.
using Progress.Json.ObjectModel.JsonDataType from propath.
using Pmfo.Core.Error.IllegalOperationError from propath.
using Pmfo.Core.BusinessLogic.BusinessEntity from propath.
using Pmfo.Core.Error.IllegalStateError from propath.
using Pmfo.Util.JsonUtil from propath.
using Pmfo.Util.ServiceInfo from propath.

class Pmfo.Core.BusinessLogic.Relation implements IRelation:
    // Parent Resource name  
    define public property ParentName  as character no-undo get. protected set.
    // child resource name
    define public property Name        as character no-undo get. protected set.
    define public property Cardinality as RelationTypeEnum no-undo 
        get. 
        set(pCardinality as RelationTypeEnum):
         //   AssertCardinality(pCardinality).
            Cardinality = pCardinality.
        end.    
    
     // How to validate relation only needed for ManyToOne and OneToOne relations, return None for other cases  
/*    define public property ValidationRule as RelationValidationEnum                                                                              */
/*        get():                                                                                                                                   */
/*            if valid-object(ValidationRule) = false then                                                                                         */
/*            do:                                                                                                                                  */
/*                if not valid-object(Cardinality)  then                                                                                           */
/*                    return RelationValidationEnum:None.                                                                                          */
/*                else do:                                                                                                                         */
/*                    if RelationTypeEnum:OneToMany:Equals(Cardinality)                                                                            */
/*                    or RelationTypeEnum:OneToOne:Equals(Cardinality) then                                                                        */
/*                        return RelationValidationEnum:Mandatory.                                                                                 */
/*                    else                                                                                                                         */
/*                        return RelationValidationEnum:None.                                                                                      */
/*                end.                                                                                                                             */
/*            end.                                                                                                                                 */
/*            else                                                                                                                                 */
/*                return ValidationRule.                                                                                                           */
/*        end get.                                                                                                                                 */
/*        set(pValidationRule as RelationValidationEnum):                                                                                          */
/*            if RelationTypeEnum:ManyToOne:Equals(Cardinality) then                                                                               */
/*            do:                                                                                                                                  */
/*                if not RelationValidationEnum:None:Equals(pValidationRule) then                                                                  */
/*                    undo, throw new IllegalStateError(subst('Cannot set Validation Rule to "&1" for "&2" Relation',pValidationRule,Cardinality)).*/
/*            end.                                                                                                                                 */
/*            ValidationRule = pValidationRule.                                                                                                    */
/*        end set.                                                                                                                                 */
    
    define private variable mPublic    as logical init ? no-undo.
    define private variable mReadOnly  as logical init ? no-undo.
    
    define public property Public      as logical init ?  no-undo 
        get():
            if Public = ? then 
            do:
                if mPublic = ? then
                    return true.   
                return mPublic.
            end.    
            else do: 
                return Public.
            end.       
        end. 
        set(plPublic as logical ):
            if plPublic and mPublic = false then
                undo, throw new IllegalOperationError(subst("Cannot set public to true when the '&1' entity is private",name)).     
            Public = plPublic.
        end.  
        
     /* allow an alias - important if more than one relation to the same entity  */
    define public property AliasName      as character no-undo get. protected set. 
    
    define public property PublicName as character no-undo 
         get():
             return if AliasName > "" then AliasName else name.
         end.    
    // set to true if link should be populated with blank char or 0 number in single value property    
    define public property LinkToEmpty    as logical no-undo get. set. 
    
    define public property LookupFunction as character no-undo get. set. 
       
    define protected property ServiceManager as IServiceManager no-undo 
        get():
            return  cast(Application:ServiceManager,IServiceManager).
        end. 
         
    define public property ParentKeys     as character     extent  no-undo get. 
    
    // See IRelation
    define public property XRef           as logical        no-undo 
       get. 
       set(plXref as logical):
           define variable cAlias as character no-undo.
           define variable cMessage as character no-undo.
           if plXref then
           do:
               
               if AliasName = "" then 
                   undo, throw new IllegalStateError("Cannot set XRef true when there is no AliasName").
               
               calias =  AliasName.
               // the alia must exits and have correct casing for this to work
               AliasName = AssertAndFixRelation(AliasName).
               
               if compare (cAlias,"<>",AliasName,"case-sensitive") and not session:remote then
               do:
                   cMessage =  subst("The XREF relation from '&1' to '&2' has AliasName '&3' with incorrect casing. It was corrected to '&4'.",
                                ParentName,Name,cAlias,AliasName
                                    ).
                                    
                  if session:remote = false then 
                 //      message cMessage. 
                 // else
                   
                   message cMessage
                      view-as alert-box warning. 
               end.
               
           end.
           this-object:Xref = plXref.      
       end.
      
    // keep track of the dynamic dataset currently created to manage requests
    // we could alternatively store the BE 
    // one of them have to be kept alive while the relation is in use 
    define protected property LinkDataset as handle no-undo get. set.
  
    define protected property LinkBuffer as handle no-undo 
        get():
            define variable oChild        as IBusinessEntity no-undo.
            define variable hChildDataset as handle no-undo.
            
            if not valid-handle (LinkDataset) then
            do:
                // cannot store this since the object will be garbage collected when done  
                oChild = cast(ServiceManager:CreateBusinessEntity(this-object:Name),IBusinessEntity).
                oChild:GetDataset(output dataset-handle hChildDataset).
                if valid-handle(hChildDataset) then
                      LinkDataset = hChildDataset.
                return DatasetUtil:GetTopBuffer(hChildDataset,1).
            end.
            else do: 
                if not valid-handle(LinkBuffer) then
                     LinkBuffer = DatasetUtil:GetBufferBySerializeName(LinkDataset, Name).
                return LinkBuffer.     
            end.    
        end.
        set.
    
    define public property JoinPredicates as IJoinPredicate no-undo extent  get. protected set. 
        
    define public    property ForeignKey     as character      no-undo get. protected set.
    // set at creation to tell link routers to call CreateDynamicBusinessEntity
    // with parent buffer and this relation as context when starting the BE  
    // The BE must implement IDynamicBusinessEntity
    define public property IsDynamic      as logical no-undo get. set. 

   
     
    define protected property OtherKey       as character      no-undo 
        get():
            if OtherKey = "" then
            do:
                OtherKey = ServiceInfo:GetKeys(LinkBuffer).
            end.
            return OtherKey.
        end. 
        protected set. 
    
    define protected  property PublicLinkTemplate as character  no-undo get. set.
    
    define public property CanDelete      as logical init true no-undo 
        get.
        set(plCan as logical).
           if plCan = ? then 
               undo, throw new IllegalArgumentError("CanDelete set to null").
           // mReadOnly if false or ? then this is allowed
           if mReadOnly = true and plCan then 
               undo, throw new IllegalOperationError("CanDelete cannot be set to true when ReadOnly has been set to true").
           CanDelete = plCan.    
        end method. 
 
    define public property CanCreate      as logical init true no-undo 
        get.
        set(plCan as logical).
           if plCan = ? then 
               undo, throw new IllegalArgumentError("CanCreate set to null").
           
           // mReadOnly if false or ? then this is allowed
           if mReadOnly = true and plCan then 
               undo, throw new IllegalOperationError("CanCreate cannot be set to true when ReadOnly has been set to true").
           CanCreate = plCan.        
        end method. 
        
    define public property CanUpdate      as logical init true no-undo 
        get.
        set(plCan as logical).
           if plCan = ? then 
               undo, throw new IllegalArgumentError("CanUpdate set to null").
           
           // mReadOnly if false or ? then this is allowed
           if mReadOnly = true and plCan then 
               undo, throw new IllegalOperationError("CanUpdate cannot be set to true when ReadOnly has been set to true").
           CanUpdate = plCan.    
        end method. 
     
    define public property ReadOnly       as logical  init ?  no-undo 
        get():
           if mReadOnly = ? then 
               return CanDelete = false and CanCreate = false and CanUpdate = false. 
           else 
               return mReadOnly.    
        end.
        set(plReadonly as logical) :
            if (CanDelete = true and CanCreate = true and CanUpdate = true) 
            or (CanDelete = false and CanCreate = false and CanUpdate = false) then 
            do: 
                mReadonly = plReadOnly.
                CanCreate = false.
                CanDelete = false.
                CanUpdate = false.
            end.
            else 
                undo, throw new IllegalOperationError("ReadOnly cannot be set when one or more of CanUpdate, CanUpdate or CanDelete is false").
                    
        end.
      
   /*------------------------------------------------------------------------------
     Purpose: Defines a relation to another table using primary key
     Notes:
    ------------------------------------------------------------------------------*/
    
    // for subclass that manages fields differently 
    constructor protected Relation (pParent as character, pChild as character):
        Assert:NotNullOrEmpty(pParent,"Parent resource name").
        Assert:NotNullOrEmpty(pChild,"Foreign/child resource name").
        ParentName      = pParent.
        Name            = AssertAndFixRelation(pChild).  
        Cardinality     = RelationTypeEnum:OneToMany. 
    end constructor.
    
    constructor public Relation (pParent as character, pChild as character,pForeignKey as character ):
        Assert:NotNullOrEmpty(pParent,"Parent resource name").
        Assert:NotNullOrEmpty(pChild,"Foreign/child resource name").
        Assert:NotNullOrEmpty(pForeignKey,"Foreign key").
        ParentName      = pParent.
        Name            = AssertAndFixRelation(pChild).
          
        ForeignKey      = pForeignKey.
        Cardinality     = RelationTypeEnum:ManyToOne. 
    end constructor.
    
    constructor public Relation (pParent as character, pChild as character,pParentField as character, pChildField as character ):
        this-object(pParent,pChild,Array:ToArray(pParentField),Array:ToArray(pChildField)).
    end constructor.
    
    constructor public Relation (pParent as character, pChild as character,pParentField as character, pChildField as character , pcAliasNAme as character):
        this-object(pParent,pChild,Array:ToArray(pParentField),Array:ToArray(pChildField),pcAliasName).
        
    end constructor.
    
    constructor public Relation (pParent as character, pChild as character,pParentField as character, pChildField as character,pCardinality as RelationTypeEnum ):
        this-object(pParent,pChild,Array:ToArray(pParentField),Array:ToArray(pChildField),pCardinality).
    end constructor.
    
    constructor public Relation (pParent as character, pChild as character,pParentField as character, pChildField as character,pCardinality as RelationTypeEnum ,pcAliasName as character):
        this-object(pParent,pChild,Array:ToArray(pParentField),Array:ToArray(pChildField),pCardinality,pcAliasName).
    end constructor.
    
    constructor public Relation (pParent as character, pChild as character,pPredicate as IJoinPredicate ):
        this-object(pParent,pChild,cast(Array:ToArray(pPredicate),IJoinPredicate)). 
    end constructor.
    
    constructor public Relation (pParent as character, pChild as character,pPredicate as IJoinPredicate, pCardinality as RelationTypeEnum ):
        this-object(pParent,pChild,cast(Array:ToArray(pPredicate),IJoinPredicate),pCardinality). 
    end constructor.
    
    constructor public Relation (pParent as character, pChild as character,pPredicate as IJoinPredicate, pCardinality as RelationTypeEnum, pcAliasNAme as character ):
        this-object(pParent,pChild,cast(Array:ToArray(pPredicate),IJoinPredicate),pCardinality,pcAliasName). 
    end constructor.
    
    constructor public Relation (pParent as character, pChild as character,pParentFields as character extent, pChildFields as character extent):
        this-object(pParent,pChild,CreateJoinPredicates(pParentFields,pChildFields)).
    end constructor.
    
    constructor public Relation (pParent as character, pChild as character,pParentFields as character extent, pChildFields as character extent, pcAliasName as character):
        this-object(pParent,pChild,CreateJoinPredicates(pParentFields,pChildFields),pcAliasName).
    end constructor.
  
    constructor public Relation (pParent as character, pChild as character,pParentFields as character extent, pChildFields as character extent,pCardinality as RelationTypeEnum):
        this-object(pParent,pChild,CreateJoinPredicates(pParentFields,pChildFields),pCardinality).
    end constructor.
    
    constructor public Relation (pParent as character, pChild as character,pParentFields as character extent, pChildFields as character extent,pCardinality as RelationTypeEnum, pcAliasName as character):
        this-object(pParent,pChild,CreateJoinPredicates(pParentFields,pChildFields),pCardinality,pcAliasName).
    end constructor.
    
    constructor public Relation (pParent as character, pChild as character,pPredicates as IJoinPredicate extent ):
        this-object(pParent,pchild,pPredicates,RelationTypeEnum:OneToMany).
    end constructor.
    
    constructor public Relation (pParent as character, pChild as character,pPredicates as IJoinPredicate extent, pcAliasName as character ):
        this-object(pParent,pchild,pPredicates,RelationTypeEnum:OneToMany,pcAliasName).
    end constructor.
    
    constructor public Relation (pParent as character, pChild as character,pPredicates as IJoinPredicate extent, pCardinality as RelationTypeEnum ):
        super().
        Assert:NotNullOrEmpty(pParent,"Parent resource name").
        Assert:NotNullOrEmpty(pChild,"Child resource name").
        Assert:HasDeterminateExtent(pPredicates,"Join Predicates").
        Assert:NotNull(pCardinality,"Cardinality").
        ParentName      = pParent.
        Name            = AssertAndFixRelation(pChild).  
        JoinPredicates  = pPredicates. 
        Cardinality     = pCardinality. 
    end constructor.
    
    constructor public Relation (pParent as character, pChild as character,pPredicates as IJoinPredicate extent, pCardinality as RelationTypeEnum, pcAliasName as character ):
        this-object(pParent,pChild,pPredicates,pCardinality).
        AliasName = pcAliasName.
    end constructor.
    
    // separate out for override in subclass
    method protected character AssertAndFixPrivateEntity(pcName as character) :
        if ServiceManager:CanFindBusinessEntity(input-output pcName) then
            return pcName.
        else 
            undo, throw new IllegalOperationError(subst("'&1' is not a valid Business Entity Resource",pcname)).
    end method.
    
    // allow setter to check ValidationRule
/*    method protected void AssertCardinality(pCardinality as RelationTypeEnum):                                                                            */
/*        if RelationTypeEnum:ManyToOne:Equals(pCardinality) then                                                                                           */
/*        do:                                                                                                                                               */
/*            if not RelationValidationEnum:None:Equals(ValidationRule) then                                                                                */
/*                undo, throw new IllegalStateError(subst('Cannot set Cardinality to "&1" when ValidationRule is set to "&2"',pCardinality,ValidationRule)).*/
/*        end.                                                                                                                                              */
/*    end method.                                                                                                                                           */
    
    method protected character AssertAndFixRelation(input pcname as char):
        define variable lok as logical no-undo.
        // utilize the can find that also fixes the name casing 
        // the entity does not need to be public  
         
        lok = ServiceManager:CanFindPublicBusinessEntity(input-output pcName).
        if lok then 
        do: 
            mPublic = true.
            return pcName.
        end.
        else
            return AssertAndFixPrivateEntity(pcname).
                
    end method.    
     
    method protected IJoinPredicate extent CreateJoinPredicates(pParentFields as character extent, pChildFields as character extent):
        define variable i as integer no-undo.
        define variable oPredicates as IJoinPredicate extent no-undo.
        Assert:IsTrue(extent(pParentFields) = extent(pChildFields),"Parent and Child fields must have the same number of extents").
        extent(oPredicates) = extent(pParentFields).
        do i = 1 to extent(oPredicates):
            oPredicates[i] = new JoinPredicate(pParentFields[i],pChildFields[i]).
        end.    
        return oPredicates.
    end method.    
    
    method public character GetPublicLink(pJson as JsonObject,phSchema as handle):
        define variable cKey as character no-undo.
        
        if not RelationTypeEnum:ManyToOne:Equals(Cardinality) then 
            return PublicName.
        else do: 
            cKey = GetPublicKeyExpression(pJson,phSchema).
            if cKey = ? then 
                return ?.
            else     
                return subst("&1(&2)",PublicName,cKey).
        end.        
    end method.
    
    method public character GetPublicKeyTemplate(phSchema as handle):
        return GetPublicKeyExpression(?,phSchema).
    end.    
    
    method protected character GetPublicKeyExpression(pJson as JsonObject,phSchema as handle):
        define variable iFld              as integer   no-undo.
        define variable hFld              as handle    no-undo.
        define variable cFld              as character no-undo.
        define variable cJoinList         as character no-undo.
        define variable cExpression       as character no-undo.
        define variable lUseTemplate      as logical   no-undo.
        define variable iCount            as integer   no-undo.
        define variable cValue            as character no-undo.
        define variable cChildField       as character no-undo. 
        define variable cFieldValues      as character no-undo extent.
        define variable cNames            as character no-undo.
        define variable lSinglePrimaryKey as logical   no-undo.
        define variable iNumber           as int64 no-undo.
        define variable cCharValue        as character no-undo.
       
        if ForeignKey > "" then
        do:
            cJoinList = ForeignKey.
            extent(cFieldValues) = 1.
            lSinglePrimarykey = true.    
        end.    
        else do:
            cJoinList = PairsList().
            extent(cFieldValues) = extent(JoinPredicates).
           // lSinglePrimaryKey = extent(cFieldValues) = 1 and BufferUtil:GetKeys( the other table) = entry(2,cJoinList).  
        end.
        
        if valid-object(pJson) then
        do:
            cNames = Array:ToString(pJson:GetNames()).                
            do iFld = 1 to num-entries(cJoinList) by 2:
                cFld = entry(iFld,cJoinList).
                hFld = phSchema:buffer-field(cFld).
                
                // if hidden the field will not be in the json either 
                // this can happen with select - 
                // This is not a problem - the rule is that if you don't have the field then 
                // you don't need the link
                if hFld:serialize-hidden then 
                    return ?.
                
                cFld = hFld:serialize-name.
                // make sure correct casing is used - these names comes from user 
                cFld  = entry(lookup(cFld,cNames),cNames).
                iCount = iCount + 1.
                if pJson:GetType(cFld) = JsonDataType:NUMBER then
                do:
                    cFieldValues[iCount] = JsonUtil:GetStringValue(pJson, cFld).
                    if decimal(cfieldValues[iCount]) = 0 
                    and not LinkToEmpty 
                    and num-entries(cJoinList) <= 2 then 
                        return ?.
                end.
                else if pJson:GetType(cFld) = JsonDataType:BOOLEAN then 
                do:
                    cFieldValues[iCount] = string(pJson:GetLogical(cFld),"true/false").
                end.
                else if pJson:GetType(cFld) = JsonDataType:STRING then
                do:
                    cCharValue = pJson:GetCharacter(cFld).
                    if cCharValue = "" 
                    and not LinkToEmpty 
                    and num-entries(cJoinList) <= 2 then 
                        return ?.
                    else  
                        cFieldValues[iCount] = quoter(cCharValue,"'"). 
                end.
                else if pJson:GetType(cFld) = JsonDataType:NULL then do:
                    // if single key and null then we assume link is useless 
                    // add parameter if neeed to return null 
                    if num-entries(cJoinList) <= 2 then 
                        return ?. 
                    cFieldValues[iCount] = "null".
                end.    
            end.
        end.
        else do:
             do iFld = 1 to num-entries(cJoinList) by 2:
                cFld = entry(iFld,cJoinList).
                hFld = phSchema:buffer-field(cFld).
                cFld = hFld:serialize-name.
                iCount = iCount + 1.
                cFieldValues[iCount] = "@" + cFld.
            end.    
        end.    
        if not lSinglePrimaryKey then
        do: 
            lUseTemplate = extent(cFieldValues) <= 9. 
            if lUseTemplate = false or PublicLinkTemplate = "" then
            do:  
                iCount = 0.
                do iFld = 2 to num-entries(cJoinList) by 2:
                    cFld = entry(iFld,cJoinList).
                    hFld = LinkBuffer:buffer-field(cFld).
                    iCount = iCount + 1.
                    cValue = if lUseTemplate then "&" + string(iCount) else cFieldValues[iCount].
                    
                    cExpression = cExpression 
                                + (if cExpression = "" then "" else ",")
                                + subst("&1=&2",hFld:serialize-name,cValue).
                                 
                end.
                if lUseTemplate then 
                    PublicLinkTemplate = cExpression.          
                else 
                    return cExpression.       
            end.
            return Array:SubstituteArray(PublicLinkTemplate, cFieldValues).
        end.
        else do:
            return cFieldValues[1].
        end.        
       
        catch e as Progress.Lang.Error :
            undo, throw e.            
        end catch.     
    end method.    
    
    method public character PairsList().
        define variable i            as integer no-undo.
        define variable cList        as character no-undo.
        define variable cDlm         as character no-undo.
       
        if extent(JoinPredicates) > 0 then
        do i = 1 to extent(JoinPredicates):
            assign 
                cList = clist + cDlm + subst("&1,&2",JoinPredicates[i]:ParentField,JoinPredicates[i]:ChildField)
                cDlm  = ",".
        end.    
        else if ForeignKey > "" then 
        do:
            cList = clist + cDlm + subst("&1,&2",ForeignKey,OtherKey).     
        end.    
        else // the constructors should prevent this , but 
            undo, throw new IllegalStateError("Relation has no predicate or ForeignKey").    
           
        return cList.
        catch e as Progress.Lang.Error :
            undo, throw new IllegalStateError(subst("Could not obtain relation information for &1",Name),e).    
        end catch.
    end method.
     
    
    method override public character ToString( ):
        return BuildString(this-object).
    end method.
    
    method override public logical Equals(pobject as Object ):
        if super:Equals(pObject) then 
            return true.
        
        if type-of(pObject,Relation) then do:
            return ToString() = pObject:ToString().
        end.
        
        if type-of(pObject,IRelation) then do:
            return ToString() = BuildString(cast(pObject,IRelation)).   
        end.
        
        return false. 
    end method.     
    
    method protected character BuildString(pRelation as IRelation):
        define variable cString as character no-undo.
        cString = subst("&1 &2",pRelation:ParentName,pRelation:PublicName). 
        if extent(pRelation:joinPredicates) <> ? then
            cString = cString + subst(" relation-fields(&1)",pRelation:PairsList()).
        else 
            cString = cString + subst(" foreign-key &1",pRelation:ForeignKey).
             
        return cString + subst(" &1",pRelation:Cardinality:ToString()).
    end.
    
    method public override Object Clone():
        define variable oRelation as Relation no-undo.
        if extent(JoinPredicates) > 0 then 
            oRelation = new Relation(ParentName,this-object:Name,JoinPredicates,Cardinality).
        else 
            oRelation = new Relation(ParentName,this-object:Name,ForeignKey).
        
        oRelation:AliasName = AliasName.
        oRelation:Public = Public.
    //    oRelation:ValidationRule = ValidationRule.
        if mReadOnly <> ? then 
            oRelation:ReadOnly = ReadOnly.    
        oRelation:CanCreate = CanCreate.
        oRelation:CanUpdate = CanUpdate.
        oRelation:CanDelete = CanDelete.
          
        return oRelation. 
    end.    
    
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	destructor public Relation ( ):
        if valid-handle(LinkDataset) then
            delete object LinkDataset. 
	end destructor.
    
     
end class.