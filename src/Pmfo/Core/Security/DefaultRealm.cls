
 /*------------------------------------------------------------------------
    File        : Realm
    Purpose     : Sample OOABL Server class of OERealm
    Syntax      : 
    Description : Singleton class that implements IHybridRealm for use as
                  a single point of authentication for OE Realm Clients
                   
    Author(s)   : hdanielsen
    Created     : 3/10/2021
    Notes       : This is a sample that does nothing other than putting out 
                  messages to the log from methods called from Spring  
  ----------------------------------------------------------------------*/
 
using Progress.Security.Realm.IHybridRealm.
 
routine-level on error undo, throw.

class Pmfo.Core.Security.DefaultRealm implements IHybridRealm:

    
    
  //  define private variable m_spaProps as class OpenEdge.Security.Util.Properties.
      
    /*------------------------------------------------------------------------------
     Purpose: Default constructor
     Notes: Reads a property file names "spaservice.properties" to intialize:
         1. ValidateCP - If true, the OERealm methods will validate if the request came 
                         from a trusted client by checking for existence of a 
                         sealed client-Principal(C-P)
                       - If false, no such C-P validation will not be performed.  
                       - This serves as an additional layer of security.It is highly
                         recommended that this validation be turned on
                       - In order to turn on this additional security validation:
                         1. The Implementor of OE Realm will create a .cp file using
                         genspacp utility and distribute it to the trusted OERealm Clients.
--------------------------------------------------------------------
The syntax of genspacp is as below:
genspacp -password <text> [-user <text> -role <text> -domain <text> -file <text>]
--------------------------------------------------------------------
                         2. The .cp file will be sealed by the "password" field provided
                            to the utility   
         2. Password  -  The value will be used to validate the incoming C-P
                      -  It must match the value provided while generating .cp using
                         genspacp utility
                      -  It is applicable only when validateCP is true
         3. Role      -  The value must match the role that the incoming C-P contains
                      -  It must match the value provided while generating .cp using
                         genspacp utility
                      -  It is applicable only when validateCP is true
         4. DebugMsg  - If true, debug messages will be logged in server log file
    ------------------------------------------------------------------------------*/
        
    constructor public DefaultRealm (  ):
        super ().
    end constructor.
 
    /*------------------------------------------------------------------------------
     Purpose: Returns the attribute value of a user
     Notes: 
     INPUT : theUserId - positive user number
                       - In this example it corresponds to _User._User_number.
             attrName  - String name for an attribute to be fetched 
                       - Refer to OERealmDefs for the attribute name that this
                         sample supports
     Throws: 
     Progress.Lang.AppError("Unauthorized client", 1).
     Progress.Lang.AppError("Invalid User Id")             
    ------------------------------------------------------------------------------*/

    method public character GetAttribute( input theUserId as integer, input attrName as character ):
        define variable cRoles as character no-undo init "PSCUser,PSCAdmin,PSCDebug".
  
        case attrname: 
            when "ATTR_ENABLED" then return "T".
            when "ATTR_LOCKED"  then return "F".
            when "ATTR_EXPIRED" then return "F".
            when "ATTR_ROLES"   then return cRoles.
            
            otherwise return ?.  
        end.     
       
        /**
         ATTR_ROLES  string-extent   "" or "xxxx[,yyyy,zzzz]"
         Note: The OERealm service interface must return roles without the ROLE_ prefix. 
         For example, the interface must return PSCUser instead of ROLE_PSCUser. 
         These roles are used in oeablSecurity-xxxxx-oerealm.xml grant/deny access to URL/method combinations and will 
         be used by the external security as part of its authorization functions.
         ATTR_ENABLED    boolean     A character from the {T,t,F,f,Y,y,N,n,0,1} set. 
                         T,t,Y,y,1 denotes that the user is enabled. 
                         F,f,N,n,0 denotes that the user is disabled.
         ATTR_LOCKED     boolean     A character from the {T,t,F,f,Y,y,N,n,0,1} set.
                         T,t,Y,y,1 denotes that the user is locked. 
                         F,f,N,n,0 denotes that the user is not locked.
         ATTR_EXPIRED    boolean     A character from the {T,t,F,f,Y,y,N,n,0,1} set.
                         T,t,Y,y,1 denotes that the user is expired.
                         F,f,N,n,0 denotes that the user is not expired. Spring Security does not allow expired users.
        ***/
        
    end method.
    
    /*------------------------------------------------------------------------------
     Purpose:Returns all the attributes defined for a user as a comma separeted list
     Notes: 
     INPUT : theUserId - positive user number
                       - In this example it corresponds to _User._User_number.
     Throws: 
     Progress.Lang.AppError("Unauthorized client", 1).
     Progress.Lang.AppError("User not found"). 
    ------------------------------------------------------------------------------*/
    method public character extent GetAttributeNames( input theUserId as integer ):
        define variable results as character extent no-undo.
        return results.

    end method.
    
    /*------------------------------------------------------------------------------
     Purpose: Returns the fully qualified usernames a matching a query pattern
     Notes: 
     INPUT : queryString - character value which against which the userid should be matched
    ------------------------------------------------------------------------------*/
    method public character extent GetUserNamesByQuery( input queryString as character ):
        
        define variable results as character extent no-undo.
        message program-name(1).
        
        return results. 
    end method.
    
    method public character extent GetUserNames(  ):
        
        define variable results as character extent no-undo.
        message program-name(1).
       
        return results. 
    end method.
    
    
    /*------------------------------------------------------------------------------
     Purpose: Returns the fully qualified usernames whose attributes match 
     Notes: 
     INPUT : attrName  - character value representing the attribute name
             attrValue - character value representing the attribute value
    ------------------------------------------------------------------------------*/
    method public character extent GetUserNamesByQuery( input attrName as character, input attrValue as character ):
        
        define variable results as character extent no-undo.
        message program-name(1).
        
        return results.
        

    end method.

    /*------------------------------------------------------------------------------
     Purpose: Removes an attribute from User account based on unique user number
     Notes: 
     INPUT : theUserId - positive user number
                       - In this example it corresponds to _User._User_number.
             attrName  - String representing the attribute name to be removed
                       - Refer to OERealmDefs for the attribute name that this
                         sample supports
     Throws: 
     Progress.Lang.AppError("Unauthorized client", 1).
     Progress.Lang.AppError("Invalid User Id").            
    ------------------------------------------------------------------------------*/
    method public logical RemoveAttribute( input theUserId as integer, input attrName as character ):
        
        define variable retVal as logical no-undo initial true.
        message program-name(1).
        return retVal.

    end method.

    /*------------------------------------------------------------------------------
     Purpose: Sets an attribute value for a user
     value
     Notes: 
     INPUT : theUserId - positive user number
                       - In this example it corresponds to _User._User_number.
             attrName  - String representing the attribute name to be removed
                       - Refer to OERealmDefs for the attribute name that this
                         sample supports
             attrValue - value of an attribute
     Throws: 
     Progress.Lang.AppError("Unauthorized client", 1).
     Progress.Lang.AppError("Invalid User Id").          
    ------------------------------------------------------------------------------*/
    method public logical SetAttribute( input theUserId as integer, input attrName as character, input attrValue as character ):
        
        define variable retVal as logical no-undo initial true.
        
        message program-name(1).
        
        return retVal.

    end method.
    
     /*------------------------------------------------------------------------------
     Purpose: Sets an password for a given user
     Notes: 
     INPUT : theUserId - positive user number
                       - In this example it corresponds to _User._User_number.
             password  - String value to be set as the password
     The incoming request must contain a C-P to perform this operation   
     Throws: 
     Progress.Lang.AppError("Unauthorized client", 1).
     Progress.Lang.AppError("Invalid User Id").             
    ------------------------------------------------------------------------------*/
    method public logical SetAttribute( input theUserId as integer,  input password as character ):
        
        define variable retVal as logical no-undo initial true.
        
        message program-name(1).
        return retVal.

    end method.
   


    /*------------------------------------------------------------------------------
     Purpose: validate a password for a given user
     Notes: 
     INPUT : theUserId - positive user number
                       - In this example it corresponds to _User._User_number.
             password  - String value to be set as the password
     Throws: 
     Progress.Lang.AppError("Unauthorized client", 1).
     Progress.Lang.AppError("Invalid User Id").           
    ------------------------------------------------------------------------------*/
    method public logical ValidatePassword( input theUserId as integer, input password as character ):
        
        define variable retVal as logical no-undo initial false.      
        
/*        if m_validateCP then do:                                                 */
/*            if this-object:ValidateClient() = false then                         */
/*                undo, throw new Progress.Lang.AppError("Unauthorized client", 1).*/
/*        end.                                                                     */
/*                                                                                 */
/*        if m_debugMsg then do:                                                   */
/*            message "Validate password using basic".                             */
/*            message "Password: " password.                                       */
/*        end.                                                                     */

        message program-name(1) "userid" theuserid  "password" password.
        
        return true.
        
    end method.
    
    /*------------------------------------------------------------------------------
     Purpose: validate a password for a given user using digest
     Notes: 
     INPUT : theUserId - positive user number
                       - In this example it corresponds to _User._User_number.
             digest    - String representing the digest value of the password
             nounce    - String representing the nounce
             timestamp - String representing the timestamp.
             
     NOTE: Digest version of ValidatePassword requires that the passwords be 
     stored in such a way that they can be retrieved as clear text. 
     Since, the _password field of _User table stores password in one way 
     encoded format from which the clear-text password cannot be retrieved, 
     therefore, in this sample, we have encrypted the passwords using 
     db/encPwd.p and stored  in _User._U-misc2[1]. 
     The  encrypted passwords stored thus in the _User._U-misc2[1] can be 
     retrieved as clear-text using db/decPwd.p.
     Please note that this is just a sample implementation, you can store the 
     passwords in any other way of your choice such that it can be recovered
     as a clear-text.

     Throws: 
     Progress.Lang.AppError("Unauthorized client", 1).
     Progress.Lang.AppError("User not found").   
     Progress.Lang.AppError("Old timestamp sent").        
    ------------------------------------------------------------------------------*/
    method public logical ValidatePassword( input theUserId as integer, input digest as character, input nonce as character, input timestamp as character ):
         message program-name(1) "userid" theUserId "figest" digest "nonce" nonce.
       
         return true. 
    end method.

    /*------------------------------------------------------------------------------
     Purpose: validate if the given user exists and if it exists returns the user number
     Notes: 
     INPUT : userName - Fully qualified user name
     Throws: 
     Progress.Lang.AppError("Unauthorized client", 1).
     Progress.Lang.AppError("User not found", 8).
     Progress.Lang.AppError("User number unknown for the user " + userName).
    ------------------------------------------------------------------------------*/
    method public integer ValidateUser( input userName as character ):
        message program-name(1).
        return 1. 
       //   undo, throw new Progress.Lang.AppError("User not found", 8).
    end method.

end class.