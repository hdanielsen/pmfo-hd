/*------------------------------------------------------------------------
    File           : Pmfo.Core.Security.OEUserRealm
    Purpose        : Implement Progress.Security.Realm.IHybridRealm.
    Description    : Applies back-end logic to OEReam authorization requests.
    Original Author: PDustin Grau (dugrau@progress.com)
    Created        : Wed Dec 10 09:13:43 EST 2014
    Notes          : This is a trimmed down version of Spark.Core.Security.OEUserRealm
                     IMPORTANT: It is less secure as it does not use the .cp and does not
                     support and there is no config to load
                   - It does not have all the various password encryption variations
                   - It is defined as abastract and the intention is that the abstract methods also 
                     are overridable in Spartk version, so that the subclass can switch to the 
                     Spark version with no changes. 
                      - some protected password related methids are abstract    
                      - IAuthHandler methods are abstract  
 ----------------------------------------------------------------------*/

 
 

block-level on error undo, throw.

using Progress.Security.Realm.IHybridRealm from propath.
using Spark.Core.Constant.OERealmDefs from propath.
using Spark.Core.Security.IAuthHandler from propath.

class Pmfo.Core.Security.OEUserRealm  implements IHybridRealm, IAuthHandler abstract:
    constructor public OEUserRealm ( ):
        super().
    end constructor.

    /**************************** IAuthHandler Methods *****************************/

    /**
     * These methods are meant to be "helpers" to the larger tasks provided by the
     * IHybridRealm class. NOTE: The default for these methods is to return a passing
     * value, so if your application does not support a particular function, leaving
     * the default method will allow authentication to continue without error.
     */
     /*------------------------------------------------------------------------------
     Purpose: Obtain the UserID for a given username (and domain, optional).
     Notes:
        @param pcUsername Username portion of credentials
        @param pcDomainName Domain portion of credentials
        @return Unique identifier for user, as integer
    ------------------------------------------------------------------------------*/
    method public abstract integer getUserID ( input pcUsername   as character,
                                               input pcDomainName as character ).
   
    /*------------------------------------------------------------------------------
     Purpose: Return active status for a given UserID.
     Notes:
        @param piUserID UserID value, as integer
        @return Logical pass/fail for operation
    ------------------------------------------------------------------------------*/
    method public abstract logical isActive ( input piUserID as integer ).

    /*------------------------------------------------------------------------------
     Purpose: Return expiration status for a given UserID.
     Notes:
        @param piUserID UserID value, as integer
        @return Logical pass/fail for operation
    ------------------------------------------------------------------------------*/
    method public abstract logical isExpired ( input piUserID as integer ).
   
    /*------------------------------------------------------------------------------
     Purpose: Return locked status for a given UserID.
     Notes:
        @param piUserID UserID value, as integer
        @return Logical pass/fail for operation
    ------------------------------------------------------------------------------*/
    method public abstract logical isLocked ( input piUserID as integer ). 
    
    /*------------------------------------------------------------------------------
     Purpose: Return available roles for a given UserID.
     Notes:
        @param piUserID UserID value, as integer
        @return Comma-separated list of roles (without ROLE_ prefix)
    ------------------------------------------------------------------------------*/
    method public abstract character getUserRoles ( input piUserID as integer ).
   
    
    /******************* protected abstract methods that exists in Spark also  ****************************/
    
    /*------------------------------------------------------------------------------
     Purpose: Compare the user-supplied password to a hashed password, with UserID.
     Notes: Implement as a protected method in your own class.
        @param piUserID UserID value, as integer
        @param pcPassword Externally supplied password by user
        @param pcHashPass Internally stored password for user
        @return Logical pass/fail for operation
    ------------------------------------------------------------------------------*/
    method protected abstract logical checkPasswordHash ( input piUserID   as integer,
                                                 input pcPassword as character,
                                                 input pcHashPass as character ).
   

    /*------------------------------------------------------------------------------
     Purpose: Obtain the password for a given UserID.
     Notes: Implement as a protected method in your own class.
         @param piUserID UserID value, as integer
         @return Internally stored password value
    ------------------------------------------------------------------------------*/
    method protected abstract character getPassword ( input piUserID as integer ).

   /******************* protected method that exists in Spark also  ****************************/
    
    /*------------------------------------------------------------------------------
     Purpose: Perform an action after authentication (validatePassword).
     Notes: Implement as a protected method in your own class, if needed.
         @param piUserID UserID value, as integer
         @param lRetVal Result of authentication action, as logical
         @return Void
      NOT abstract - override if needed    
    ------------------------------------------------------------------------------*/
    method protected void postAuthenticate ( input piUserID as integer,
                                             input lRetVal  as logical ):
        /* Override to note this user's success/failure to authenticate. */
    end method. /* postAuthenticate */

    /**************************** IHybridRealm Methods ******************************/

    /**
     * These methods come from the original IHybridRealm class within OpenEdge,
     * and are broken down to call more atomic methods to obtain data. The goal
     * is to not have to know how these are implemented directly, but to only
     * require the individual components be overridden when necessary.
     */
    
    // final - subclasses should add logic to the 4 methods called from here
    method public final character GetAttribute ( input piUserID   as integer,
                                           input pcAttrName as character ):
                                               
        case pcAttrName:
            when OERealmDefs:REALM_ATTR_ENABLED then
                /* Value for OERealm ATTR_ENABLED */
                return string(isActive(piUserID), "1/0").
            when OERealmDefs:REALM_ATTR_LOCKED then
                    /* Value for OERealm ATTR_LOCKED */
                return string(isLocked(piUserID), "1/0").
            when OERealmDefs:REALM_ATTR_EXPIRED then
               /* Value for OERealm ATTR_EXPIRED */
                return string(isExpired(piUserID), "1/0").
            when OERealmDefs:REALM_ATTR_ROLES then
                /* Generate a comma-delimited list of roles associated with this user.
                 * These roles will map to intercept-url patterns to grant access to
                 * various resource URI's as defined for the REST adapter services.
                 * A basic role of "EndUser" (mapped to ROLE_EndUser) is required for
                 * minimum secured access, based on this demo code. Your actual app
                 * security may vary.
                 */
                return getUserRoles(piUserID).
            otherwise
                return  ?.
        end case.
        
        catch err as Progress.Lang.Error:
            message substitute("GetAttribute: &1", err:GetMessage(1)).
            return ?.
        end catch.
    end method. /* GetAttribute */


    method public character extent GetAttributeNames ( input piUserID as integer ):
        /* Not configured. */
        define variable cpcAttrNames as character extent no-undo.
        return cpcAttrNames.
    end method. /* GetAttributeNames */


    method public character extent GetUsernames ( ):
        /* Not configured*/
        define variable cUsernames as character extent no-undo.
        return cUsernames.
    end method. /* GetUsernames */


    method public character extent GetUsernamesByQuery ( input pcQueryString as character ):
        /* Not configured. */
        define variable cUsernames as character extent no-undo.
        return cUsernames.
    end method. /* GetUsernamesByQuery */


    method public character extent GetUsernamesByQuery ( input pcAttrName  as character,
                                                         input pcAttrValue as character ):
        /* Not configured */
        define variable cUsernames as character extent no-undo.
        return cUsernames.
    end method. /* GetUsernamesByQuery */


    method public logical RemoveAttribute ( input piUserID   as integer,
                                            input pcAttrName as character ):
        /* Not configured  */
        return true.
    end method. /* RemoveAttribute */

    method public logical SetAttribute ( input piUserID    as integer,
                                         input pcAttrName  as character,
                                         input pcAttrValue as character ):
        /* Not configured' */
        return true.
    end method. /* SetAttribute */

    
    // final - subclasses implement checkPasswordHash and getPassword (optionally postAuthenticate)
    method public final logical ValidatePassword ( input piUserID   as integer,
                                                   input pcPassword as character ):
         
         define variable lRetVal as logical no-undo initial ?.
         if piUserID eq 0 and pcPassword eq "undefined" then
            assign pcPassword = "".

       
        assign lRetVal = checkPasswordHash(piUserID, pcPassword, getPassword(piUserID)).
        
        postAuthenticate(piUserID, lRetVal).

        return lRetVal.

        catch err as Progress.Lang.Error:
            message substitute("ValidatePassword: &1", err:GetMessage(1)).
            return false.
        end catch.
    end method. /* ValidatePassword */


    method public logical ValidatePassword ( input piUserID    as integer,
                                             input pcDigest    as character,
                                             input pcNonce     as character,
                                             input pcTimestamp as character ):
        /* Not configured */
        message "ValidatePassword digest - should not be in use" piUserID ":" pcDigest ":" pcNonce ":" pcTimestamp.

        return false.

         
    end method. /* ValidatePassword */

    // final - subclasses implements getUserID
    method public final integer ValidateUser ( input pcUsername as character ):
        /* Users are identified by a unique integer value. */
        define variable cUsername   as character no-undo.
        define variable cDomainName as character no-undo.

        assign
            cUsername   = entry(1, pcUsername, "@")
            cDomainName = entry(2, pcUsername, "@") when num-entries(pcUsername, "@") eq 2
            no-error
            .

        return getUserID(cUsername, cDomainName).

        catch err as Progress.Lang.Error:
            message substitute("ValidateUser: &1", err:GetMessage(1)).
            return -1.
        end catch.
    end method. /* ValidateUser */

end class.
