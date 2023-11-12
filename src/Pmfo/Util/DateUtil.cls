 
 /*------------------------------------------------------------------------
    File        : DateUtil
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Sun May 19 14:08:40 EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using Pmfo.Util.Array from propath.
using Pmfo.Core.Error.IllegalArgumentError from propath.

block-level on error undo, throw.

class Pmfo.Util.DateUtil: 
    define static public property WeekDays as character init "Sunday,Monday,Tuesday,Wednesday,Thursday,Friday,Saturday"  get.
    
    method public static character GetDayOfWeekName(piDay as integer) :
        if piDay < 1  or piDay > 7 then
            undo, throw new IllegalArgumentError(subst("Day Number &1",piDay)).
        return entry(piDay,WeekDays). 
    end method.
    
    method public static character GetDayOfWeekShortName(piDay as integer) :
        return substr(GetDayOfWeekName(piDay),1,3).     
    end method.
        
    method public static datetime GetDateTimeFromDateSeconds(pDate as date,pTime as integer):
        return datetime(pDate,ptime * 1000). 
    end method.    
    
    method public static datetime-tz GetDateTimeTzFromDateSeconds(pDate as date,pTime as integer):
        return datetime-tz(pDate,ptime * 1000). 
    end method.
    
    method public static void GetDateAndTime(pDatetime as datetime, output pdate as date, output pTime as integer):
        pDate = date(pDateTime).
        pTime = mtime(pDateTime) / 1000.
    end method.    
    
    method public static void GetDateAndTime(pDatetime as datetime-tz, output pdate as date, output pTime as integer):
        pDate = date(pDateTime).
        pTime = mtime(pDateTime) / 1000.
    end method.    
    
    method public static integer GetTimeInSeconds(pDatetime as datetime-tz):
        return integer(mtime(pDateTime) / 1000).
    end method.
        
    method public static integer GetTimeInSeconds(pDatetime as datetime):
        return integer(mtime(pDateTime) / 1000).
    end method.
    
    // convert a iso-date to date     
    method static public date DateFromIsoDate(pcDate as character):
        if pcDate = ? then 
            return ?.
        define variable cMsgTemplate as character init "'&1' is not a valid iso-date" no-undo.
        if substr(pcdate,5,1) <> "-"  
        or substr(pcdate,8,1) <> "-" 
        or length(pcdate) <> 10 then
             undo, throw new IllegalArgumentError(subst(cMsgTemplate,pcDate)).   
         
        return DateFromOtherFormat(pcDate,"ymd").
        catch e as Progress.Lang.Error :
            if e:GetMessageNum(1) = 85 then 
                 undo, throw new IllegalArgumentError(subst(cMsgTemplate,pcDate)).   
            undo, throw e.       
        end catch.
    end method.
    
    // convert a string date with specified date format to date    
    method static public date DateFromOtherFormat(pcDate as character, pcDateFormat as char):
        define variable cDateFormat as character no-undo.
        define variable dDate as date no-undo.
        if pcDateFormat <> session:date-format then
        do on error undo, throw: 
            cDateFormat = session:date-format.
            session:date-format = pcDateFormat.
            dDate = date(pcDate).
            session:date-format = cDateFormat.
            return dDate.
            finally:
                if session:date-format <> cDateFormat then
                    session:date-format = cDateFormat.
            end.    
        end.
        else 
            return date(pcDate).
    end method.
    
    method public static integer GetTimeInSeconds(pcTime as char):
        define variable cSegments as character extent no-undo.
        define variable cMsgTemplate as character init '"&1" is not valid. Expected formats are "HH:MM", HH:MM:SS" with optional AM or PM' no-undo.
        define variable lPm as logical no-undo.
        define variable lam as logical no-undo.
        define variable iHours   as integer no-undo.
        define variable iMinutes as integer no-undo.
        define variable iSeconds as integer no-undo.
        
        if index(pcTime,"/") > 0 or index(pcTime,"-") > 0 then
            undo, throw new IllegalArgumentError(Subst(cMsgTemplate,pctime)).
            
        if index(pcTime,"AM") > 0 then 
        do:
            pctime = trim(replace(pcTime,"AM","")).
            lam = true.
        end.
        else if index(pcTime,"PM") > 0 then 
        do: 
            pctime = trim(replace(pcTime,"PM","")).
            lpm = true.
        end.       
        
        cSegments = Array:Split(trim(pcTime),":").
        if extent(cSegments) >= 1 then
        do: 
            iHours = integer(cSegments[1]).
            if iHours < 0 then
                undo, throw new IllegalArgumentError('Hours must be positive').
             
            if lam or lpm then 
            do:
                if iHours < 1 or iHours > 12 then 
                  undo, throw  new IllegalArgumentError(subst('hours must be between 1 and 12 for &1',if lam then "AM" else "PM")).
            
                if lpm then iHours  = iHours + 12.    
            end.    
        end.
        if extent(cSegments) >= 2 then
        do: 
            iMinutes = integer(cSegments[2]).
            if iMinutes < 0 then
                undo, throw new IllegalArgumentError('Minutes must be positive').
            
        end.    
        if extent(cSegments) = 3 then
        do: 
            iSeconds = integer(cSegments[3]).
            if iSeconds < 0 then
               undo, throw new IllegalArgumentError('Seconds must be positive').
        end.
       
        return (ihours * 60 * 60) + (iMinutes * 60) + iSeconds.     
        catch e1 as IllegalArgumentError :
            undo, throw e1.
        end.    
        catch e2 as Progress.Lang.Error :
            undo, throw  new IllegalArgumentError(Subst(cMsgTemplate,pctime)).
        end catch. 
    end method.
    
           
end class.