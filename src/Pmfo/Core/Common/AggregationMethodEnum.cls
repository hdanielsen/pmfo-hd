
/*------------------------------------------------------------------------
    File        : AggregationMethod
    Purpose     : for odata aggregate 
    Syntax      : url query example that uses Sum of property pieces serialized in response as totalPieees:
                     ?apply=aggregate(pieces with sum as totalPieees)
    Description : 
    Author(s)   : hdaniels
    Created     : Sun Dec 11 13:25:14 EST 2022
    Notes       : 
  ----------------------------------------------------------------------*/

enum Pmfo.Core.Common.AggregationMethodEnum :  

    define enum Max 
                Min 
                Sum 
                Average 
                CountDistinct .

end enum.