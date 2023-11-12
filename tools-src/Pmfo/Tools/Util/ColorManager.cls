 
 /*------------------------------------------------------------------------
    File        : ColorManager
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Wed Mar 27 07:37:14 EDT 2019
    Notes       : The colors are added to the color-table when they are 
                  used. 
                  NOTE: The color-table is not large enough to hold all 
                  defined colors, so there is a limit to tne total number of
                  colors that can be used in a session  
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using OpenEdge.Core.Assert from propath.

block-level on error undo, throw.

class Pmfo.Tools.Util.ColorManager: 
    define static public property Black         as integer no-undo init 0 get.
    define static public property Navy          as integer no-undo init 1 get.
    define static public property Green         as integer no-undo init 2 get.
    define static public property Teal          as integer no-undo init 3 get.
    define static public property Maroon        as integer no-undo init 4 get.
    define static public property Purple        as integer no-undo init 5 get.
    define static public property Olive         as integer no-undo init 6 get.
    define static public property Gray          as integer no-undo init 7 get.
    define static public property Silver        as integer no-undo init 8 get.
    define static public property Blue          as integer no-undo init 9 get. 
    define static public property Lime          as integer no-undo init 10 get.
    define static public property Aqua          as integer no-undo init 11 get.
    define static public property Red           as integer no-undo init 12 get.
    define static public property Fuchsia       as integer no-undo init 13 get. // same as magenta in US
    define static public property Magenta       as integer no-undo init 13 get. // same as fuchsia in US
    define static public property Yellow        as integer no-undo init 14 get. 
    define static public property White         as integer no-undo init 15 get. 
    
    
    {Pmfo/Tools/Util/colorproperty.i BlueViolet     138  43 226}
    {Pmfo/Tools/Util/colorproperty.i Coral          255 127  80}
    {Pmfo/Tools/Util/colorproperty.i Crimson        220  20  60}
    {Pmfo/Tools/Util/colorproperty.i DarkRed        139   0   0}
    {Pmfo/Tools/Util/colorproperty.i DodgerBlue      30 144 255}
    {Pmfo/Tools/Util/colorproperty.i FireBrick      178  34  34}
    {Pmfo/Tools/Util/colorproperty.i Gold           255 215   0}
    {Pmfo/Tools/Util/colorproperty.i Khaki          240 230 140}
    {Pmfo/Tools/Util/colorproperty.i LavenderBlush  255 240 245}
    {Pmfo/Tools/Util/colorproperty.i LightBlue      173 216 230}
    {Pmfo/Tools/Util/colorproperty.i LightGoldenrodYellow 250 250 210}
    {Pmfo/Tools/Util/colorproperty.i LightGrey      211 211 211}
    {Pmfo/Tools/Util/colorproperty.i LightGreen     144 238 144}
    {Pmfo/Tools/Util/colorproperty.i LightPink      255 182 193}
    {Pmfo/Tools/Util/colorproperty.i LightSalmon    255 112 147}
    {Pmfo/Tools/Util/colorproperty.i LightSkyBlue   135 206 250}
    {Pmfo/Tools/Util/colorproperty.i LightYellow    255 255 224}
    {Pmfo/Tools/Util/colorproperty.i LimeGreen      50  205  50}
    {Pmfo/Tools/Util/colorproperty.i MediumSeaGreen 60  179 113}
    {Pmfo/Tools/Util/colorproperty.i OldLace        253 245 230}
    {Pmfo/Tools/Util/colorproperty.i Orange         255 165   0}
    {Pmfo/Tools/Util/colorproperty.i OrangeRed      255  69   0}
    {Pmfo/Tools/Util/colorproperty.i PaleVioletRed  219 112 147}
    {Pmfo/Tools/Util/colorproperty.i PaleGreen      152 251 152}
    {Pmfo/Tools/Util/colorproperty.i PowderBlue     176 224 230}
    {Pmfo/Tools/Util/colorproperty.i Tomato         255  99  71}
    {Pmfo/Tools/Util/colorproperty.i WhiteSmoke     245 245 245}
    {Pmfo/Tools/Util/colorproperty.i YellowGreen    154 205  50}
         
    constructor public ColorManager():
        super().
    end constructor.    
     
    method static private integer AddColor(red as int, green as int, blue as int ):
        Assert:IsTrue(red >= 0 and red <= 255,"Red >= 0 and Red <= 255").
        Assert:IsTrue(green >= 0 and green <= 255,"Green >= 0 and Green <= 255").
        Assert:IsTrue(blue >= 0 and blue <= 255,"Blue >= 0 and Blue <= 255").

        define variable i as integer no-undo.
        i = color-table:num-entries.
        color-table:num-entries = i + 1.
        color-table:set-dynamic(i, true).
        color-table:set-red-value(i, red).
        color-table:set-green-value(i, green).
        color-table:set-blue-value(i, blue).
        return i.
    end method.


end class.