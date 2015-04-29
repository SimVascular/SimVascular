#===========================================================================
#    
# Copyright (c) 2009-2011 Open Source Medical Software Corporation,
#                         University of California, San Diego.
#
# All rights reserved.
#
# Portions of the code Copyright (c) 1998-2007 Stanford University,
# Charles Taylor, Nathan Wilson, Ken Wang.
#
# See SimVascular Acknowledgements file for additional
# contributors to the source code. 
#
# Permission is hereby granted, free of charge, to any person obtaining
# a copy of this software and associated documentation files (the
# "Software"), to deal in the Software without restriction, including 
# without limitation the rights to use, copy, modify, merge, publish, 
# distribute, sublicense, and/or sell copies of the Software, and to
# permit persons to whom the Software is furnished to do so, subject
# to the following conditions:
# 
# The above copyright notice and this permission notice shall be included 
# in all copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
# OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
# IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
# CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
# TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
# SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
#
#===========================================================================    

# @author Nathan Wilson
# @c Code to return RGB values for unix color names.

set gColorList {
       {199 21 133 "medium violet red"} \
       {176 196 222 "light steel blue"} \
       {102 139 139 "paleturquoise4"} \
       {159 121 238 "mediumpurple2"} \
       {141 182 205 "lightskyblue3"} \
       {0 238 118 "springgreen2"} \
       {255 160 122 "light salmon"} \
       {154 205 50 "yellowgreen"} \
       {178 58 238 "darkorchid2"} \
       {69 139 116 "aquamarine4"} \
       {71 60 139 "slateblue4"} \
       {131 111 255 "slateblue1"} \
       {192 255 62 "olivedrab1"} \
       {139 105 20 "goldenrod4"} \
       {205 155 29 "goldenrod3"} \
       {142 229 238 "cadetblue2"} \
       {255 211 155 "burlywood1"} \
       {112 128 144 "slategrey"} \
       {255 228 225 "mistyrose"} \
       {50 205 50 "limegreen"} \
       {224 255 255 "lightcyan"} \
       {218 165 32 "goldenrod"} \
       {220 220 220 "gainsboro"} \
       {135 206 255 "skyblue1"} \
       {240 255 240 "honeydew"} \
       {238 238 0 "yellow2"} \
       {205 79 57 "tomato3"} \
       {135 206 235 "skyblue"} \
       {85 26 139 "purple4"} \
       {205 133 0 "orange3"} \
       {205 183 158 "bisque3"} \
       {238 213 183 "bisque2"} \
       {87 87 87 "grey34"} \
       {252 252 252 "gray99"} \
       {161 161 161 "gray63"} \
       {112 112 112 "gray44"} \
       {94 94 94 "gray37"} \
       {84 84 84 "gray33"} \
       {66 66 66 "gray26"} \
       {240 255 255 "azure1"} \
       {139 137 137 "snow4"} \
       {205 133 63 "peru"} \
       {219 112 147 "pale violet red"} \
       {139 129 76 "lightgoldenrod4"} \
       {60 179 113 "mediumseagreen"} \
       {255 240 245 "lavender blush"} \
       {209 95 238 "mediumorchid2"} \
       {176 226 255 "lightskyblue1"} \
       {72 61 139 "darkslateblue"} \
       {25 25 112 "midnightblue"} \
       {255 160 122 "lightsalmon1"} \
       {255 250 205 "lemonchiffon"} \
       {173 255 47 "green yellow"} \
       {255 160 122 "lightsalmon"} \
       {240 128 128 "light coral"} \
       {24 116 205 "dodgerblue3"} \
       {139 69 0 "darkorange4"} \
       {106 90 205 "slate blue"} \
       {39 64 139 "royalblue4"} \
       {255 69 0 "orange red"} \
       {50 205 50 "lime green"} \
       {224 255 255 "light cyan"} \
       {148 0 211 "darkviolet"} \
       {233 150 122 "darksalmon"} \
       {255 140 0 "darkorange"} \
       {95 158 160 "cadet blue"} \
       {255 20 147 "deep pink"} \
       {238 0 238 "magenta2"} \
       {139 71 38 "sienna4"} \
       {238 230 133 "khaki2"} \
       {191 191 191 "grey75"} \
       {189 189 189 "grey74"} \
       {186 186 186 "grey73"} \
       {176 176 176 "grey69"} \
       {173 173 173 "grey68"} \
       {89 89 89 "grey35"} \
       {33 33 33 "grey13"} \
       {229 229 229 "gray90"} \
       {207 207 207 "gray81"} \
       {140 140 140 "gray55"} \
       {130 130 130 "gray51"} \
       {79 79 79 "gray31"} \
       {238 233 233 "snow2"} \
       {205 145 158 "pink3"} \
       {18 18 18 "grey7"} \
       {3 3 3 "gray1"} \
       {139 0 0 "red4"} \
       {205 0 0 "red3"} \
       {210 180 140 "tan"} \
       {255 0 0 "red"} \
       {199 21 133 "mediumvioletred"} \
       {119 136 153 "lightslategrey"} \
       {135 206 250 "lightskyblue"} \
       {139 87 66 "lightsalmon4"} \
       {34 139 34 "forestgreen"} \
       {16 78 139 "dodgerblue4"} \
       {153 50 204 "dark orchid"} \
       {188 143 143 "rosy brown"} \
       {205 175 149 "peachpuff3"} \
       {124 205 124 "palegreen3"} \
       {238 64 0 "orangered2"} \
       {122 139 139 "lightcyan4"} \
       {139 58 58 "indianred4"} \
       {205 85 85 "indianred3"} \
       {0 0 128 "navyblue"} \
       {105 105 105 "dim grey"} \
       {255 20 147 "deeppink"} \
       {139 76 57 "salmon4"} \
       {205 112 84 "salmon3"} \
       {196 196 196 "grey77"} \
       {145 145 145 "gray57"} \
       {28 28 28 "gray11"} \
       {205 150 205 "plum3"} \
       {23 23 23 "gray9"} \
       {20 20 20 "gray8"} \
       {0 0 139 "blue4"} \
       {245 245 220 "beige"} \
       {250 250 210 "light goldenrod yellow"} \
       {139 131 134 "lavenderblush4"} \
       {0 206 209 "dark turquoise"} \
       {0 206 209 "darkturquoise"} \
       {47 79 79 "darkslategrey"} \
       {205 129 98 "lightsalmon3"} \
       {139 105 105 "rosybrown4"} \
       {255 228 225 "misty rose"} \
       {78 238 148 "seagreen2"} \
       {205 92 92 "indianred"} \
       {255 20 147 "deeppink1"} \
       {0 0 139 "dark blue"} \
       {230 230 250 "lavender"} \
       {253 245 230 "oldlace"} \
       {199 199 199 "grey78"} \
       {138 138 138 "grey54"} \
       {115 115 115 "grey45"} \
       {54 54 54 "grey21"} \
       {247 247 247 "gray97"} \
       {245 245 245 "gray96"} \
       {242 242 242 "gray95"} \
       {224 224 224 "gray88"} \
       {222 222 222 "gray87"} \
       {219 219 219 "gray86"} \
       {179 179 179 "gray70"} \
       {97 97 97 "gray38"} \
       {31 31 31 "gray12"} \
       {250 240 230 "linen"} \
       {72 209 204 "medium turquoise"} \
       {72 61 139 "dark slate blue"} \
       {139 137 112 "lemonchiffon4"} \
       {193 255 193 "darkseagreen1"} \
       {205 192 176 "antiquewhite3"} \
       {186 85 211 "mediumorchid"} \
       {0 255 127 "springgreen"} \
       {0 134 139 "turquoise4"} \
       {79 148 205 "steelblue3"} \
       {238 213 210 "mistyrose2"} \
       {209 238 238 "lightcyan2"} \
       {205 92 92 "indian red"} \
       {238 44 44 "firebrick2"} \
       {65 105 225 "royalblue"} \
       {95 158 160 "cadetblue"} \
       {108 166 205 "skyblue3"} \
       {205 205 0 "yellow3"} \
       {255 140 105 "salmon1"} \
       {139 90 0 "orange4"} \
       {255 105 180 "hotpink"} \
       {229 229 229 "grey90"} \
       {143 143 143 "gray56"} \
       {99 99 99 "gray39"} \
       {46 46 46 "gray18"} \
       {36 36 36 "gray14"} \
       {139 102 139 "plum4"} \
       {15 15 15 "grey6"} \
       {15 15 15 "gray6"} \
       {205 173 0 "gold3"} \
       {255 215 0 "gold1"} \
       {0 0 238 "blue2"} \
       {238 154 73 "tan2"} \
       {0 255 255 "cyan"} \
       {0 250 154 "mediumspringgreen"} \
       {188 238 104 "darkolivegreen2"} \
       {238 232 170 "pale goldenrod"} \
       {176 196 222 "lightsteelblue"} \
       {244 164 96 "sandy brown"} \
       {255 239 213 "papaya whip"} \
       {102 205 0 "chartreuse3"} \
       {139 34 82 "violetred4"} \
       {67 110 238 "royalblue2"} \
       {72 118 255 "royalblue1"} \
       {255 239 213 "papayawhip"} \
       {205 183 181 "mistyrose3"} \
       {224 255 255 "lightcyan1"} \
       {127 255 212 "aquamarine"} \
       {74 112 139 "skyblue4"} \
       {139 58 98 "hotpink4"} \
       {205 96 144 "hotpink3"} \
       {238 106 167 "hotpink2"} \
       {169 169 169 "darkgrey"} \
       {105 105 105 "dimgray"} \
       {255 99 71 "tomato"} \
       {168 168 168 "grey66"} \
       {166 166 166 "grey65"} \
       {163 163 163 "grey64"} \
       {84 84 84 "grey33"} \
       {69 69 69 "grey27"} \
       {194 194 194 "gray76"} \
       {176 176 176 "gray69"} \
       {173 173 173 "gray68"} \
       {0 0 0 "grey0"} \
       {240 255 255 "azure"} \
       {0 250 154 "medium spring green"} \
       {139 101 8 "darkgoldenrod4"} \
       {205 149 12 "darkgoldenrod3"} \
       {238 173 14 "darkgoldenrod2"} \
       {184 134 11 "darkgoldenrod"} \
       {139 69 19 "saddle brown"} \
       {238 149 114 "lightsalmon2"} \
       {0 104 139 "deepskyblue4"} \
       {0 154 205 "deepskyblue3"} \
       {0 178 238 "deepskyblue2"} \
       {0 191 255 "deepskyblue"} \
       {255 127 0 "darkorange1"} \
       {205 50 120 "violetred3"} \
       {238 58 140 "violetred2"} \
       {255 62 150 "violetred1"} \
       {105 89 205 "slateblue3"} \
       {122 103 238 "slateblue2"} \
       {107 142 35 "olive drab"} \
       {255 106 106 "indianred1"} \
       {255 48 48 "firebrick1"} \
       {83 134 139 "cadetblue4"} \
       {208 32 144 "violetred"} \
       {188 143 143 "rosybrown"} \
       {0 0 128 "navy blue"} \
       {178 34 34 "firebrick"} \
       {139 0 0 "dark red"} \
       {255 255 255 "grey100"} \
       {139 126 102 "wheat4"} \
       {201 201 201 "grey79"} \
       {194 194 194 "grey76"} \
       {156 156 156 "grey61"} \
       {237 237 237 "gray93"} \
       {214 214 214 "gray84"} \
       {166 166 166 "gray65"} \
       {92 92 92 "gray36"} \
       {82 82 82 "gray32"} \
       {33 33 33 "gray13"} \
       {26 26 26 "gray10"} \
       {193 205 205 "azure3"} \
       {255 250 250 "snow1"} \
       {255 165 79 "tan1"} \
       {119 136 153 "light slate gray"} \
       {202 255 112 "darkolivegreen1"} \
       {100 149 237 "cornflower blue"} \
       {255 235 205 "blanched almond"} \
       {205 193 197 "lavenderblush3"} \
       {238 224 229 "lavenderblush2"} \
       {255 240 245 "lavenderblush1"} \
       {85 107 47 "darkolivegreen"} \
       {255 240 245 "lavenderblush"} \
       {118 238 198 "aquamarine2"} \
       {208 32 144 "violet red"} \
       {179 238 58 "olivedrab2"} \
       {139 125 123 "mistyrose4"} \
       {255 228 225 "mistyrose1"} \
       {180 205 205 "lightcyan3"} \
       {240 128 128 "lightcoral"} \
       {127 255 0 "chartreuse"} \
       {255 218 185 "peachpuff"} \
       {152 251 152 "palegreen"} \
       {245 255 250 "mintcream"} \
       {126 192 238 "skyblue2"} \
       {255 228 181 "moccasin"} \
       {255 99 71 "tomato1"} \
       {205 105 201 "orchid3"} \
       {205 41 144 "maroon3"} \
       {250 128 114 "salmon"} \
       {207 207 207 "grey81"} \
       {158 158 158 "grey62"} \
       {99 99 99 "grey39"} \
       {97 97 97 "grey38"} \
       {94 94 94 "grey37"} \
       {235 235 235 "gray92"} \
       {212 212 212 "gray83"} \
       {168 168 168 "gray66"} \
       {138 138 138 "gray54"} \
       {127 127 127 "gray50"} \
       {77 77 77 "gray30"} \
       {48 48 48 "gray19"} \
       {38 38 38 "gray15"} \
       {131 139 139 "azure4"} \
       {8 8 8 "grey3"} \
       {205 133 63 "tan3"} \
       {255 192 203 "pink"} \
       {190 190 190 "gray"} \
       {0 0 255 "blue"} \
       {188 210 238 "lightsteelblue2"} \
       {202 225 255 "lightsteelblue1"} \
       {32 178 170 "light sea green"} \
       {119 136 153 "lightslategray"} \
       {238 233 191 "lemonchiffon2"} \
       {0 255 127 "springgreen1"} \
       {173 255 47 "greenyellow"} \
       {118 238 0 "chartreuse2"} \
       {112 128 144 "slate grey"} \
       {58 95 205 "royalblue3"} \
       {176 224 230 "powderblue"} \
       {238 203 173 "peachpuff2"} \
       {144 238 144 "palegreen2"} \
       {245 255 250 "mint cream"} \
       {106 90 205 "slateblue"} \
       {238 229 222 "seashell2"} \
       {238 18 137 "deeppink2"} \
       {189 183 107 "darkkhaki"} \
       {139 28 98 "maroon4"} \
       {160 82 45 "sienna"} \
       {181 181 181 "grey71"} \
       {171 171 171 "grey67"} \
       {46 46 46 "grey18"} \
       {150 150 150 "gray59"} \
       {110 110 110 "gray43"} \
       {64 64 64 "gray25"} \
       {255 228 196 "bisque"} \
       {255 0 0 "red1"} \
       {123 104 238 "mediumslateblue"} \
       {255 236 139 "lightgoldenrod1"} \
       {238 221 130 "light goldenrod"} \
       {150 205 205 "paleturquoise3"} \
       {96 123 139 "lightskyblue4"} \
       {0 255 127 "spring green"} \
       {255 255 224 "light yellow"} \
       {245 245 245 "white smoke"} \
       {0 0 205 "medium blue"} \
       {248 248 255 "ghost white"} \
       {54 100 139 "steelblue4"} \
       {205 155 155 "rosybrown3"} \
       {255 218 185 "peachpuff1"} \
       {154 255 154 "palegreen1"} \
       {138 43 226 "blueviolet"} \
       {139 134 130 "seashell4"} \
       {169 169 169 "darkgray"} \
       {205 104 57 "sienna3"} \
       {102 102 102 "grey40"} \
       {232 232 232 "gray91"} \
       {209 209 209 "gray82"} \
       {13 13 13 "gray5"} \
       {0 238 238 "cyan2"} \
       {0 255 255 "cyan1"} \
       {0 0 255 "blue1"} \
       {255 250 250 "snow"} \
       {238 220 130 "lightgoldenrod2"} \
       {132 112 255 "lightslateblue"} \
       {180 82 205 "mediumorchid3"} \
       {105 139 105 "darkseagreen4"} \
       {0 205 102 "springgreen3"} \
       {34 139 34 "forest green"} \
       {108 123 139 "slategray4"} \
       {159 182 205 "slategray3"} \
       {185 211 238 "slategray2"} \
       {65 105 225 "royal blue"} \
       {139 119 101 "peachpuff4"} \
       {84 139 84 "palegreen4"} \
       {152 251 152 "pale green"} \
       {205 55 0 "orangered3"} \
       {255 193 37 "goldenrod1"} \
       {248 248 255 "ghostwhite"} \
       {139 26 26 "firebrick4"} \
       {205 38 38 "firebrick3"} \
       {122 197 205 "cadetblue3"} \
       {112 128 144 "slategray"} \
       {205 197 191 "seashell3"} \
       {193 205 193 "honeydew3"} \
       {139 136 120 "cornsilk4"} \
       {238 232 205 "cornsilk2"} \
       {155 48 255 "purple1"} \
       {105 105 105 "dimgrey"} \
       {139 0 0 "darkred"} \
       {255 246 143 "khaki1"} \
       {205 205 193 "ivory3"} \
       {179 179 179 "grey70"} \
       {153 153 153 "grey60"} \
       {82 82 82 "grey32"} \
       {56 56 56 "grey22"} \
       {31 31 31 "grey12"} \
       {250 250 250 "gray98"} \
       {227 227 227 "gray89"} \
       {181 181 181 "gray71"} \
       {163 163 163 "gray64"} \
       {153 153 153 "gray60"} \
       {125 125 125 "gray49"} \
       {224 238 238 "azure2"} \
       {8 8 8 "gray3"} \
       {187 255 255 "paleturquoise1"} \
       {171 130 255 "mediumpurple1"} \
       {147 112 219 "medium purple"} \
       {255 250 205 "lemonchiffon1"} \
       {0 191 255 "deep sky blue"} \
       {205 179 139 "navajowhite3"} \
       {191 62 255 "darkorchid1"} \
       {255 140 0 "dark orange"} \
       {238 180 34 "goldenrod2"} \
       {189 183 107 "dark khaki"} \
       {238 118 33 "chocolate2"} \
       {238 197 145 "burlywood2"} \
       {240 255 240 "honeydew1"} \
       {0 100 0 "darkgreen"} \
       {205 181 205 "thistle3"} \
       {238 210 238 "thistle2"} \
       {255 225 255 "thistle1"} \
       {0 0 139 "darkblue"} \
       {216 191 216 "thistle"} \
       {238 48 167 "maroon2"} \
       {255 52 179 "maroon1"} \
       {135 135 135 "grey53"} \
       {112 112 112 "grey44"} \
       {64 64 64 "grey25"} \
       {189 189 189 "gray74"} \
       {115 115 115 "gray45"} \
       {105 105 105 "gray41"} \
       {89 89 89 "gray35"} \
       {69 69 69 "gray27"} \
       {59 59 59 "gray23"} \
       {41 41 41 "gray16"} \
       {139 35 35 "brown4"} \
       {245 222 179 "wheat"} \
       {255 127 80 "coral"} \
       {139 90 43 "tan4"} \
       {250 250 210 "lightgoldenrodyellow"} \
       {132 112 255 "light slate blue"} \
       {85 107 47 "dark olive green"} \
       {47 79 79 "dark slate gray"} \
       {205 104 137 "palevioletred3"} \
       {93 71 139 "mediumpurple4"} \
       {137 104 205 "mediumpurple3"} \
       {139 69 19 "saddlebrown"} \
       {176 224 230 "powder blue"} \
       {104 34 139 "darkorchid4"} \
       {154 50 205 "darkorchid3"} \
       {255 218 185 "peach puff"} \
       {105 139 34 "olivedrab4"} \
       {104 131 139 "lightblue4"} \
       {255 182 193 "lightpink"} \
       {211 211 211 "lightgray"} \
       {224 238 224 "honeydew2"} \
       {255 248 220 "cornsilk1"} \
       {253 245 230 "old lace"} \
       {255 130 71 "sienna1"} \
       {139 125 107 "bisque4"} \
       {218 112 214 "orchid"} \
       {205 198 115 "khaki3"} \
       {214 214 214 "grey84"} \
       {212 212 212 "grey83"} \
       {209 209 209 "grey82"} \
       {184 184 184 "grey72"} \
       {133 133 133 "grey52"} \
       {110 110 110 "grey43"} \
       {66 66 66 "grey26"} \
       {36 36 36 "grey14"} \
       {26 26 26 "grey10"} \
       {191 191 191 "gray75"} \
       {135 135 135 "gray53"} \
       {54 54 54 "gray21"} \
       {51 51 51 "gray20"} \
       {205 51 51 "brown3"} \
       {20 20 20 "grey8"} \
       {238 0 0 "red2"} \
       {0 0 128 "navy"} \
       {190 190 190 "grey"} \
       {255 215 0 "gold"} \
       {102 205 170 "mediumaquamarine"} \
       {238 221 130 "lightgoldenrod"} \
       {82 139 139 "darkslategray4"} \
       {155 205 155 "darkseagreen3"} \
       {180 238 180 "darkseagreen2"} \
       {139 131 120 "antiquewhite4"} \
       {250 235 215 "antique white"} \
       {0 139 69 "springgreen4"} \
       {139 139 122 "lightyellow4"} \
       {255 250 240 "floral white"} \
       {127 255 212 "aquamarine1"} \
       {0 197 205 "turquoise3"} \
       {92 172 238 "steelblue2"} \
       {238 180 180 "rosybrown2"} \
       {255 182 193 "light pink"} \
       {211 211 211 "light gray"} \
       {238 99 99 "indianred2"} \
       {30 144 255 "dodgerblue"} \
       {0 100 0 "dark green"} \
       {84 255 159 "seagreen1"} \
       {139 10 80 "deeppink4"} \
       {240 248 255 "aliceblue"} \
       {255 0 255 "magenta1"} \
       {255 105 180 "hot pink"} \
       {238 121 66 "sienna2"} \
       {255 131 250 "orchid1"} \
       {255 255 255 "gray100"} \
       {247 247 247 "grey97"} \
       {240 240 240 "grey94"} \
       {222 222 222 "grey87"} \
       {219 219 219 "grey86"} \
       {130 130 130 "grey51"} \
       {107 107 107 "grey42"} \
       {48 48 48 "grey19"} \
       {240 240 240 "gray94"} \
       {217 217 217 "gray85"} \
       {156 156 156 "gray61"} \
       {238 59 59 "brown2"} \
       {240 230 140 "khaki"} \
       {3 3 3 "grey1"} \
       {139 117 0 "gold4"} \
       {123 104 238 "medium slate blue"} \
       {60 179 113 "medium sea green"} \
       {47 79 79 "dark slate grey"} \
       {175 238 238 "pale turquoise"} \
       {175 238 238 "paleturquoise"} \
       {122 55 139 "mediumorchid4"} \
       {238 223 204 "antiquewhite2"} \
       {238 238 209 "lightyellow2"} \
       {144 238 144 "light green"} \
       {148 0 211 "dark violet"} \
       {233 150 122 "dark salmon"} \
       {127 255 0 "chartreuse1"} \
       {0 245 255 "turquoise1"} \
       {244 164 96 "sandybrown"} \
       {255 69 0 "orangered1"} \
       {255 174 185 "lightpink1"} \
       {178 223 238 "lightblue2"} \
       {191 239 255 "lightblue1"} \
       {211 211 211 "light grey"} \
       {46 139 87 "seagreen4"} \
       {67 205 128 "seagreen3"} \
       {173 216 230 "lightblue"} \
       {205 16 118 "deeppink3"} \
       {169 169 169 "dark grey"} \
       {0 139 139 "dark cyan"} \
       {222 184 135 "burlywood"} \
       {255 245 238 "seashell"} \
       {255 110 180 "hotpink1"} \
       {105 105 105 "dim gray"} \
       {0 139 139 "darkcyan"} \
       {139 139 0 "yellow4"} \
       {255 255 0 "yellow"} \
       {160 32 240 "purple"} \
       {255 165 0 "orange"} \
       {139 139 131 "ivory4"} \
       {252 252 252 "grey99"} \
       {227 227 227 "grey89"} \
       {161 161 161 "grey63"} \
       {148 148 148 "grey58"} \
       {125 125 125 "grey49"} \
       {79 79 79 "grey31"} \
       {61 61 61 "grey24"} \
       {51 51 51 "grey20"} \
       {0 139 0 "green4"} \
       {0 255 0 "green1"} \
       {186 186 186 "gray73"} \
       {171 171 171 "gray67"} \
       {205 91 69 "coral3"} \
       {238 106 80 "coral2"} \
       {238 174 238 "plum2"} \
       {139 99 108 "pink4"} \
       {255 255 240 "ivory"} \
       {10 10 10 "gray4"} \
       {5 5 5 "gray2"} \
       {238 201 0 "gold2"} \
       {102 205 170 "medium aquamarine"} \
       {119 136 153 "light slate grey"} \
       {205 190 112 "lightgoldenrod3"} \
       {162 205 90 "darkolivegreen3"} \
       {255 185 15 "darkgoldenrod1"} \
       {184 134 11 "dark goldenrod"} \
       {186 85 211 "medium orchid"} \
       {255 250 205 "lemon chiffon"} \
       {139 121 94 "navajowhite4"} \
       {0 191 255 "deepskyblue1"} \
       {255 255 224 "lightyellow"} \
       {255 250 240 "floralwhite"} \
       {30 144 255 "dodger blue"} \
       {0 0 205 "mediumblue"} \
       {144 238 144 "lightgreen"} \
       {139 69 19 "chocolate4"} \
       {205 102 29 "chocolate3"} \
       {139 115 85 "burlywood4"} \
       {64 224 208 "turquoise"} \
       {70 130 180 "steelblue"} \
       {46 139 87 "sea green"} \
       {124 252 0 "lawngreen"} \
       {131 139 131 "honeydew4"} \
       {169 169 169 "dark gray"} \
       {46 139 87 "seagreen"} \
       {139 71 137 "orchid4"} \
       {255 231 186 "wheat1"} \
       {238 130 238 "violet"} \
       {255 255 240 "ivory1"} \
       {224 224 224 "grey88"} \
       {217 217 217 "grey85"} \
       {145 145 145 "grey57"} \
       {143 143 143 "grey56"} \
       {140 140 140 "grey55"} \
       {122 122 122 "grey48"} \
       {120 120 120 "grey47"} \
       {117 117 117 "grey46"} \
       {77 77 77 "grey30"} \
       {43 43 43 "grey17"} \
       {120 120 120 "gray47"} \
       {74 74 74 "gray29"} \
       {238 169 184 "pink2"} \
       {13 13 13 "grey5"} \
       {10 10 10 "grey4"} \
       {0 255 0 "green"} \
       {0 0 0 "gray0"} \
       {165 42 42 "brown"} \
       {110 123 139 "lightsteelblue4"} \
       {110 139 61 "darkolivegreen4"} \
       {139 71 93 "palevioletred4"} \
       {135 206 250 "light sky blue"} \
       {121 205 205 "darkslategray3"} \
       {141 238 238 "darkslategray2"} \
       {151 255 255 "darkslategray1"} \
       {255 235 205 "blanchedalmond"} \
       {238 232 170 "palegoldenrod"} \
       {25 25 112 "midnight blue"} \
       {32 178 170 "lightseagreen"} \
       {205 201 165 "lemonchiffon3"} \
       {47 79 79 "darkslategray"} \
       {154 205 50 "yellow green"} \
       {143 188 143 "darkseagreen"} \
       {250 235 215 "antiquewhite"} \
       {238 118 0 "darkorange2"} \
       {69 139 0 "chartreuse4"} \
       {70 130 180 "steel blue"} \
       {255 193 193 "rosybrown1"} \
       {154 205 50 "olivedrab3"} \
       {238 162 173 "lightpink2"} \
       {255 69 0 "orangered"} \
       {139 123 139 "thistle4"} \
       {135 206 235 "sky blue"} \
       {255 248 220 "cornsilk"} \
       {238 130 98 "salmon2"} \
       {238 122 233 "orchid2"} \
       {238 238 224 "ivory2"} \
       {237 237 237 "grey93"} \
       {235 235 235 "grey92"} \
       {232 232 232 "grey91"} \
       {92 92 92 "grey36"} \
       {74 74 74 "grey29"} \
       {71 71 71 "grey28"} \
       {41 41 41 "grey16"} \
       {201 201 201 "gray79"} \
       {199 199 199 "gray78"} \
       {196 196 196 "gray77"} \
       {122 122 122 "gray48"} \
       {43 43 43 "gray17"} \
       {139 62 47 "coral4"} \
       {255 114 86 "coral1"} \
       {255 187 255 "plum1"} \
       {255 181 197 "pink1"} \
       {23 23 23 "grey9"} \
       {5 5 5 "grey2"} \
       {18 18 18 "gray7"} \
       {0 139 139 "cyan4"} \
       {0 0 205 "blue3"} \
       {221 160 221 "plum"} \
       {100 149 237 "cornflowerblue"} \
       {164 211 238 "lightskyblue2"} \
       {255 239 219 "antiquewhite1"} \
       {238 207 161 "navajowhite2"} \
       {255 222 173 "navajowhite1"} \
       {205 205 180 "lightyellow3"} \
       {139 0 139 "dark magenta"} \
       {255 222 173 "navajowhite"} \
       {205 102 0 "darkorange3"} \
       {245 245 245 "whitesmoke"} \
       {0 229 238 "turquoise2"} \
       {99 184 255 "steelblue1"} \
       {139 95 101 "lightpink4"} \
       {154 192 205 "lightblue3"} \
       {124 252 0 "lawn green"} \
       {255 127 36 "chocolate1"} \
       {240 248 255 "alice blue"} \
       {107 142 35 "olivedrab"} \
       {211 211 211 "lightgrey"} \
       {210 105 30 "chocolate"} \
       {139 0 139 "magenta4"} \
       {205 0 205 "magenta3"} \
       {255 255 0 "yellow1"} \
       {125 38 205 "purple3"} \
       {145 44 238 "purple2"} \
       {238 154 0 "orange2"} \
       {255 165 0 "orange1"} \
       {255 0 255 "magenta"} \
       {255 228 196 "bisque1"} \
       {238 216 174 "wheat2"} \
       {176 48 96 "maroon"} \
       {139 134 78 "khaki4"} \
       {245 245 245 "grey96"} \
       {242 242 242 "grey95"} \
       {204 204 204 "grey80"} \
       {127 127 127 "grey50"} \
       {105 105 105 "grey41"} \
       {38 38 38 "grey15"} \
       {28 28 28 "grey11"} \
       {204 204 204 "gray80"} \
       {148 148 148 "gray58"} \
       {102 102 102 "gray40"} \
       {87 87 87 "gray34"} \
       {56 56 56 "gray22"} \
       {255 64 64 "brown1"} \
       {205 201 201 "snow3"} \
       {72 209 204 "mediumturquoise"} \
       {162 181 205 "lightsteelblue3"} \
       {238 121 159 "palevioletred2"} \
       {255 130 171 "palevioletred1"} \
       {174 238 238 "paleturquoise2"} \
       {143 188 143 "dark sea green"} \
       {219 112 147 "palevioletred"} \
       {224 102 255 "mediumorchid1"} \
       {255 222 173 "navajo white"} \
       {147 112 219 "mediumpurple"} \
       {255 255 224 "lightyellow1"} \
       {28 134 238 "dodgerblue2"} \
       {30 144 255 "dodgerblue1"} \
       {139 0 139 "darkmagenta"} \
       {138 43 226 "blue violet"} \
       {102 205 170 "aquamarine3"} \
       {198 226 255 "slategray1"} \
       {112 128 144 "slate gray"} \
       {139 37 0 "orangered4"} \
       {205 140 149 "lightpink3"} \
       {173 216 230 "light blue"} \
       {153 50 204 "darkorchid"} \
       {152 245 255 "cadetblue1"} \
       {205 170 125 "burlywood3"} \
       {255 245 238 "seashell1"} \
       {205 200 177 "cornsilk3"} \
       {139 54 38 "tomato4"} \
       {238 92 66 "tomato2"} \
       {205 186 150 "wheat3"} \
       {250 250 250 "grey98"} \
       {150 150 150 "grey59"} \
       {59 59 59 "grey23"} \
       {0 205 0 "green3"} \
       {0 238 0 "green2"} \
       {184 184 184 "gray72"} \
       {158 158 158 "gray62"} \
       {133 133 133 "gray52"} \
       {117 117 117 "gray46"} \
       {107 107 107 "gray42"} \
       {71 71 71 "gray28"} \
       {61 61 61 "gray24"} \
       {255 255 255 "white"} \
       {0 205 205 "cyan3"} \
       {0 0 0 "black"}}

# ----------------
# gdscGetUnixColor
# ----------------

proc gdscGetUnixColor {name} {

  # @author Nathan Wilson
  # @c Returns the RGB values given a UNIX color name
  # @a name:  UNIX color name
  # @r RGB vector with range of each component from 0 - 1
  global gColorList
  set i 0

  for {set i 0} {$i < 752} {incr i} {
    if {[lindex [lindex $gColorList $i] 3] == "$name"} {
       set rgb(0) [expr double([lindex [lindex $gColorList $i ] 0])/255.0]
       set rgb(1) [expr double([lindex [lindex $gColorList $i ] 1])/255.0]
       set rgb(2) [expr double([lindex [lindex $gColorList $i ] 2])/255.0]
       break
    }
  }

  # use default color if name not found
   if {$i == 752} {
   set rgb(0) 0.5
   set rgb(1) 0.5
   set rgb(2) 0.5
   }
  #puts "$rgb(0) $rgb(1) $rgb(2)"
   return "$rgb(0) $rgb(1) $rgb(2)"
}

