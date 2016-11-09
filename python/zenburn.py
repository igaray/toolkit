#!/usr/bin/python
from collections import OrderedDict

f = open('zenburn.html', 'w')

begin1 = """
<html>
<head>
<title>Zenburn Theme Colors</title></head>
<body bgcolor="000000">
<font color = "ffffff"><h1>Zenburn Colors by Hex Value</h1></font>
<table width = "100%">
<tr height = 20><td width = 20 bgcolor = "000000">&nbsp;</td><td><font color = "ffffff">Hex</font></td><td><font color = "ffffff">Freq</font></td><td><font color = "ffffff">Description</font></td></tr>
"""

end1 = """
</table>
"""

begin2 = """
<font color = "ffffff"><h1>Zenburn Colors by Frequency</h1></font>
<table width = "100%">
<tr height = 20><td width = 20 bgcolor = "000000">&nbsp;</td><td><font color = "ffffff">Hex</font></td><td><font color = "ffffff">Freq</font></td><td><font color = "ffffff">Description</font></td></tr>
"""

end2 = """
</table>
</body>
</html>
"""

data = [
    ["000000", "menu_border_color"],
    ["000d18", "Cursor"],
    ["121212", "CursorLine"],
    ["1E2320", "bg_focus"],
    ["1E2320", "urxvt*color0"],
    ["1E2320", "xterm*color0"],
    ["242424", "Pmenu"],
    ["2c302d", "WildMenu"],
    ["2e3330", "PmenuSbar"],
    ["2e3330", "StatusLineNC"],
    ["2e3330", "VertSplit"],
    ["313633", "StatusLine"],
    ["333333", "DiffChange"],
    ["333333", "DiffDelete"],
    ["333333", "VisualNOS"],
    ["353a37", "PMenuSel"],
    ["3F3F3F", "XClipboard*Background"],
    ["3F3F3F", "XClipboard*borderColor"],
    ["3F3F3F", "Xman*Background"],
    ["3F3F3F", "Xmessage*background"],
    ["3F3F3F", "bg_normal"],
    ["3F3F3F", "bg_urgent"],
    ["3F3F3F", "border_normal"],
    ["3F3F3F", "border_widget"],
    ["3F3F3F", "titlebar_bg_focus"],
    ["3F3F3F", "titlebar_bg_normal"],
    ["3F3F3F", "xclock*Background"],
    ["3F3F3F", "xclock*Border"],
    ["3F3F3F", "xdiary*background"],
    ["3f3f3f", "urxvt*background"],
    ["3f3f3f", "urxvt*color0"],
    ["3f3f3f", "xterm*background"],
    ["404040", "NonText"],
    ["494B4F", "bg_widget"],
    ["506070", "urxvt*color4"],
    ["506070", "xterm*color4"],
    ["60b48a", "urxvt*color2"],
    ["60b48a", "xterm*color2"],
    ["6F6F6F", "border_focus"],
    ["6c6c9c", "SpellCap"],
    ["705050", "urxvt*color1"],
    ["705050", "xterm*color1"],
    ["709080", "DiffAdd"],
    ["709080", "urxvt*color8"],
    ["709080", "xterm*color8"],
    ["7cac7c", "SpellLocal"],
    ["7f9f7f", "Comment"],
    ["80d4aa", "ErrorMsg"],
    ["82a282", "SpecialComment"],
    ["88A175", "fg_center_widget"],
    ["8cd0d3", "Number"],
    ["8cd0d3", "urxvt*color6"],
    ["8cd0d3", "xterm*color6"],
    ["8f8f8f", "Delimiter"],
    ["93b3a3", "FoldColumn"],
    ["93b3a3", "Folded"],
    ["93e0e3", "urxvt*color14"],
    ["93e0e3", "xterm*color14"],
    ["94bff3", "urxvt*color12"],
    ["94bff3", "xterm*color12"],
    ["9ece9e", "SpecialKey"],
    ["9fafaf", "LineNr"],
    ["9fafaf", "SignColumn"],
    ["AECF96", "fg_widget"],
    ["CC9393", "border_marked"],
    ["CC9393", "fg_urgent"],
    ["CC9393", "mouse_finder_color"],
    ["DCDCCC", "fg_normal"],
    ["DCDCCC", "urxvt.cursorColor"],
    ["F0DFAF", "XClipboard*Foreground"],
    ["F0DFAF", "Xman*Foreground"],
    ["F0DFAF", "Xmessage*foreground"],
    ["F0DFAF", "fg_focus"],
    ["F0DFAF", "xclock*Foreground"],
    ["F0DFAF", "xdiary*foreground"],
    ["FF5656", "fg_end_widget"],
    ["a0afa0", "PMenuThumb"],
    ["b6bf98", "TabLine"],
    ["bc6c4c", "SpellBad"],
    ["bc6c9c", "SpellRare"],
    ["bca3a3", "Debug"],
    ["c0bed1", "Float"],
    ["c3bf9f", "Exception"],
    ["c3bf9f", "StorageClass"],
    ["c3bf9f", "urxvt*color10"],
    ["c3bf9f", "xterm*color10"],
    ["cc9393", "String"],
    ["cfbfaf", "Special"],
    ["cfcfaf", "TabLineFill"],
    ["dc8cc3", "urxvt*color5"],
    ["dc8cc3", "xterm*color5"],
    ["dca3a3", "Boolean"],
    ["dca3a3", "Character"],
    ["dca3a3", "Constant"],
    ["dca3a3", "SpecialChar"],
    ["dca3a3", "urxvt*color9"],
    ["dca3a3", "xterm*color9"],
    ["dcdccc", "CursorColumn"],
    ["dcdccc", "Directory"],
    ["dcdccc", "Normal"],
    ["dcdccc", "Underlined"],
    ["dcdccc", "urxvt*color7"],
    ["dcdccc", "urxvt*foreground"],
    ["dcdccc", "xterm*color7"],
    ["dcdccc", "xterm*foreground"],
    ["dfaf8f", "PreCondit"],
    ["dfaf8f", "urxvt*color3"],
    ["dfaf8f", "xterm*color3"],
    ["dfcfaf", "Label"],
    ["dfdfbf", "Type"],
    ["dfdfdf", "Todo"],
    ["dfe4cf", "Typedef"],
    ["e3ceab", "Statement"],
    ["e89393", "Tag"],
    ["ec93d3", "urxvt*color13"],
    ["ec93d3", "xterm*color13"],
    ["ecbcbc", "DiffText"],
    ["efdcbc", "Identifier"],
    ["efef8f", "Function"],
    ["efefaf", "Structure"],
    ["efefef", "TabLineSel"],
    ["efefef", "Title"],
    ["f0dfaf", "Conditional"],
    ["f0dfaf", "Keyword"],
    ["f0dfaf", "urxvt*color11"],
    ["f0dfaf", "xterm*color11"],
    ["f0efd0", "Operator"],
    ["f0f0c0", "MatchParen"],
    ["f8f893", "IncSearch"],
    ["ffcfaf", "Define"],
    ["ffcfaf", "Macro"],
    ["ffcfaf", "ModeMsg"],
    ["ffcfaf", "PreProc"],
    ["ffd7a7", "Repeat"],
    ["ffffe0", "Search"],
    ["ffffff", "MoreMsg"],
    ["ffffff", "Question"],
    ["ffffff", "WarningMsg"],
    ["ffffff", "urxvt*color15"],
    ["ffffff", "xterm*color15"],
    ["3f3f3f", "bgcolor"],
    ["dcdccc", "textcolor"],
    ["dcdccc", "pagetitlecolor"],
    ["dfaf8f", "titlecolor"],
    ["7f9f7f", "footercolor"],
    ["8cd0d3", "linkcolor"],
    ["93e0e3", "visitedlinkcolor"],
    ["cc9393", "bordercolor"],
    ["b5d2a9", "activeBackground"],
    ["b5d2a9", "activeBlend"],
    ["6a756e", "activeForeground"],
    ["313633", "activeTitleBtnBg"],
    ["3c3c3c", "alternateBackground"],
    ["6a756e", "background"],
    ["8b9990", "buttonBackground"],
    ["dcdccc", "buttonForeground"],
    ["b5d2a9", "foreground"],
    ["4b614b", "frame"],
    ["8f8f8f", "handle"],
    ["6a756e", "inactiveBackground"],
    ["6a756e", "inactiveBlend"],
    ["8b9990", "inactiveForeground"],
    ["8f8f8f", "inactiveFrame"],
    ["8f8f8f", "inactiveHandle"],
    ["3f3f3f", "inactiveTitleBtnBg"],
    ["8cd0d3", "linkColor"],
    ["4b614b", "selectBackground"],
    ["dcdccc", "selectForeground"],
    ["dca3a3", "visitedLinkColor"],
    ["3f3f3f", "windowBackground"],
    ["dcdccc", "windowForeground"],
    ["1e2320", "zen-black (norm. black)"],
    ["709080", "zen-bright-black (norm. darkgrey)"],
    ["705050", "zen-red (norm. darkred)"],
    ["dca3a3", "zen-bright-red (norm. red)"],
    ["60b48a", "zen-green (norm. darkgreen)"],
    ["c3bf9f", "zen-bright-green (norm. green)"],
    ["dfaf8f", "zen-yellow (norm. brown)"],
    ["f0dfaf", "zen-bright-yellow (norm. yellow)"],
    ["506070", "zen-blue (norm. darkblue)"],
    ["94bff3", "zen-bright-blue (norm. blue)"],
    ["dc8cc3", "zen-purple (norm. darkmagenta)"],
    ["ec93d3", "zen-bright-purple (norm. magenta)"],
    ["8cd0d3", "zen-cyan (norm. darkcyan)"],
    ["93e0e3", "zen-bright-cyan (norm. cyan)"],
    ["dcdccc", "zen-white (norm. lightgrey)"],
    ["ffffff", "zen-bright-white (norm. white)"],
    ["709080", "zenburn-term-dark-gray"],
    ["94bff3", "zenburn-term-light-blue"],
    ["93e0e3", "zenburn-term-light-cyan"],
    ["c3bf9f", "zenburn-term-light-green"],
    ["ec93d3", "zenburn-term-light-magenta"],
    ["dca3a3", "zenburn-term-light-red"],
    ["f0dfaf", "zenburn-term-light-yellow"],
    ["ffffff", "zenburn-term-white"],
    ["000000", "zenburn-term-black"],
    ["506070", "zenburn-term-dark-blue"],
    ["8cd0d3", "zenburn-term-dark-cyan"],
    ["60b48a", "zenburn-term-dark-green"],
    ["dc8cc3", "zenburn-term-dark-magenta"],
    ["705050", "zenburn-term-dark-red"],
    ["dfaf8f", "zenburn-term-dark-yellow"],
    ["dcdccc", "zenburn-fg"],
    ["3f3f3f", "zenburn-bg"],
    ["4f4f4f", "zenburn-bg1"],
    ["5f5f5f", "zenburn-bg2"],
    ["dca3a3", "zenburn-red1"],
    ["cc9393", "zenburn-red"],
    ["bc8383", "zenburn-red1"],
    ["ac7373", "zenburn-red2"],
    ["9c6363", "zenburn-red3"],
    ["8c5353", "zenburn-red4"],
    ["dfaf8f", "zenburn-orange"],
    ["f0dfaf", "zenburn-yellow"],
    ["e0cf9f", "zenburn-yellow1"],
    ["d0bf8f", "zenburn-yellow2"],
    ["5f7f5f", "zenburn-green1"],
    ["7f9f7f", "zenburn-green"],
    ["8fb28f", "zenburn-green1"],
    ["9fc59f", "zenburn-green2"],
    ["afd8af", "zenburn-green3"],
    ["bfebbf", "zenburn-green4"],
    ["93e0e3", "zenburn-cyan"],
    ["94bff3", "zenburn-blue1"],
    ["8cd0d3", "zenburn-blue"],
    ["7cb8bb", "zenburn-blue1"],
    ["6ca0a3", "zenburn-blue2"],
    ["5c888b", "zenburn-blue3"],
    ["4c7073", "zenburn-blue4"],
    ["dc8cc3", "zenburn-magenta"]
    ]
d = {} # dictionary
c = {} # count
for kv in data:
    k = kv[0].lower()
    v = kv[1]
    if (k in d):
        d[k] = d[k] + ", " + v
        c[k] = c[k] + 1
    else:
        d[k] = v
        c[k] = 1

do = OrderedDict(sorted(d.items(), key=lambda t: t[0]))
f.write(begin1)
for k, v in do.items():
    f.write('<tr height = 20><td width = 20 bgcolor = "' + k + '">&nbsp;</td><td><font color = "ffffff">' + k + '</font></td><td><font color = "ffffff">' + str(c[k]) + '</font></td><td><font color = "ffffff">' + v + '</font></td></tr>i\n')
f.write(end1)

do = OrderedDict(sorted(c.items(), key=lambda t: t[1]))
f.write(begin2)
for k, value in do.items():
    v = d[k]
    f.write('<tr height = 20><td width = 20 bgcolor = "' + k + '">&nbsp;</td><td><font color = "ffffff">' + k + '</font></td><td><font color = "ffffff">' + str(c[k]) + '</font></td><td><font color = "ffffff">' + v + '</font></td></tr>\n')
f.write(end2)

f.close()
