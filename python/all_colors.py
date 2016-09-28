import string

header = """
<html>
<head><title></title></head>
<body bgcolor=\"#000000\">
<center>
<table>
"""

footer = """
</table>
</center>
</body>
</html>
"""

file = open("all_colors.html", "w")
file.write(header)
for i1 in string.hexdigits[:16]:
    file.write('<tr>\n')
    for i3 in string.hexdigits[:16]:
        number = str(i1) + str(0) + str(i3)
        file.write('<td bgcolor="#' + number + '0FF">.</td>\n')
    file.write('</tr>\n')
file.write(footer)
file.close()
