' Single-line if/then statements normally indent correctly, but they fail when
' nested inside another block:

If something Then
    ' Indented here
    If False Then Continue
    ' Also indented correctly
Else
    ' Should be indented here
End If
' End of indents
