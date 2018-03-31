' Issue #004 - Extern does not indent correctly

' With a space in end name
Extern
    ' Should be indented one level
End Extern
' After "End Extern" should not be indented

' Without comment
Extern
    ' Should be indented one level
EndExtern
' After "End Extern" should not be indented
