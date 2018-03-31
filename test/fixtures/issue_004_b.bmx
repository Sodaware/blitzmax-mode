' Issue #004 - Extern does not indent correctly

' With functions
Extern
    Function ExternalFunction()
    Function AnotherExternalFunction()
End Extern
' After "End Extern" should not be indented
