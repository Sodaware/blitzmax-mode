' Standard block.
Try
    do_this()
Catch e:Object
    Print "Exception"
End Try

' Not indented afterwards

Try
    do_this()
Catch e:Object
    Print "Exception"
EndTry

' Also supports single word ending
