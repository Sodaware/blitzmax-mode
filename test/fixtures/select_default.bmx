' Indent Case
Select myVariable:Int

    case 1
        ' Indented

    default
        ' Indented

End Select
' Not indented

Select myVariable:Int
    case 1
        ' Indented
    default
        ' Indented
EndSelect
' Not indented
