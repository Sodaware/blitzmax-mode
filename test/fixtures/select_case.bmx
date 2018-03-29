' Indent Case
Select myVariable:Int

    case 1
        ' Indented

    case 2
        ' Indented

End Select
' Not indented

Select myVariable:Int
    case 1
    case 2
EndSelect
' Not indented
