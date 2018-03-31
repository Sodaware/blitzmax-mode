' Issue 006 - Functions that return types with `Abstract` in the name are not indented properly

Type MyType

    Function returnType:AbstractType()
        ' Should indent one level
    End Function

    Function doAbstractThings()
        ' Should also indent one level
    End Function


    Method returnAType:AbstractType()
        ' Should indent one level
    End Method

    Method doAbstractThingsMethod()
        ' Should also indent one level
    End Method

End Type
