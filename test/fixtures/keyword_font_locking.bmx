Type ThisIsMyType Extends TypeWithFunctionKeywordName
    Field myField:Int
    Global myGlobal:Int

    ' Comment goes here
    Function myFunction()
        Print ThisIsMyType.myGlobal
    End Function

    Method myMethod()
        Print self.myField
    End Method

End Type
