' Full indentation test of a type.

Type MyType Extends OtherType

    Const SOME_CONSTANT:Int = 100

    Global TestGlobal:Int

    Field myField:Int
    Field anotherField:String = "Hello"

    Method simpleMethod()
        Print "I am a simple method"
    End Method

    Method completeMethod:Object(arg1:String, arg2:String)
        Print arg1 + " => " + arg2
        Return Null
    End Method

    Function simpleFunction()
        Print "I am a simple function"
    End Function

    Function completeFunction:Object(arg1:String, arg2:String)
        Print arg1 + " => " + arg2
        Return Null
    End Function

End Type
