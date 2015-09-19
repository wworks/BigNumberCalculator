Imports System.Text

Public Class BigNumberCalculator

    ''' <summary>
    ''' Returns the reuslt of LeftOperand + RightOperand as a String.
    ''' </summary>
    ''' <param name="LeftOperand">Must be a whole number.</param>
    ''' <param name="RightOperand">Must be a whole number.</param>
    ''' <returns>Returns sum of LeftOperand and RightOperand as String</returns>
    ''' <remarks></remarks>
    Public Function Add(LeftOperand As String, RightOperand As String) As String

        'Handles negative number's:
        Dim IsNegative = False

        If (LeftOperand.Contains("-") And RightOperand.Contains("-") = False) Then
            Return Subtract(RightOperand, LeftOperand.Remove(0, 1))

        ElseIf (LeftOperand.Contains("-") = False And RightOperand.Contains("-") = True) Then
            Return Subtract(LeftOperand, RightOperand.Remove(0, 1))

        ElseIf LeftOperand.Contains("-") And RightOperand.Contains("-") Then
            LeftOperand = LeftOperand.Remove(0, 1)
            RightOperand = RightOperand.Remove(0, 1)
            IsNegative = True
        End If


        'Casting to CharArray for individual digit acces.
        Dim LeftOperandArray = LeftOperand.ToCharArray
        Dim RightOperandArray = RightOperand.ToCharArray

        Dim ResultOfAddition(Math.Max(RightOperand.Length, LeftOperand.Length) - 1) As Integer

        'The addition:
        For i = 0 To ResultOfAddition.Length - 1
            If i < LeftOperandArray.Length Then
                'Filling the result with the digits of the LeftOperand
                ResultOfAddition(i) += CInt(LeftOperandArray(LeftOperandArray.Length - 1 - i).ToString)
            End If
            If i < RightOperandArray.Length Then
                'Adding the values of the RightOperand to the result.
                ResultOfAddition(i) += CInt(RightOperandArray(RightOperandArray.Length - 1 - i).ToString)
            End If

        Next
        For i = 0 To ResultOfAddition.Length - 2
            'Bringing over the numbers > 9
            If ResultOfAddition(i) > 9 Then
                ResultOfAddition(i + 1) += (ResultOfAddition(i) \ 10)
                ResultOfAddition(i) = (ResultOfAddition(i) Mod 10)

            End If
        Next

        'Preparing the answer, using Stringbuilder for speed.
        Dim Number As New System.Text.StringBuilder
        For i = ResultOfAddition.Length - 1 To 0 Step -1
            Number.Append(ResultOfAddition(i))

        Next

        'Returning the answer
        If Number.ToString = "0" Then
            Return 0
        Else
            If IsNegative Then
                Return "-" & Number.ToString.TrimStart("0")
            Else
                Return Number.ToString.TrimStart("0")
            End If
        End If

    End Function

    ''' <summary>
    ''' Returns the result of LeftOperand - RightOperand as string, where LeftOperand and RightOperand are whole numbers.
    ''' </summary>
    ''' <param name="LeftOperand">Must be whole numbers.</param>
    ''' <param name="RightOperand">Must be whole numbers.</param>
    ''' <returns>Returns the result of LeftOperand - RightOperand as a String.</returns>
    ''' <remarks></remarks>
    Public Function Subtract(LeftOperand As String, RightOperand As String) As String


        'Handling negative number's
        Dim Negative = False
        Dim LeftNegative = LeftOperand.Contains("-")
        Dim RightNegative = RightOperand.Contains("-")

        If (LeftNegative AndAlso RightNegative = False) Then
            Return "-" & Add(RightOperand, LeftOperand.Remove(0, 1))
        ElseIf LeftNegative = False AndAlso RightNegative Then
            Return Add(LeftOperand, RightOperand.Remove(0, 1))
        ElseIf LeftNegative And RightNegative Then
            LeftOperand = LeftOperand.Remove(0, 1)
            RightOperand = RightOperand.Remove(0, 1)
            Negative = True
        End If

        'If both operands return 0
        If LeftOperand = RightOperand Then
            Return "0"
        End If

        'Figuring out the biggest number for algorithm purposes.
        Dim BiggestNumber
        Dim SmallestNumber

        If BiggerThan(LeftOperand, RightOperand) = True Then
            BiggestNumber = LeftOperand.ToCharArray
            SmallestNumber = RightOperand.ToCharArray
        Else
            BiggestNumber = RightOperand.ToCharArray
            SmallestNumber = LeftOperand.ToCharArray
            Negative = True

        End If

        Dim ResultOfSubtraction(BiggestNumber.length - 1) As Integer

        For Digit = 0 To BiggestNumber.Length - 1
            'Filling the result with the digits of the biggest number
            ResultOfSubtraction(Digit) = CInt(BiggestNumber(BiggestNumber.length - 1 - Digit).ToString)
            'Subtracting the digits of the smalles number from the result.
            If Digit < SmallestNumber.length Then
                ResultOfSubtraction(Digit) -= CInt(SmallestNumber(SmallestNumber.length - Digit - 1).ToString)
            End If
        Next

        'Bringing over to small digits
        For Digit = 0 To ResultOfSubtraction.Length - 2
            If ResultOfSubtraction(Digit) < 0 Then
                ResultOfSubtraction(Digit + 1) -= 1
                ResultOfSubtraction(Digit) = 10 + ResultOfSubtraction(Digit)
            End If
        Next

        'Preparing the answer, using Stringbuilder for speed.
        Dim Number As New System.Text.StringBuilder
        For Digit = ResultOfSubtraction.Length - 1 To 0 Step -1
            Number.Append(ResultOfSubtraction(Digit))
        Next

        'Returning the answer.
        If Negative Then
            Return "-" & Number.ToString.TrimStart("0")
        Else
            Return Number.ToString.TrimStart("0")
        End If

    End Function


    ''' <summary>
    ''' Returns LeftOperand * RightOperand as string.
    ''' </summary>
    ''' <param name="LeftOperand">Must be a whole number.</param>
    ''' <param name="RightOperand">Must be a whole number.</param>
    ''' <returns>Returns LeftOperand * RightOperand as string.</returns>
    ''' <remarks></remarks>
   
    Public Function Multiply(LeftOperand As String, RightOperand As String) As String

        'Handling negative number's
        Dim IsNegative = False
        Dim lefttoperandNegative = LeftOperand.Contains("-")
        Dim rightoperandNegative = RightOperand.Contains("-")

        If (lefttoperandNegative AndAlso rightoperandNegative = False) Then
            IsNegative = True
            LeftOperand = LeftOperand.Remove(0, 1)
        ElseIf (lefttoperandNegative = False AndAlso rightoperandNegative) Then
            IsNegative = True
            RightOperand = RightOperand.Remove(0, 1)
        ElseIf lefttoperandNegative And rightoperandNegative Then
            RightOperand = RightOperand.Remove(0, 1)
            LeftOperand = LeftOperand.Remove(0, 1)
        End If

        'Optimizing, the iterator must be the smallest number.
        Dim SmallestNumber
        Dim biggestNumber
        If LeftOperand.Length > RightOperand.Length Then
            SmallestNumber = RightOperand
            biggestNumber = LeftOperand
        Else
            SmallestNumber = LeftOperand
            biggestNumber = RightOperand
        End If

        'Multiplying
        Dim Result As String = "0"

        While SmallestNumber <> "0"
            Dim Fast = False
            If SmallestNumber.Length > 1 Then
                Dim ZeroStart = SmallestNumber.LastIndexOf("0")
                If ZeroStart > -1 Then
                    Dim First = SmallestNumber.Remove(ZeroStart)
                    Dim Zeros = SmallestNumber.Remove(0, ZeroStart)
                    SmallestNumber = First
                    biggestNumber &= Zeros
                    Fast = True
                End If
            End If
            If Fast = False Then
                SmallestNumber = Subtract(SmallestNumber, 1)
                Result = Add(Result, biggestNumber)
            End If
        End While

        'Returning the result
        If IsNegative Then
            Return "-" & Result
        Else
            Return Result
        End If
    End Function

    ''' <summary>
    ''' Returns the result of Nominator / Denominator as string, with specified amount of decimal places.
    ''' </summary>
    ''' <param name="Nominator">Must be whole number.</param>
    ''' <param name="Denominator">Must be whole number.</param>
    ''' <param name="MaxDigits">Optional, if supplied specifies the length of the result.</param>
    ''' <param name="DecimalPlaces">Optional, specifies the amount of decimal places in the result. The default is 0 for N/D bigger than 0, 2 for N/D smaller than 0 </param>
    ''' <returns></returns>
    ''' <remarks>The result is not rounded(14,656 = 14,65), it just gives the specified amount of decimal places. Closer(lengthwise) operands will be faster to calculate.</remarks>
    Public Function Divide(Nominator As String, Denominator As String, Optional MaxDigits As Integer = Nothing, Optional DecimalPlaces As Integer = -1) As String
        'Handles dividing by zero.
        If Nominator = "0" And Denominator = "0" Then
            Return "Undefined"
        ElseIf Nominator = "0" Or Denominator = "0" Then
            Return "0"
        End If

        'Handles negative number's:
        Dim IsNegative = False
        Dim NominatorNegative = Nominator.Contains("-")
        Dim DenominatorNegative = Denominator.Contains("-")

        If (NominatorNegative AndAlso DenominatorNegative = False) Then
            IsNegative = True
            Nominator = Nominator.Remove(0, 1)
        ElseIf (NominatorNegative = False AndAlso DenominatorNegative) Then
            IsNegative = True
            Denominator = Denominator.Remove(0, 1)
        ElseIf NominatorNegative And DenominatorNegative Then
            Denominator = Denominator.Remove(0, 1)
            Nominator = Nominator.Remove(0, 1)
        End If

        'Determine type of division to use;
        If Denominator = Nominator Then
            Return "1"
        ElseIf BiggerThan(Nominator, Denominator) Then

            'Setting the number of digits of the returned result.
            If MaxDigits = 0 Then
                MaxDigits = Max(Nominator, Denominator).Length - min(Nominator, Denominator).Length + 1
                If DecimalPlaces <> -1 Then
                    MaxDigits += DecimalPlaces + 1

                End If
            End If

            'Doing the actual division work.
            Dim Result As String = ""
            Dim Remainder As String = Nominator
            Dim CommaSet = False

            While Result.Length < MaxDigits

                'Append the comma:
                If CommaSet = False AndAlso Result <> "" AndAlso BiggerThan(Denominator, Remainder.Remove(Remainder.Length - 1)) Then
                    Result &= ","
                    CommaSet = True

                End If

                'Appending the digit
                Result &= IntegerDivision(Remainder, Denominator)

                'Setting up the ramainder for the next digit:
                Remainder = Modulus(Remainder, Denominator) & "0"

            End While

            'Return the answer:
            If IsNegative Then
                Return "-" & Result
            Else
                Return Result
            End If

        Else
            'Setting the number of digits of the returned result.
            If MaxDigits = 0 Then
                If DecimalPlaces <> -1 Then
                    MaxDigits += DecimalPlaces + 2
                Else
                    MaxDigits = 3
                End If

            End If

            Dim Result As String = "0,"
            Dim Remainder As String = Nominator & "0"

            While Result.Length < MaxDigits
                'Appending the digit
                Result &= IntegerDivision(Remainder, Denominator)

                'Setting up the ramainder for the next digit:
                Remainder = Modulus(Remainder, Denominator) & "0"
            End While

            'Return the answer:
            If IsNegative Then
                Return "-" & Result
            Else
                Return Result
            End If

        End If
    End Function


    ''' <summary>
    ''' Returns Base ^ Power as string. Maxdigits must be large enough to contain the whole number, otherwise the result will be unnaccurate, and thus incorrect.
    ''' </summary>
    ''' <param name="Base">Must be a whole number.</param>
    ''' <param name="Power">Must be a whole number.</param>
    ''' <param name="MaxDigits">Must be a whole number. Large enought to contain the whole number.</param>
    ''' <returns>Returns Base ^ Power as string.</returns>
    ''' <remarks>Base and Power are Integers, can't imagine anybody calculating something larger than 2,147,483,647 ^ 2,147,483,647, also it's much faster with integers.</remarks>
    Public Function Exponent(Base As Integer, Power As Integer, MaxDigits As Integer) As String

        'Handling cornercases
        If Power = 0 Then
            Return 1
        ElseIf Base = 0 Then
            Return 0
        End If

        'Handling negative numbers

        Dim baseIsNegative = Base < 0
        Dim powerIsNegative = Power < 0

        If (baseIsNegative AndAlso powerIsNegative = False) Then
            Base *= -1
        ElseIf (baseIsNegative = False AndAlso powerIsNegative) Then
            Power *= -1
            Return Divide(1, Exponent(Base, Power, MaxDigits), MaxDigits)
        ElseIf baseIsNegative And powerIsNegative Then
            Power *= -1
            Base *= -1
            Return Divide(1, Exponent(Base, Power, MaxDigits), MaxDigits)
        End If


        'The exponentiation
        Dim Result(MaxDigits) As Integer

        Result(0) = Base
        For i As Integer = 1 To Power - 1
            'Multiplying
            For j As Integer = 0 To MaxDigits - 1
                Result(j) = Result(j) * Base
            Next
            'Bringing over too big numbers
            For j As Integer = 0 To MaxDigits - 1
                If Result(j) > 9 Then
                    Result(j + 1) += (Result(j) \ 10)
                    Result(j) = (Result(j) Mod 10)

                End If
            Next
        Next

        'Preparing the result
        Dim Number As New System.Text.StringBuilder
        For i = MaxDigits To 0 Step -1
            Number.Append(Result(i))

        Next

        'Returning the answer
        Return Number.ToString.TrimStart("0")



    End Function


    ''' <summary>
    ''' Returns LeftOperand Mod RightOperand.
    ''' </summary>
    ''' <param name="LeftOperand">Must be a whole number.</param>
    ''' <param name="RightOperand">Must be a whole number.</param>
    ''' <returns>Returns LeftOperand Mod RightOperand</returns>
    ''' <remarks></remarks>
    Public Function Modulus(LeftOperand As String, RightOperand As String) As String
        'Handling corner cases:
        If RightOperand = "0" Or IsZero(RightOperand) Then
            Return "Undefined"
        ElseIf RightOperand = "1" Then
            Return "0"
        End If
        If LeftOperand = "0" Or IsZero(LeftOperand) Then
            Return "0"
        End If

        'Handling negative numbers:
        Dim IsNegative = False
        Dim lefttoperandNegative = LeftOperand(0) = "-"
        Dim rightoperandNegative = RightOperand(0) = "-"
        If (lefttoperandNegative AndAlso rightoperandNegative = False) Then
            IsNegative = True
            LeftOperand = LeftOperand.Remove(0, 1)
        ElseIf (lefttoperandNegative = False AndAlso rightoperandNegative) Then
            RightOperand = RightOperand.Remove(0, 1)
        ElseIf lefttoperandNegative And rightoperandNegative Then
            RightOperand = RightOperand.Remove(0, 1)
            LeftOperand = LeftOperand.Remove(0, 1)
            IsNegative = True
        End If

        'Handling more corner cases:
        If BiggerThan(RightOperand, LeftOperand) Then
            If IsNegative Then
                Return "-" & LeftOperand
            End If
            Return LeftOperand
        ElseIf RightOperand = LeftOperand Then
            Return 0
        End If

        'Doing the work:
        Dim Leftln = LeftOperand.Length
        Dim rightln = RightOperand.Length

        For i = Leftln - rightln To 0 Step -1
            'First removing bigger versions(*10,*100) of the RightOperand from LeftOperand
            Dim times = (New String("0", (i)))
            While Leftln - rightln > 0 And BiggerThan(LeftOperand, RightOperand & times)
                LeftOperand = Subtract(LeftOperand, RightOperand & times)
                Leftln = LeftOperand.Length
            End While
        Next

        While LeftOperand = RightOperand OrElse BiggerThan(LeftOperand, RightOperand) = True
            Dim SubtractedValue = Subtract(LeftOperand, RightOperand)
            If SubtractedValue = "0" OrElse BiggerThan(SubtractedValue, "0") Then
                LeftOperand = SubtractedValue
            End If
        End While

        'Returning the result
        If IsNegative Then
            Return "-" & LeftOperand
        Else
            Return LeftOperand
        End If

    End Function

    ''' <summary>
    ''' Returns LeftOperand \ RightOperand as string, basically how many times RightOperand Fits into LeftOperand.
    ''' </summary>
    ''' <param name="LeftOperand">Must be whole number.</param>
    ''' <param name="RightOperand">Must be whole number.</param>
    ''' <returns>Returns LeftOperand \ RightOperand as String</returns>
    ''' <remarks>If the numbers are closer together, the algorithm will be faster.</remarks>
    Public Function IntegerDivision(LeftOperand As String, RightOperand As String) As String
        'Handling cornercases
        If RightOperand = "0" OrElse LeftOperand = "0" OrElse IsZero(RightOperand) OrElse IsZero(LeftOperand) Then
            Return "0"
        End If

        If RightOperand = "1" Then
            Return LeftOperand
        End If

        'Handling negative numbers
        Dim IsNegative = False
        Dim lefttoperandNegative = LeftOperand(0) = "-"
        Dim rightoperandNegative = RightOperand(0) = "-"
        If (lefttoperandNegative AndAlso rightoperandNegative = False) Then
            IsNegative = True
            LeftOperand = LeftOperand.Remove(0, 1)
        ElseIf (lefttoperandNegative = False AndAlso rightoperandNegative) Then
            RightOperand = RightOperand.Remove(0, 1)
        ElseIf lefttoperandNegative And rightoperandNegative Then
            RightOperand = RightOperand.Remove(0, 1)
            LeftOperand = LeftOperand.Remove(0, 1)
            IsNegative = False
        End If

        'Handling another cornercase
        If BiggerThan(RightOperand, LeftOperand) Then
            Return 0
        ElseIf RightOperand = LeftOperand Then
            Return 1
        End If

        Dim Leftln = LeftOperand.Length
        Dim rightln = RightOperand.Length
        Dim count As String = "0"

        'First removing bigger versions(*10,*100) of the RightOperand from LeftOperand
        For i = Leftln - rightln To 0 Step -1
            Dim times = (New String("0", (i)))
            While Leftln - rightln > 0 And (BiggerThan(LeftOperand, RightOperand & times) Or LeftOperand = RightOperand & times)
                LeftOperand = Subtract(LeftOperand, RightOperand & times)
                Leftln = LeftOperand.Length
                count = Add(count, 1 & times)
            End While
        Next
        'Integer division of the remaining LeftOperand
        While LeftOperand = RightOperand OrElse BiggerThan(LeftOperand, RightOperand) = True
            Dim SubtractedValue = Subtract(LeftOperand, RightOperand)
            If SubtractedValue = "0" OrElse BiggerThan(SubtractedValue, "0") Then
                LeftOperand = SubtractedValue
                count = Add(count, 1)
            End If
        End While

        'Returning the result
        If IsNegative Then
            Return "-" & count
        Else
            Return count
        End If
    End Function
   
    ''' <summary>
    ''' Returns the highest number of the two numbers suplied. If both numbers are the same, it will return LeftOperand(same as RightOperand).
    ''' </summary>
    ''' <param name="LeftOperand">Must be a whole number</param>
    ''' <param name="RightOperand">Must be a whole number</param>
    ''' <returns>Returns the highest of the two suplied numbers.</returns>
    ''' <remarks></remarks>
    Public Function Max(LeftOperand As String, RightOperand As String) As String
        'If both operands are equal just return LeftOperand
        If LeftOperand = RightOperand Then
            Return LeftOperand
        End If

        'Handling negative numbers
        Dim lefttoperandNegative = LeftOperand(0) = "-"
        Dim rightoperandNegative = RightOperand(0) = "-"
        If (lefttoperandNegative AndAlso rightoperandNegative = False) Then
            Return RightOperand
        ElseIf (lefttoperandNegative = False AndAlso rightoperandNegative) Then
            Return LeftOperand
        ElseIf lefttoperandNegative And rightoperandNegative Then
            RightOperand = RightOperand.Remove(0, 1)
            LeftOperand = LeftOperand.Remove(0, 1)
            Return "-" & Min(LeftOperand, RightOperand)
        End If



        'Comparing the numbers to find out the max
        If LeftOperand.Length > RightOperand.Length Then
            Return LeftOperand
        ElseIf LeftOperand.Length < RightOperand.Length Then
            Return RightOperand
        Else
            Dim LeftOperandArray = LeftOperand.ToCharArray
            Dim RightOperandArray = RightOperand.ToCharArray
            For Digit = 0 To LeftOperandArray.Count - 1
                If LeftOperandArray(Digit) > RightOperand(Digit) Then
                    Return LeftOperand

                ElseIf LeftOperandArray(Digit) < RightOperand(Digit) Then
                    Return RightOperand

                End If
            Next
        End If

       

    End Function

    ''' <summary>
    ''' Returns the lowest number of the two numbers suplied. If both numbers are the same, it will return LeftOperand(same as RightOperand).
    ''' </summary>
    ''' <param name="LeftOperand">Must be a whole number</param>
    ''' <param name="RightOperand">Must be a whole number</param>
    ''' <returns>Returns the lowest of the two suplied numbers.</returns>
    ''' <remarks></remarks>
    Public Function Min(LeftOperand As String, RightOperand As String) As String
        'If both operands are equal just return LeftOperand
        If LeftOperand = RightOperand Then
            Return LeftOperand
        End If

        'Handling negative numbers
        Dim lefttoperandNegative = LeftOperand(0) = "-"
        Dim rightoperandNegative = RightOperand(0) = "-"
        If (lefttoperandNegative AndAlso rightoperandNegative = False) Then
            Return LeftOperand
        ElseIf (lefttoperandNegative = False AndAlso rightoperandNegative) Then
            Return RightOperand
        ElseIf lefttoperandNegative And rightoperandNegative Then
            RightOperand = RightOperand.Remove(0, 1)
            LeftOperand = LeftOperand.Remove(0, 1)
            Return "-" & Max(LeftOperand, RightOperand)
        End If


        'Comparing the numbers to find out the minimum
        If LeftOperand.Length > RightOperand.Length Then
            Return RightOperand
        ElseIf LeftOperand.Length < RightOperand.Length Then
            Return LeftOperand
        Else
            Dim LeftOperandArray = LeftOperand.ToCharArray
            Dim RightOperandArray = RightOperand.ToCharArray
            For Digit = 0 To LeftOperandArray.Count - 1
                If LeftOperandArray(Digit) > RightOperand(Digit) Then
                    Return RightOperand

                ElseIf LeftOperandArray(Digit) < RightOperand(Digit) Then
                    Return LeftOperand

                End If
            Next
        End If

    End Function

    ''' <summary>
    ''' Returns LeftOperand > RightOperand as boolean.
    ''' </summary>
    ''' <param name="LeftOperand">Must be a whole number.</param>
    ''' <param name="RightOperand">Must be a whole number.</param>
    ''' <returns>Returns a boolean value, true if LeftOperand > RightOperand, false in all other cases.</returns>
    ''' <remarks></remarks>
    Public Function BiggerThan(LeftOperand As String, RightOperand As String) As Boolean
        'If operands are equal this means LeftOperand can't be bigger.
        If LeftOperand = RightOperand Then
            Return False
        End If

        'Handling negative numbers.
        If LeftOperand.Contains("-") And RightOperand.Contains("-") = False Then
            Return False
        ElseIf LeftOperand.Contains("-") = False And RightOperand.Contains("-") = True Then
            Return True
        ElseIf LeftOperand.Contains("-") And RightOperand.Contains("-") Then
            LeftOperand = LeftOperand.Remove(0)
            RightOperand = RightOperand.Remove(0)
        End If

        'Figuring out which one is bigger, by length and individual digits.
        If LeftOperand.Length > RightOperand.Length Then
            Return True
        ElseIf LeftOperand.Length < RightOperand.Length Then
            Return False
        Else
            Dim LeftOperandArray = LeftOperand.ToCharArray
            Dim RightOperandArray = RightOperand.ToCharArray
            For Digit = 0 To LeftOperandArray.Count - 1
                If LeftOperandArray(Digit) > RightOperand(Digit) Then
                    Return True

                ElseIf LeftOperandArray(Digit) < RightOperand(Digit) Then
                    Return False

                End If
            Next
        End If

    End Function

    ''' <summary>
    ''' Returns Number = 0 as Boolean.
    ''' </summary>
    ''' <param name="Number">Must be whole number.</param>
    ''' <returns>Returns a Boolean value indicating if Number = 0.</returns>
    ''' <remarks></remarks>
    Public Function IsZero(Number As String) As Boolean
        'Return false if one of the digits doesn't equal 0
        For Digit = 0 To Number.Length - 1
            If Number(Digit) <> "0" Then
                Return False
            End If
        Next

        'Returning true in all other cases
        Return True

    End Function

End Class
