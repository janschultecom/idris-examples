data Format = Number Format
            | Str Format
            | Character Format
            | Decimal Format
            | Lit String Format
            | End


PrintfType : Format -> Type
PrintfType (Number fmt) = (i:Int) -> PrintfType fmt
PrintfType (Str fmt) = (s:String) -> PrintfType fmt
PrintfType (Character fmt) = (c:Char) -> PrintfType fmt
PrintfType (Decimal fmt) = (d:Double) -> PrintfType fmt
PrintfType (Lit str fmt) = PrintfType fmt
PrintfType End = String


printfFmt : (fmt : Format) -> (acc : String) -> PrintfType fmt
printfFmt (Number fmt) acc = \i => printfFmt fmt (acc ++ show i)
printfFmt (Str fmt) acc = \s => printfFmt fmt (acc ++ s)
printfFmt (Character fmt) acc = \c => printfFmt fmt (acc ++ show c)
printfFmt (Decimal fmt) acc = \d => printfFmt fmt (acc ++ show d)
printfFmt (Lit lit fmt) acc = printfFmt fmt (acc ++ lit)
printfFmt End acc = acc

toFormat : (xs : List Char) -> Format
toFormat [] = End
toFormat ('%' :: 'd' :: chars) = Number (toFormat chars)
toFormat ('%' :: 's' :: chars) = Str (toFormat chars)
toFormat ('%' :: 'c' :: chars) = Character (toFormat chars)
toFormat ('%' :: 'f' :: chars) = Decimal (toFormat chars)
toFormat (c :: chars) = case toFormat chars of
                             Lit lit chars' => Lit (strCons c lit) chars'
                             fmt => Lit (strCons c "") fmt

printf : (fmt : String) -> PrintfType (toFormat (unpack fmt))
printf fmt = printfFmt _ ""
