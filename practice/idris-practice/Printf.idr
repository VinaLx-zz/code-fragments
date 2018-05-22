module Printf

data Format = Decimal Format
            | Floating Format
            | Str Format
            | Literal String Format
            | End

FormatType : Format -> Type
FormatType format =
  case format of
       (Decimal fmt) => Integer -> FormatType fmt
       (Floating fmt) => Double -> FormatType fmt
       (Str fmt) => String -> FormatType fmt
       (Literal _ fmt) => FormatType fmt
       End => String

stringToFormat : List Char -> Format
stringToFormat s =
  case s of
       [] => End
       '%' :: c :: rest =>
             case c of
                  'd' => Decimal (stringToFormat rest)
                  'f' => Floating (stringToFormat rest)
                  's' => Str (stringToFormat rest)
                  '%' => combineLiteral '%' (stringToFormat rest)
                  other => combineLiteral '%' (stringToFormat (c :: rest))
       c :: rest => combineLiteral c (stringToFormat rest)
  where combineLiteral : Char -> Format -> Format
        combineLiteral c fmt =
          case fmt of
               Literal s fmt => Literal (strCons c s) fmt
               other         => Literal (cast c) other 

printF : (format : Format) -> (acc : String) -> FormatType format
printF (Decimal fmt) acc = \d => printF fmt (acc ++ (show d))
printF (Floating fmt) acc = \f => printF fmt (acc ++ (show f))
printF (Str fmt) acc = \s => printF fmt (acc ++ s)
printF (Literal s fmt) acc = printF fmt (acc ++ s)
printF End acc = acc

printf : (format: String) -> (FormatType (stringToFormat (unpack format)))
printf format = printF (stringToFormat (unpack format)) ""
