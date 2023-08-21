(* Parser combinators over substrings *)
structure LibParserStr = struct
    val |> = Utilities.|>
    infix |>

    type input_type = substring
    type 'a parser = substring -> ('a * substring) option

    fun fail input = NONE : ('a * substring) option;

    fun succeed v (input:substring) = SOME (v, input);

    fun satisfy pred input =
        if Substring.isEmpty input then fail input
        else Substring.getc input
            |> (fn (c, rest) =>
                if pred c then succeed c rest
                else fail input);

    fun charLit c = satisfy (fn x => x = c);

    fun xOrMoreStrHelper failAction f input = let
        fun helper pos =
            if pos < Substring.size input andalso f (Substring.sub (input, pos)) then helper (pos+1)
            else if pos > 0 then SOME (Substring.splitAt (input, pos))
            else failAction input
    in
        helper 0
    end;

    val zeroOrMoreStr = xOrMoreStrHelper (fn input => SOME (Substring.splitAt (input, 0)));
    val oneOrMoreStr = xOrMoreStrHelper (fn _ => NONE);

    val digits = oneOrMoreStr Char.isDigit
    val letters = oneOrMoreStr Char.isAlpha

    fun symbol target input =
        if Substring.isPrefix target input then SOME (Substring.splitAt (input, String.size target))
        else NONE;
end

structure LibParserStr_Higher_Order = LIBPARSER_Higher_Order (LibParserStr : LIBPARSER)