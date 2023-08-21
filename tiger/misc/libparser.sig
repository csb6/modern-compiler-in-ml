(* Basic parser combinators over unspecified sequence type *)
signature LIBPARSER = sig
    type input_type
    type 'a parser = input_type -> ('a * input_type) option

    (* Always fails *)
    val fail : 'a parser
    (* Always succeeds, yielding a value *)
    val succeed : 'a -> 'a parser
end

(* Higher-order combinators that don't depend on the parser's input_type *)
functor LIBPARSER_Higher_Order (libparser : LIBPARSER) = struct
    val |> = Utilities.|>
    infix |>

    type 'a parser = 'a libparser.parser

    (* Succeeds if at least one of two parsers succeeds *)
    fun alt (parser1:'a parser) (parser2:'a parser) input = (case parser1 input of
       NONE   => parser2 input
     | result => result
    );
    val || = fn (parser1, parser2) => alt parser1 parser2;
    infix ||;

    (* For a list of parsers: [p0, p1, ..., pn], produces a parser
       equivalent to: p0 || p1 || ... || pn *)
    fun altList (alts:'a parser list) = foldl (op ||) (hd alts) (tl alts)

    (* Succeeds if first parser, then second parser, succeed in sequence *)
    fun seq (parser1:'a parser) (parser2:'b parser) input =
        parser1 input
        |> (fn (v1, rest) => parser2 rest
        |> (fn (v2, rest') => libparser.succeed (v1, v2) rest'));
    val --> = fn (parser1, parser2) => seq parser1 parser2;
    infix -->;

    (* Transforms output value of a parser into a new value *)
    fun bind (p:'a parser) f input =
        p input
        |> (fn (v, rest) => libparser.succeed (f v) rest);

    (* If the parser succeeds, yield a given value *)
    fun return p result = bind p (fn _ => result);

    (* Succeeds if parser matches zero or more times *)
    fun zeroOrMore (p:'a parser) input = case p input of
        NONE           => libparser.succeed [] input
      | SOME (v, rest) => (case zeroOrMore p rest of
          NONE             => libparser.succeed [v] rest
        | SOME (vs, rest') => libparser.succeed (v::vs) rest'
      );

    (* Succeeds if parser matches one or more times *)
    fun oneOrMore p = bind (p --> zeroOrMore p) (op ::);

    (* Drop the result of the first parser *)
    fun dropLeft p1 p2 = bind (p1 --> p2) (fn r => #2 r);
    val *--> = fn (parser1, parser2) => dropLeft parser1 parser2;
    infix *-->;

    (* Drop the result of the second parser *)
    fun dropRight p1 p2 = bind (p1 --> p2) (fn r => #1 r);
    val --*> = fn (parser1, parser2) => dropRight parser1 parser2;
    infix --*>;
end