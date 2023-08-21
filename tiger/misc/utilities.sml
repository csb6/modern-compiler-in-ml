structure Utilities = struct
    val |> = fn (x, f) => Option.mapPartial f x;
end