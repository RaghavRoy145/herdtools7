// Interprocedural: caller uses callee's return value.
func Double(x : integer) => integer
begin
    return x + x;
end;

func CallDouble(n : integer) => integer
begin
    let r : integer = Double(n);
    return r + 1;
end;
