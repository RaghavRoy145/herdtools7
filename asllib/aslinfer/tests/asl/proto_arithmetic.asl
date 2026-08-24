func Add(a : integer, b : integer) => integer
begin
    return a + b;
end;

func ArithOps(x : integer, y : integer) => integer
begin
    let sum : integer = x + y;
    let diff : integer = x - y;
    let prod : integer = x * y;
    let quot : integer = x DIV y;
    let rem : integer = x MOD y;
    return sum + diff + prod + quot + rem;
end;
