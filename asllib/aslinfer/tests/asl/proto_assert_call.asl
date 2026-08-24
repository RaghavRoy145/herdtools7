func SafeDiv(x : integer, y : integer) => integer
begin
    assert y != 0;
    return x DIV y;
end;

func Double(x : integer) => integer
begin
    return x + x;
end;

func UseDouble(n : integer) => integer
begin
    let r : integer = Double(n);
    return r + 1;
end;
