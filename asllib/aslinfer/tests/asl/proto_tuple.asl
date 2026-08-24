
func SwapPair(a : integer, b : integer) => (integer, integer)
begin
    return (b, a);
end;

func UseTuple() => integer
begin
    let (x, y) = SwapPair(10, 20);
    return x + y;
end;

func TripleSumFirst() => integer
begin
    let t = (1, 2, 3);
    return t.item0;
end;
