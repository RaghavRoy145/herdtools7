type XY of record { x : integer, y : integer };

func PartialInit() => integer
begin
    var foo : XY;
    foo.x = 1;
    return foo.y;
end;
