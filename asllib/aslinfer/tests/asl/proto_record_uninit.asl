
type XY of record { x : integer, y : integer };

// BAD: y never initialised
func PartialInit() => integer
begin
    var foo : XY;
    foo.x = 1;
    return foo.y;
end;

// GOOD: both fields initialised
func FullInit() => integer
begin
    var foo : XY;
    foo.x = 1;
    foo.y = 2;
    return foo.x + foo.y;
end;
