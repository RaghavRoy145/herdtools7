
type Point of record { x : integer, y : integer };

func MakePoint(a : integer, b : integer) => Point
begin
    var p : Point;
    p.x = a;
    p.y = b;
    return p;
end;

func GetX(p : Point) => integer
begin
    return p.x;
end;

func DistFromOrigin(p : Point ) => integer
begin
    return p.x + p.y;
end;
