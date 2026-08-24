type Point of record { x : integer, y : integer };

func InitPoint(a : integer, b : integer) => integer
begin
    var p : Point;
    p.x = a;
    p.y = b;
    return p.x + p.y;
end;

func GetX(p : Point) => integer
begin
    return p.x;
end;

func DistFromOrigin(p : Point) => integer
begin
    return p.x + p.y;
end;
