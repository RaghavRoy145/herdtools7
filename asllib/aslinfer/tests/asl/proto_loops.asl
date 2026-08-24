
func SumTo(n : integer) => integer
begin
    var total : integer = 0;
    for i = 1 to n do
        total = total + i;
    end;
    return total;
end;

func Countdown(start : integer) => integer
begin
    var x : integer = start;
    while x > 0 do
        x = x - 1;
    end;
    return x;
end;

func CountUp(limit : integer) => integer
begin
    var x : integer = 0;
    repeat
        x = x + 1;
    until x >= limit;
    return x;
end;
