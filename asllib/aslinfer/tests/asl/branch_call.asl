// Combined branching + interprocedural call.
func Clamp(x : integer, lo : integer, hi : integer) => integer
begin
    if x < lo then
        return lo;
    elsif x > hi then
        return hi;
    else
        return x;
    end;
end;

func ClampPositive(x : integer) => integer
begin
    let result : integer = Clamp(x, 0, 100);
    return result;
end;
