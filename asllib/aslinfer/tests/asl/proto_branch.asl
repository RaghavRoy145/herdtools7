
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

func Absnew(x : integer) => integer
begin
    if x < 0 then
        return 0 - x;
    else
        return x;
    end;
end;
