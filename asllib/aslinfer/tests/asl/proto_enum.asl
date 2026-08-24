
type Color of enumeration { RED, GREEN, BLUE };

func IsRed(c : Color) => boolean
begin
    return c == RED;
end;

func ColorToInt(c : Color) => integer
begin
    if c == RED then
        return 0;
    elsif c == GREEN then
        return 1;
    else
        return 2;
    end;
end;
