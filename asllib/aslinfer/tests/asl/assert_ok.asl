func AlwaysSafe(x : integer) => integer
begin
    if x > 0 then
        assert x > 0;
    end;
    return x;
end;
