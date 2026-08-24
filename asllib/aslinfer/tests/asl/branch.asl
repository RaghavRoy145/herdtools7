// Both branches of an if/else, proving Pulse handles prune.
func MyAbs(x : integer) => integer
begin
    if x < 0 then
        return 0 - x;
    else
        return x;
    end;
end;
