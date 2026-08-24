func UNPREDICTABLE() => integer
begin
    return 0;
end;

func ConstrainedBad() => integer
begin
    let x : integer = ARBITRARY : integer {0, 1, 2};
    if x == 0 then
        let u : integer = UNPREDICTABLE();
        return u;
    end;
    return x;
end;
