func SecurityCheck(val : integer) => integer
begin
    return val;
end;

func ArbitraryBad() => integer
begin
    let x : integer = ARBITRARY : integer;
    let result : integer = SecurityCheck(x);
    return result;
end;

func ArbitrarySafe() => integer
begin
    let x : integer = 42;
    let result : integer = SecurityCheck(x);
    return result;
end;
