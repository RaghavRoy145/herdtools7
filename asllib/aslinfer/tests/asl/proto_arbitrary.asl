
func UnconstrainedArbitrary() => integer
begin
    let x : integer = ARBITRARY : integer;
    return x;
end;

func ConstrainedArbitrary() => integer
begin
    // x is non-deterministic but must be 1, 2, or 3
    let x : integer = ARBITRARY : integer {1, 2, 3};
    return x;
end;

func ArbitraryBranch() => integer
begin
    let x : integer = ARBITRARY : integer {0, 1};
    if x == 0 then
        return 100;
    else
        return 200;
    end;
end;
