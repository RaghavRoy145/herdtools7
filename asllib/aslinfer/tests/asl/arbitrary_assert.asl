func ArbitraryAssertBad() => integer
begin
    let x : integer = ARBITRARY : integer {0, 1, 2};
    assert x != 0;
    return 100 DIV x;
end;

func ArbitraryAssertOk() => integer
begin
    let x : integer = ARBITRARY : integer {1, 2, 3};
    assert x != 0;
    return 100 DIV x;
end;
