func ArraySum() => integer
begin
    var arr : array[[3]] of integer;
    arr[[0]] = 10;
    arr[[1]] = 20;
    arr[[2]] = 30;
    return arr[[0]] + arr[[1]] + arr[[2]];
end;

func ArrayUpdate(idx : integer, val : integer) => integer
begin
    var arr : array[[5]] of integer;
    arr[[idx]] = val;
    return arr[[idx]];
end;
