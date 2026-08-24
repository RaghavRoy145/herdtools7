func DangerousOp(x : integer) => integer
begin
    return x;
end;

func SafeOp(x : integer) => integer
begin
    return x;
end;

// BAD: arbitrary value {0, 1, 2} flows to DangerousOp
func ConstrainedBad() => integer
begin
    let x : integer = ARBITRARY : integer {0, 1, 2};
    let result : integer = DangerousOp(x);
    return result;
end;

// GOOD: only uses a fixed value
func ConstrainedGood() => integer
begin
    let x : integer = 42;
    let result : integer = SafeOp(x);
    return result;
end;
