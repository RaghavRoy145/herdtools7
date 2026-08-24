// Bug: x is declared but never initialised before use.

func UseUninit(flag : integer) => integer
begin
var x : integer;
    if flag > 0 then
        x = x - 42;
    end;
    // x is uninitialised when flag <= 0
    return x;
end;
