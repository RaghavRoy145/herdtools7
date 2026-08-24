


func UninitLocal(flag : integer) => integer
begin
    var x : integer;
    if flag > 0 then
        x = 42;
    end;
    return x;
end;


func InitLocal(flag : integer) => integer
begin
    var x : integer;
    if flag > 0 then
        x = 42;
    else
        x = 0;
    end;
    return x;
end;
