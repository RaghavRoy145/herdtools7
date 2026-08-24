// Bug: UNDEFINED is reachable when addr is not aligned.
func UNDEFINED() => integer
begin
    return 0;
end;

func CheckAlign(addr : integer) => integer
begin
    if addr MOD 4 != 0 then
        let u : integer = UNDEFINED();
        return u;
    end;
    return addr;
end;
