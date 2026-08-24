
// Run with: ASL_EXTERNALS=UNPREDICTABLE
// Topl property: "UNPREDICTABLE must not be reachable"

func UNPREDICTABLE() => integer
begin
    return 0;
end;

// BAD: UNPREDICTABLE reachable when addr not aligned
func CheckAlign(addr : integer) => integer
begin
    if addr MOD 4 != 0 then
        let u : integer = UNPREDICTABLE();
        return u;
    end;
    return addr;
end;

// GOOD: no path reaches UNPREDICTABLE
func AlwaysAligned(addr : integer) => integer
begin
    let aligned : integer = addr - (addr MOD 4);
    return aligned;
end;
