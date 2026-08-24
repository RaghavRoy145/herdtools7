
// Run with: ASL_EXTERNALS=TaintSource,TaintSink

func TaintSource() => integer
begin
    return 0;
end;

func TaintSink(x : integer) => integer
begin
    return x;
end;

// BAD: tainted value flows to sink
func TaintBad() => integer
begin
    let tainted : integer = TaintSource();
    let result : integer = TaintSink(tainted);
    return result;
end;

// GOOD: clean value flows to sink
func TaintGood() => integer
begin
    let clean : integer = 42;
    let result : integer = TaintSink(clean);
    return result;
end;
