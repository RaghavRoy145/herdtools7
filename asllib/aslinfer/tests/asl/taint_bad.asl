func TaintSource() => integer
begin
    return 0;
end;

func TaintSink(x : integer) => integer
begin
    return x;
end;

func TaintBad() => integer
begin
    let tainted : integer = TaintSource();
    let result : integer = TaintSink(tainted);
    return result;
end;

func TaintGood() => integer
begin
    let clean : integer = 42;
    let result : integer = TaintSink(clean);
    return result;
end;
