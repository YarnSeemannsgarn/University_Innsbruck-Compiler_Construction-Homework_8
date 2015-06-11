
{ This program implements some well-known algorithms }


program minipas;

  var x, y, i : integer;      
      a	      : array [1..100] of real;
     a2	      : array [-1..-100] of real; { Error for d) }
      i	      : boolean; { Error for b) }

  begin
     
     { Errors }
     c := true; { Error for a) }
     a[true] := 1.0; { Error for c) }
     a[3.14] := 1.0; { Error for c) }
     x := true; { Error for e) }
     a := y; { No error for e) }
     y := a; { Error for e) }
     if 3.14 and 2.0 then write("Error"); { Error for f) }
     if 4 - 3 then write("Error"); { Error for g) }
  end.
