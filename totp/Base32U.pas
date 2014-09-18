unit Base32U;

interface

uses
  SysUtils;  // For UpperCase (Base32Decode)

type
  Base32 = class
  public
    class function Decode(const inString: String): String;
  end;

implementation

const
  ValidChars = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ234567';

function Base32Decode(const source: String): String;
var
  UpperSource: String;
  p, i, l, n, j: Integer;
begin
  UpperSource := UpperCase(source);

  l := Length(source);
  n := 0; j := 0;
  Result := '';

  for i := 1 to l do
  begin
    n := n shl 5; 				// Move buffer left by 5 to make room

    p := Pos(UpperSource[i], ValidChars);
    if p >= 0 then
      n := n + (p - 1);         // Add value into buffer

		j := j + 5;				// Keep track of number of bits in buffer

    if (j >= 8) then
    begin
      j := j - 8;
      Result := Result + chr((n AND ($FF shl j)) shr j);
    end;
  end;
end;

class function Base32.Decode(const inString: String): String;
begin
  Result := Base32Decode(inString);
end;

end.
