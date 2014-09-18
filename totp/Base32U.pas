unit Base32U;

{ <
  A simple Base32 decoder

  @author(Nicholas K. Dionysopoulos (https://www.dionysopoulos.me))
  @license(GNU General Public License version 3 or later (GNU GPLv3+))
}

interface

uses
  SysUtils;  // For UpperCase (Base32Decode)

type
  Base32 = class
  public
    // Decodes a Base32-encoded string to a raw bytes string
    class function Decode(const inString: string): string;
  end;

implementation

const
  // Base32 character set
  ValidChars = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ234567';

function Base32Decode(const Source: string): string;
var
  UpperSource: string;
  p, i, l, n, j: integer;
begin
  UpperSource := UpperCase(Source);

  l := Length(Source);
  n := 0;
  j := 0;
  Result := '';

  for i := 1 to l do
  begin
    // Move buffer left by 5 to make room. Base32 means that each character in
    // the encoded string can have one of 32 values, i.e. it encodes 5 bits
    // (2 ^ 5 = 32)
    n := n shl 5;

    // Convert the character to its debased integer representation
    p := Pos(UpperSource[i], ValidChars);

    // Only consider valid characters
    if p >= 0 then
    begin
      // Add value into buffer
      n := n + (p - 1);
      // Keep track of number of bits in buffer
      j := j + 5;
    end;

    // If we have already gotten a byte, output it
    if (j >= 8) then
    begin
      j := j - 8;
      Result := Result + chr((n and ($FF shl j)) shr j);
    end;
  end;
end;

class function Base32.Decode(const inString: string): string;
begin
  Result := Base32Decode(inString);
end;

end.
