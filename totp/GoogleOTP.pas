unit GoogleOTP;

interface

uses
  SysUtils, Math, Base32U, HMAC;

(*

Test Case for the CalculateOTP function
---------------------------------------

Init key: AAAAAAAAAAAAAAAAAAAA
Timestamp: 1
BinCounter: 0000000000000001 (HEX-Representation)
Hash: eeb00b0bcc864679ff2d8dd30bec495cb5f2ee9e (HEX-Representation)
Offset: 14
Part 1: 73
Part 2: 92
Part 3: 181
Part 4: 242
One time password: 812658

Easy Display: Format('%.6d', [CalculateOTP(SECRET)]);
*)

function CalculateOTP(const Secret: string; const Counter: integer = -1): integer;
function ValidateTOPT(const Secret: string; const Token: integer;
  const WindowSize: integer = 4): boolean;

implementation

uses
  Classes, strutils, dateutils;

const
  otpLength = 6;
  keyRegeneration = 30;

function Int64ToRawString(I: Int64): string;
var
  B: Array[0..7] of Byte;
  k: Integer;
begin
  PInt64(@B)^ := I;

  Result := '';

  for k := 7 downto 0 do
  begin
    Result := Result + Chr(B[k]);
  end;
end;

function DigestToRawString(Digest: THMACSHA1Digest): String;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to High(Digest) do
      Result := Result + Chr(Digest[i]);
end;

function StrToIdBytes(const inInteger: int64): TBytes;
var
  ch: char;
  i: integer;
begin
  SetLength(Result, 16);
  PInt64(@Result)^ := inInteger;
end;

function CalculateOTP(const Secret: string; const Counter: integer = -1): integer;
var
  BinSecret: string;
  Hash: string;
  Offset: integer;
  Part1, Part2, Part3, Part4: integer;
  Key: integer;
  Time: integer;
  UnixTime: Int64;
  Digest : THMACSHA1Digest;
  TZOffset: Integer;
begin

  if Counter <> -1 then
    Time := Counter
  else
    begin
      UnixTime := DateTimeToUnix(now()) + GetLocalTimeOffset * 60;
      Time := UnixTime div keyRegeneration;
    end;

  BinSecret := Base32.Decode(Secret);

  Digest := HMACSHA1Digest(BinSecret, Int64ToRawString(Int64(Time)));

  Hash := DigestToRawString(Digest);

  Offset := (Ord(Hash[20]) and $0F) + 1;
  Part1 := (Ord(Hash[Offset + 0]) and $7F);
  Part2 := (Ord(Hash[Offset + 1]) and $FF);
  Part3 := (Ord(Hash[Offset + 2]) and $FF);
  Part4 := (Ord(Hash[Offset + 3]) and $FF);

  Key := (Part1 shl 24) or (Part2 shl 16) or (Part3 shl 8) or (Part4);
  Result := Key mod Trunc(IntPower(10, otpLength));
end;

function ValidateTOPT(const Secret: string; const Token: integer;
  const WindowSize: integer = 4): boolean;
var
  TimeStamp: integer;
  TestValue: integer;
begin
  Result := False;

  TimeStamp := (DateTimeToUnix(now()) + GetLocalTimeOffset * 60) div keyRegeneration;
  for TestValue := Timestamp - WindowSize to TimeStamp + WindowSize do
  begin
    if (CalculateOTP(Secret, TestValue) = Token) then
      Result := True;
  end;
end;

end.
