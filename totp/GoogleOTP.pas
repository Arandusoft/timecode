unit GoogleOTP;

{ <
  A simple unit to provide calculation and verification of Time Based One Time
  Passwords (TOTP) compatile with Google Authenticator.

  @author(Nicholas K. Dionysopoulos (https://www.dionysopoulos.me))
  @license(GNU General Public License version 3 or later (GNU GPLv3+))
}

interface

uses
  SysUtils, Math, Base32U, HMAC;

{
  Calculates the TOTP for a given secret and time segment. You need to format the
  integer into six digit representation.

  @param(Secret is the Base32-encoded secret key for TOTP calculation)
  @param(Counter is the 30-second time segment since the start of the UNIX epoch you want to calculate an OTP for)
  @returns(The TOTP as an integer)
}
function CalculateTOTP(const Secret: string; const Counter: integer = -1): integer;

{
  Checks if the provided TOTP (Token) is valid by comparing it against a freshly
  generated TOTP using the Base32-encoded Secret. Since there is time drift
  between devices, application latency and the user taking their time to enter
  the code and submit it to the application we use a time window of +/- WindowSize
  time steps. A WindowSize of -1 means that a Token is accepted if it matches
  the TOTP of the previous time step (30 seconds ago), now and the next time
  step (30 seconds in the future). You are advised not to increase the
  WindowSize over 2. The bigger the WindowSize the easier a replay attack becomes.

  @param(Secret is the Base32-encoded secret key for TOTP calculation)
  @param(Token is the TOTP you want to validate)
  @param(WindowSize is the time window you consider a TOTP valid)
  @return(@true if Token is a valid TOTP, @false otherwise)
}
function ValidateTOTP(const Secret: string; const Token: integer;
  const WindowSize: integer = 1): boolean;

implementation

uses
  Classes, strutils, dateutils;

const
  // How many digits are there in a TOTP. Google Authenticator uses 6 digits.
  otpLength = 6;
  // Time step for TOTP generation. Google Authenticator uses 30 seconds.
  keyRegeneration = 30;

// Casts an Int64 to a raw byte string
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

// Casts a SHA1 digest to a raw byte string. Assumes little-endian processor.
function DigestToRawString(Digest: THMACSHA1Digest): String;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to High(Digest) do
      Result := Result + Chr(Digest[i]);
end;

function CalculateTOTP(const Secret: string; const Counter: integer = -1): integer;
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
      // This part of the code made me lose my will to live. DateTimeToUnix is
      // stupidely unaware of your timezone. What it returns is NOT a UNIX
      // timestamp, it's simply the local time adjusted by a predefined offset.
      // This is where GetLocalTimeOffset comes into play. Which is also a very
      // stupidly behaving function, as it returns the INVERSE time offset. For
      // example, Paris is GMT+1 hour. You'd expect the local time offset to be
      // +60 minutes, right? Nope. It's -60. It tells you how many minutes you
      // need to *add* to a timestamp to make it GMT. This is the exact opposite
      // of what everyone else in the world calls a "timezone offset". ARGH!!!
      UnixTime := DateTimeToUnix(now()) + GetLocalTimeOffset * 60;
      // Now that we have a PROPER UNIX TIMESTAMP let's get the 30 second time
      // segment since the start of the UNIX epoch. Amen!
      Time := UnixTime div keyRegeneration;
    end;

  // Base32-decode the secret key
  BinSecret := Base32.Decode(Secret);

  // Create the HMAC-SHA1 digest of the secret key and the time segment
  Digest := HMACSHA1Digest(BinSecret, Int64ToRawString(Int64(Time)));

  // Convert the byte array of the digest into a raw byte string
  Hash := DigestToRawString(Digest);

  // This is directly translated from the RFC. It is basically a custom function
  // which truncates the HMAC-SHA1 digest, then compresses the result into an
  // integer that has at most otpLength digits.
  Offset := (Ord(Hash[20]) and $0F) + 1;
  Part1 := (Ord(Hash[Offset + 0]) and $7F);
  Part2 := (Ord(Hash[Offset + 1]) and $FF);
  Part3 := (Ord(Hash[Offset + 2]) and $FF);
  Part4 := (Ord(Hash[Offset + 3]) and $FF);

  Key := (Part1 shl 24) or (Part2 shl 16) or (Part3 shl 8) or (Part4);
  Result := Key mod Trunc(IntPower(10, otpLength));
end;

function ValidateTOTP(const Secret: string; const Token: integer;
  const WindowSize: integer = 1): boolean;
var
  TimeStamp: integer;
  TestValue: integer;
begin
  Result := False;

  TimeStamp := (DateTimeToUnix(now()) + GetLocalTimeOffset * 60) div keyRegeneration;
  for TestValue := Timestamp - WindowSize to TimeStamp + WindowSize do
  begin
    if (CalculateTOTP(Secret, TestValue) = Token) then
      Result := True;
  end;
end;

end.
