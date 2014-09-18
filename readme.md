# TimeCode

Using Google Authenticator with FreePascal / Lazarus / possibly other Pascal and Delphi dialects.

## What is this

At this point there's a full implementation of [RFC6238](https://tools.ietf.org/html/rfc6238)-compliant TOTP generation and validation, implemented in FreePascal 2.6 (shipped with Lazarus 1.2.0). This implementation is fully compatible with [Google Authenticator](http://en.wikipedia.org/wiki/Google_Authenticator), [Authy](https://www.authy.com/) and other similar time-based one time password generators, primarily used for two factor authentication.

The library consists of two units, Base32U to decode Base32-encoded strings and GoogleOTP to generate and validate TOTP codes.

## Credits and acknowledgements

This code began as a fork of [DelphiOTP](https://github.com/wendelb/DelphiOTP) which never quite worked for me. In the process I discovered typos which I fixed, a bad UNIX timestamp calculation (and a bug / feature in FreePascal's calculation of the UNIX timestamp) which called for a brand new implementation and unnecessary dependence to Indy 10 which I replaced with pure FreePascal code (hence the backported hmac unit which isn't available in FreePascal 2.6).

## Attention Delphi users

Delphi has switched to Unicode strings after Delphi 2005. You will need to replace String with AnsiString if you plan on using this code with Delphi. Maybe other changes are necessary, who knows? I'm only using FreePascal / Lazarus so your mileage may vary.

## How to generate a TOTP

```Pascal
uses
	GoogleOTP;

[...]

var
	Token: Integer;
begin
	// The current TOTP
	Token := CalculateOTP('MYBASE32SECRET');

	// Or a token for a specific time segment
    // Token := CalculateOTP('MYBASE32SECRET', 1234);

	// Display the token
	WriteLn( AddChar('0', IntToStr(Token), 6) );
end;
```

## How to validate a TOTP

```Pascal
uses
	GoogleOTP;

[...]

var
	Token: Integer;
begin
	// Ask the user for a token
	Token := 123456;

	// Check the Token
	if (ValidateTOPT('MYBASE32SECRET', Token)) then
		ShowMessage('Access granted')
	else
		ShowMessage('Access denied');
end;
```

## What I'd like to do

Ideally I'd like to have a command-line tool which lets you generate TOTPs based on secret keys stored somewhere. That is, if I have the time to write such a tool...