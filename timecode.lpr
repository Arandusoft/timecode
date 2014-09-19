program timecode;

{
TimeCode - Command line TOTP generator compatible with Google Authenticator
Copyright (C) 2014  Nicholas K. Dionysopoulos / Akeeba Ltd

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
}

{$mode objfpc}{$H+}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Classes,
  SysUtils,
  CustApp,
  { you can add units after this }
  strutils,
  GoogleOTP;

type

  { TTimeCode }

  TTimeCode = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

  TCommand = (Show, Add, Delete);

  { TTimeCode }

  procedure TTimeCode.DoRun;
  var
    ErrorMsg: string;
    password: string;
    command: TCommand;
    subjectName: string;
    key: string;
  begin
    WriteLn('TimeCode   Copyright (C) 2014  Akeeba Ltd');
    WriteLn('');
    WriteLn('This program is free software: you can redistribute it and/or modify');
    WriteLn('it under the terms of the GNU General Public License as published by');
    WriteLn('the Free Software Foundation, either version 3 of the License, or');
    WriteLn('(at your option) any later version.');
    WriteLn('');
    // quick check parameters
    ErrorMsg := CheckOptions('h', 'help');

    if ErrorMsg <> '' then
    begin
      ShowException(Exception.Create(ErrorMsg));
      Terminate;
      Exit;
    end;

    // Show help message if requested or no password was specified
    if HasOption('h', 'help') or not HasOption('p', 'password') then
    begin
      WriteHelp;
      Terminate;
      Exit;
    end;

    // Get the password
    password := GetOptionValue('p', 'password');

    // Which command should I use?
    if HasOption('a', 'add') then
      command := Add
    else if HasOption('d', 'delete') then
      command := Delete
    else
      command := Show;

    // Do I have a name (required for add delete)
    subjectName := '';
    if HasOption('n', 'name') then
       subjectName := GetOptionValue('n', 'name');

    if (subjectName = '') and (command in [Add, Delete]) then
    begin
      WriteHelp;
      Terminate;
      Exit;
    end;

    // Do I have a key (required for add)
    key := '';
    If HasOption('k', 'key') then
       key := GetOptionValue('k', 'key');

    if (key = '') and (command = Add) then
    begin
      WriteHelp;
      Terminate;
      Exit;
    end;

    // TODO: Load from storage

    // TODO: Execute command
    WriteLn(AddChar('0', IntToStr(CalculateTOTP('AAAAAAAAAAAAAAAA')), 6));

    // stop program loop
    Terminate;
  end;

  constructor TTimeCode.Create(TheOwner: TComponent);
  begin
    inherited Create(TheOwner);
    StopOnException := True;
  end;

  destructor TTimeCode.Destroy;
  begin
    inherited Destroy;
  end;

  procedure TTimeCode.WriteHelp;
  begin
    { add your help code here }
    writeln('Usage: ', ExeName, ' [options] [command]');
    writeln('');
    writeln('Options:');
    writeln(' -h --help      Show this help message');
    writeln(' -p --password  Settings encryption password (required)');
    writeln(' -n --name      TOTP subject name (for -a and -d only)');
    writeln(' -k --key       Secret key (for -a only)');
    writeln('');
    writeln('Commands');
    writeln('');
    writeln(' -s --show      Show current TOTPs. Default.');
    writeln(' -a --add       Add an OTP subject');
    writeln(' -d --delete    Delete an OTP subject');
  end;

var
  Application: TTimeCode;

{$R *.res}

begin
  Application := TTimeCode.Create(nil);
  Application.Title := 'TimeCode';
  Application.Run;
  Application.Free;
end.
