program timecode;

{$mode objfpc}{$H+}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Classes,
  SysUtils,
  CustApp,
  { you can add units after this }
  strutils, GoogleOTP;

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

  { TTimeCode }

  procedure TTimeCode.DoRun;
  var
    ErrorMsg: string;
    killMe: string;
  begin
    // quick check parameters
    ErrorMsg := CheckOptions('h', 'help');
    if ErrorMsg <> '' then
    begin
      ShowException(Exception.Create(ErrorMsg));
      Terminate;
      Exit;
    end;

    // parse parameters
    if HasOption('h', 'help') then
    begin
      WriteHelp;
      Terminate;
      Exit;
    end;

    { add your program here }

    WriteLn( AddChar('0', IntToStr(CalculateTOTP('AAAAAAAAAAAAAAAA')), 6) );

    Readln;

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
    writeln('Usage: ', ExeName, ' -h');
  end;

var
  Application: TTimeCode;
begin
  Application := TTimeCode.Create(nil);
  Application.Title := 'TimeCode';
  Application.Run;
  Application.Free;
end.
