unit wmTestMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.StdCtrls,AndroidApi.Jni.Idus,
  FMX.Platform, FMX.Edit, System.SyncObjs;

type
  TLogProc = procedure (const AMsg : String ) of object;
  TTaskThread = class(TThread)
    private
      FResult : Boolean;
      FStop : Boolean;
      FOnLog : TLogProc;
    FEvent: TEvent;
      procedure Log(const AMsg : String);
    procedure SetEvent(const Value: TEvent);
    protected
      procedure Execute; override;
    public
      constructor Create;
      property Result : Boolean read FResult;
      property OnLog : TLogProc read FOnLog write FonLog;
      property StopWork : boolean read FStop write FStop;
      property Event : TEvent read FEvent write SetEvent;
  end;
  TForm3 = class(TForm)
    Memo1: TMemo;
    ToolBar1: TToolBar;
    ToolBar2: TToolBar;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    FTaskThread : TTaskThread;
    FCallbacksSetup : Boolean;
    FOnStart : TIdusStartWork;
    FOnStop : TIdusStopWork;
  protected
    procedure Log(const AMsg : String );
    procedure ScheduleWork;
    procedure CancelWork(const ATag : String);
    function AppEvent(AAppEvent: TApplicationEvent; AContext: TObject): Boolean;
  public
    { Public declarations }
    function OnStartWork(const ATag : String ) : Boolean;
    procedure OnStopWork(const ATag : String );
  end;

var
  Form3: TForm3;

implementation


uses
  AndroidApi.Helpers;

var
  gMainThreadId : TThreadId;
  FAppState : TApplicationEvent;


{$R *.fmx}

function TForm3.AppEvent(AAppEvent: TApplicationEvent;
  AContext: TObject): Boolean;
begin
  FAppState := AAppEvent;
  case AAppEvent of
    TApplicationEvent.FinishedLaunching: Log('Finished launching');
    TApplicationEvent.BecameActive: begin
       CancelWork('background_sync');
       Log('Became active');
    end;
    TApplicationEvent.WillBecomeInactive: begin
      Log('Will become inactive - scheduling work');
      ScheduleWork;
    end;
    TApplicationEvent.EnteredBackground: Log('Entered background');
    TApplicationEvent.WillBecomeForeground: Log('Will become foreground');
    TApplicationEvent.WillTerminate: Log('Will terminate');
    TApplicationEvent.LowMemory: Log('Low memory');
    TApplicationEvent.TimeChange: Log('Time change');
    TApplicationEvent.OpenURL: Log('OpenURL');
  end;
end;

procedure TForm3.CancelWork(const ATag: String);
var
  LJNI : JIdusWorkManagerBridge;
begin
  Log('Cancelling work for tag ' + ATag );
  LJNI := TJIdusWorkManagerBridge.JavaClass.getInstance();
  LJNI.cancelWork( StringToJString(ATag) );
  LJNI := nil;
end;

procedure TForm3.FormCreate(Sender: TObject);
var
  LFMXAservice : IFMXApplicationEventService;
  LJNI : JIdusWorkManagerBridge;
begin
  gMainThreadId := TThread.CurrentThread.ThreadID;
  FOnStart := TIdusStartWork.Create;
  FOnStop := TIdusStopWork.Create;
  FOnStart.OnBeginWork := OnStartWork;
  FOnStop.OnStopWork := OnStopWork;
  FCallbacksSetup := false;


  LJNI := TJIdusWorkManagerBridge.JavaClass.getInstance();
  LJNI.setOnBeginWork( FOnStart );
  LJNI.setOnStopWork( FOnStop );

  if TPlatformServices.Current.SupportsPlatformService(IFMXApplicationEventService, LFMXAService ) then
  begin
    LFMXAService.SetApplicationEventHandler( AppEvent )
  end;

end;

procedure TForm3.Log(const AMsg: String);
var
  LThreadId : TThreadId;
begin
  LThreadId := TThread.CurrentThread.ThreadID;
  TThread.ForceQueue(nil,
    procedure
    begin
      Memo1.Lines.Add(Format('[%x]-%s %s', [ LThreadID, FormatDateTime('hh:nn',now) , Amsg]));
    end
  );
end;

function TForm3.OnStartWork(const ATag : String ): Boolean;
var
  wr : TWaitResult;
  I : Integer;
begin
  Log(Format('OnStartWork called for job "%s"', [ATag] ));
  if TThread.CurrentThread.ThreadID = gMainThreadID then
  begin
    Log('Called in main thread, aborting');
    Sleep(100);
    exit(True);
  end;

  FTaskThread := TTaskThread.Create;
  FTaskThread.OnLog := Log;
  FTaskThread.Start;


  repeat
    wr := FTaskThread.Event.WaitFor(100);
    case wr of
      wrSignaled: Log('Signalled');
      wrTimeout: begin
        if (I mod 100) = 0 then
          Log('.');
        Inc(I);
      end;
      wrAbandoned: Log('Abandoned');
      wrError: Log('Error');
      wrIOCompletion: Log('IOCompletion');
    end;
  until wr = wrSignaled;

  Log('signalled: Work completed');

  result := FTaskThread.Result;
end;

procedure TForm3.OnStopWork(const ATag : string );
begin
  Log(Format('OnStopWork called for job "%s"', [ATag] ));
  if Assigned(FTaskThread) then
    FTaskThread.StopWork := true;
end;

procedure TForm3.ScheduleWork;
var
  LJNI : JIdusWorkManagerBridge;
begin
  LJNI := TJIdusWorkManagerBridge.JavaClass.getInstance();
  LJNI.scheduleWork( StringToJstring('background_sync') , StringtoJString('connected'), 15 );
  LJNI := nil;
end;

{ TTaskThread }

constructor TTaskThread.Create;
begin
  inherited Create(true);
  FEvent := TEvent.Create;
  FEvent.ResetEvent;
end;

procedure TTaskThread.Execute;
var
  I: Integer;
begin
  Log('Work start');
  try
    for I := 0 to 80 do
      begin
        if (i mod 17) = 0 then
          Log('Work');

          (*
          if FAppState in [ TApplicationEvent.BecameActive, TApplicationEvent.WillBecomeForeground ] then
          begin
            Log('App became active, aborting');
            FResult:= true;
            exit;
          end;
          *)

        sleep(800);
        if FStop then
          break;
      end;

    if not FStop then
      FResult := true
    else
      FResult := false;
  finally
    FEvent.SetEvent;
  end;

end;

procedure TTaskThread.Log(const AMsg: String);
begin
  if Assigned(FOnLog) then
  begin
    var d : TDateTime := now;
    var tid := TThread.CurrentThread.ThreadID;
    TThread.ForceQueue(nil,
    procedure
    begin
      FOnLog(Format('[%x]-%s: %s', [tid,FormatDateTime('hh:nn',d), AMsg] ));
    end
    )
  end;
end;

procedure TTaskThread.SetEvent(const Value: TEvent);
begin
  FEvent := Value;
end;

end.
