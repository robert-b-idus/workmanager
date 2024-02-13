{$HINTS OFF}
{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{ Copyright(c) 2013-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit Androidapi.JNI.Idus;

interface

uses
  Androidapi.JNIBridge,
  Androidapi.JNI.AdMob,
  Androidapi.JNI.App,
  Androidapi.JNI.Bluetooth,
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.Hardware,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Location,
  Androidapi.JNI.Net,
  Androidapi.JNI.Os,
  Androidapi.JNI.PlayServices.Maps,
  Androidapi.JNI.RenderScript,
  Androidapi.JNI.Telephony,
  Androidapi.JNI.Util,
  Androidapi.JNI.Webkit,
  Androidapi.JNI.Widget;

type
  JIdusWorkManagerBridge = interface;
  JIdusWorkerBeginWorkClass = interface(IJavaClass)
    ['{701F0E95-1A40-4D7C-9694-F05779BD4923}']
  end;

  [JavaSignature('se/idus/iwm/IdusWorkerBeginWork')]
  JIdusWorkerBeginWork = interface(IJavaInstance)
    ['{DA5EC5DE-0FEC-42BF-9703-668A97680498}']
    function beginWork: Boolean; cdecl;
  end;

  TJIdusWorkerBeginWork = class(TJavaGenericImport<JIdusWorkerBeginWorkClass, JIdusWorkerBeginWork>) end;

  JIdusWorkerStopWorkClass = interface(IJavaClass)
    ['{701F0E95-1A40-4D7C-9694-F05779BD4923}']
  end;

  [JavaSignature('se/idus/iwm/IdusWorkerStopWork')]
  JIdusWorkerStopWork = interface(IJavaInstance)
    ['{3A9C569F-6945-4946-AD5B-0224AF2B1822}']
    procedure stopWork; cdecl;
  end;

  TJIdusWorkerStopWork = class(TJavaGenericImport<JIdusWorkerBeginWorkClass, JIdusWorkerStopWork>) end;

  JIdusWorkManagerBridgeClass = interface(IJavaClass)
    ['{7DFFF616-58BB-4F5D-8A13-075283C0C540}']
    function getInstance : JIdusWorkManagerBridge; cdecl;
  end;

  [JavaSignature('se/idus/iwm/IdusWorkManagerBridge')]
  JIdusWorkManagerBridge = interface(IJavaInstance)
    ['{9AB3AE36-6A34-427C-A349-4F1D2C7C540E}']
    procedure scheduleWork(); cdecl;
    procedure setOnBeginWork(AOnBeginWork : JIdusWorkerBeginWork ); cdecl;
    procedure setOnStopWork(AOnStopWork : JIdusWorkerStopWork ); cdecl;
  end;
  TJIdusWorkManagerBridge = class(TJavaGenericImport<JIdusWorkManagerBridgeClass, JIdusWorkManagerBridge>) end;

  type
    TBeginWorkFunc = function : Boolean of object;
    TStopWorkProc = procedure of object;
    TIdusStartWork = class(TJavaLocal, JIdusWorkerBeginWork)
    private
      FOnBeginWork : TBeginWorkFunc;
    public
      function beginWork: Boolean; cdecl;
      property OnBeginWork : TBeginWorkFunc read FOnBeginWork write FOnBeginWork;
    end;
    TIdusStopWork = class(TJavaLocal, JIdusWorkerStopWork)
    private
      FStopWorkProc : TStopWorkProc;
    public
      procedure stopWork; cdecl;
      property OnStopWork : TStopWorkProc read FStopWorkProc write FStopWorkProc;
    end;

implementation

  function TIdusStartWork.beginWork;
  begin
    result := OnBeginWork()
  end;

  procedure TIdusStopWork.stopWork;
  begin
    OnStopWork();
  end;

procedure RegisterTypes;
begin
//  TRegTypes.RegisterType('Androidapi.JNI.Idus.JFMXNativeActivity', TypeInfo(Androidapi.JNI.Idus.JFMXNativeActivity));
end;

initialization
  RegisterTypes;
end.


