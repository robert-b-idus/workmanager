program wmTest;

uses
  System.StartUpCopy,
  FMX.Forms,
  wmTestMain in 'wmTestMain.pas' {Form3},
  Androidapi.JNI.Idus in 'Androidapi.JNI.Idus.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm3, Form3);
  Application.Run;
end.
