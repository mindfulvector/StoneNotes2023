program StoneUpdate;

uses
  System.StartUpCopy,
  FMX.Forms,
  MainForm in 'MainForm.pas' {FormUpdate},
  UpdateSystem in 'UpdateSystem.pas',
  TreeNodeCheckboxClick in 'TreeNodeCheckboxClick.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormUpdate, FormUpdate);
  Application.Run;
end.
