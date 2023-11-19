unit MainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, Windows, ShellApi;

type
  TFormUpdate = class(TForm)
    ProgressBar1: TProgressBar;
    tFetchUpdate: TTimer;
    Label1: TLabel;
    btnCancel: TButton;
    procedure btnCancelClick(Sender: TObject);
    procedure tFetchUpdateTimer(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormUpdate: TFormUpdate;

implementation

uses
  UpdateSystem;

{$R *.fmx}

procedure TFormUpdate.btnCancelClick(Sender: TObject);
begin
  Self.Close;
end;

procedure TFormUpdate.FormShow(Sender: TObject);
begin
  tFetchUpdate.Enabled := True;
end;

procedure TFormUpdate.tFetchUpdateTimer(Sender: TObject);
var
  UpdateInfo: TUpdateInfo;
  I: integer;
  Msg: string;
  UpdateTargetDirectory, PackageTargetDirectory: string;
  UpdateResult: boolean;
begin
  ProgressBar1.Value := ProgressBar1.Value + 10;
  if (ProgressBar1.Value >= 100) and (tFetchUpdate.Enabled) and (Self.Visible) then
  begin
    tFetchUpdate.Enabled := False;
    Self.Hide;
    UpdateInfo := UpdateSystem.ParseVersionInfo('TestVersionInfo.txt');
    UpdateInfo := UpdateSystem.ShowUpdateForm(UpdateInfo);

    for I := 0 to High(UpdateInfo.SupportPackages) do begin
      if UpdateInfo.SupportPackages[I].Selected then
        Msg := Msg + UpdateInfo.SupportPackages[I].Name + ',';
    end;

    if UpdateInfo.MainUpdateSelected then
    begin
      if Length(Msg) = 0 then
       ShowMessage('No packages selected')
      else
        ShowMessage('Selected support packages: ' + Msg);

      // Create new directory for the update
      UpdateTargetDirectory := 'C:\ProgramData\StoneNotes\v'
        +IntToStr(UpdateInfo.Major)+'.'
        +IntToStr(UpdateInfo.Minor)+'.'
        +IntToStr(UpdateInfo.Patch)+'.'
        +IntToStr(UpdateInfo.Build);

      if DirectoryExists(UpdateTargetDirectory) then
      begin
        if MessageDlg('A directory for this version already exists. Are you sure you want to continue?',TMsgDlgType.mtWarning, mbYesNo, 0) = mrNo then
        begin
          Self.Close
        end;
      end else begin
        CreateDir(UpdateTargetDirectory);
        CreateDir(UpdateTargetDirectory + '\Support');
      end;

      UpdateResult := false;
      while not UpdateResult do
      begin
        // Download and unzip main package, if there is one for this update
        // and if we are not already running that version
        if '' = UpdateInfo.MainURL then
          UpdateResult := true
        else
          UpdateResult := DownloadAndUnzip(UpdateInfo.MainURL, UpdateTargetDirectory);

        // Download and unzip selected support packages
        if UpdateResult then
          for I := 0 to High(UpdateInfo.SupportPackages) do begin
            if UpdateInfo.SupportPackages[I].Selected then
            begin
              PackageTargetDirectory := UpdateTargetDirectory + '\Support\' + UpdateInfo.SupportPackages[I].Name;

              if not DirectoryExists(PackageTargetDirectory) then
                CreateDir(PackageTargetDirectory);

              UpdateResult := UpdateResult and DownloadAndUnzip(UpdateInfo.SupportPackages[I].URL, PackageTargetDirectory);
            end;
          end;

        if not UpdateResult then
        begin
          if MessageDlg('One or more updates failed to download or install properly. Do you want to retry?', TMsgDlgType.mtWarning, mbYesNo, 0) = mrNo then
          begin
            Self.Close;
            Exit;
          end;
        end;
      end;

      ShowMessage('Update complete!');

      ShellExecute(0, PChar('open'),
                      PChar(UpdateTargetDirectory + '\StoneNotes.exe'), nil,
                      PChar(UpdateTargetDirectory), SW_SHOWNORMAL);

    end else begin
      ShowMessage('Update cancelled');
    end;

    Self.Close;
  end;
end;

end.
