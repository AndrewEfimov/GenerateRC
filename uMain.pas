{ *********************************************************************
  *
  * Autor: Efimov A.A.
  * E-mail: infocean@gmail.com
  * GitHub: https://github.com/AndrewEfimov
  * Change: 07.11.2019
  *
  ******************************************************************** }
unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts, FMX.Controls.Presentation, FMX.StdCtrls,
  FMX.Edit, FMX.ListBox, FMX.ScrollBox, FMX.Memo, FMX.Effects;

type
  TFormMain = class(TForm)
    gbSource: TGroupBox;
    layMain: TLayout;
    gbOutput: TGroupBox;
    layOptions: TLayout;
    gbFinalNames: TGroupBox;
    gbSettings: TGroupBox;
    StyleBook1: TStyleBook;
    Layout5: TLayout;
    laySaveFile: TLayout;
    sbSaveFile: TSpeedButton;
    edPathToOutputFile: TEdit;
    cbAddToExistingFile: TCheckBox;
    Label1: TLabel;
    Layout2: TLayout;
    rbSelectDir: TRadioButton;
    laySelectDir: TLayout;
    edDirPath: TEdit;
    sbSelectDir: TSpeedButton;
    cbScanSubDirs: TCheckBox;
    Layout3: TLayout;
    rbSelectFiles: TRadioButton;
    laySelectFiles: TLayout;
    sbSelectFiles: TSpeedButton;
    edFilePaths: TEdit;
    Layout4: TLayout;
    Layout7: TLayout;
    Layout8: TLayout;
    Layout6: TLayout;
    rbSaveNames: TRadioButton;
    rbAutoNames: TRadioButton;
    layNameMask: TLayout;
    Label2: TLabel;
    edMaskForNames: TEdit;
    Label3: TLabel;
    Layout9: TLayout;
    Layout10: TLayout;
    Label4: TLabel;
    cboxTypeResource: TComboBox;
    lbiIcon: TListBoxItem;
    lbiBitmap: TListBoxItem;
    lbiCursor: TListBoxItem;
    lbiRcdata: TListBoxItem;
    lbiFont: TListBoxItem;
    cbCreateBatFile: TCheckBox;
    cbCreateResFile: TCheckBox;
    Label5: TLabel;
    layProcess: TLayout;
    sbStartProcess: TSpeedButton;
    mLogs: TMemo;
    layLogs: TLayout;
    ClearEditButton1: TClearEditButton;
    ClearEditButton2: TClearEditButton;
    ClearEditButton3: TClearEditButton;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    ShadowEffect1: TShadowEffect;
    labAbout: TLabel;
    procedure rbSourceChange(Sender: TObject);
    procedure rbFinalNamesChange(Sender: TObject);
    procedure sbSelectDirClick(Sender: TObject);
    procedure sbSelectFilesClick(Sender: TObject);
    procedure sbSaveFileClick(Sender: TObject);
    procedure sbStartProcessClick(Sender: TObject);
    procedure ClearEditButtonClick(Sender: TObject);
    procedure labAboutClick(Sender: TObject);
  private
    { Private declarations }
    FFileList: TArray<string>;
    procedure AddLog(const AValue: string; const AClearLog: Boolean = False);
    procedure ScanDirectoryProcess(const ADirPath: string; const ASearchInSubDirs: Boolean = False);
    function SelectedFilesToStr(const AFiles: TArray<string>): string;
    procedure DataProcessing;
    procedure ClearFileList;
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}

uses
  System.IOUtils, FMX.DialogService.Sync, Winapi.ShellAPI;

procedure TFormMain.AddLog(const AValue: string; const AClearLog: Boolean);
const
  FormatStr = '%s: %s';
begin
  if AClearLog then
  begin
    mLogs.Lines.Clear;
    // FIX BUG: https://quality.embarcadero.com/browse/RSP-12137
    mLogs.ContentBounds := TRectF.Empty;
  end;

  mLogs.Lines.Add(Format(FormatStr, [TimeToStr(Now), AValue]));
  mLogs.ScrollTo(0, mLogs.ContentBounds.Height);
end;

procedure TFormMain.ClearEditButtonClick(Sender: TObject);
begin
  ClearFileList;
end;

procedure TFormMain.ClearFileList;
begin
  if Length(FFileList) > 0 then
    SetLength(FFileList, 0);
end;

procedure TFormMain.DataProcessing;
const
  StrAutoName = '%s%d %s "%s"';
  StrName = '%s %s "%s"';
  BatFileName = 'CreateResFile.bat';
  StrForBatFile = 'brcc32.exe %s%spause';
  InfoProcessStart = 'Началась обработка данных...';
  InfoProcessFinish = 'Обработка данных завершена.';
  ErrorMessage = 'Возникла ошибка: %s';
var
  PathToOutputFile, MaskForNames, TypeResource: string;
  AutoNames, AddToExistingFile: Boolean;
  ResultFileList: TArray<string>;
  I, J: Integer;
begin
  PathToOutputFile := edPathToOutputFile.Text.Trim;
  AutoNames := rbAutoNames.IsChecked;
  MaskForNames := edMaskForNames.Text.Trim;
  TypeResource := cboxTypeResource.Selected.Text;
  AddToExistingFile := cbAddToExistingFile.IsChecked;

  AddLog(InfoProcessStart);
  try
    SetLength(ResultFileList, Length(FFileList));

    if AutoNames then
    begin
      if MaskForNames.IsEmpty then
        MaskForNames := 'file_';

      for I := Low(FFileList) to High(FFileList) do
        ResultFileList[I] := Format(StrAutoName, [MaskForNames, I, TypeResource, FFileList[I]]);
    end
    else
      for I := Low(FFileList) to High(FFileList) do
        ResultFileList[I] := Format(StrName, [TPath.GetFileNameWithoutExtension(FFileList[I]), TypeResource,
          FFileList[I]]);

    if AddToExistingFile then
      for J := Low(ResultFileList) to High(ResultFileList) do
        // Указание кодировки TEncoding.ANSI нужно для brcc32.exe
        TFile.AppendAllText(PathToOutputFile, ResultFileList[J] + SLineBreak, TEncoding.ANSI)
    else
      TFile.WriteAllLines(PathToOutputFile, ResultFileList, TEncoding.ANSI);

    if cbCreateBatFile.IsChecked then
      TFile.WriteAllText(TPath.Combine(TPath.GetDirectoryName(PathToOutputFile), BatFileName),
        Format(StrForBatFile, [TPath.GetFileName(PathToOutputFile), SLineBreak]));

    if cbCreateResFile.IsChecked then
      ShellExecute(0, 'open', 'cmd.exe', PWideChar('/c brcc32.exe ' + PathToOutputFile), nil, 1);
    // ShellExecute(0, 'open' , PWideChar(TPath.Combine(TPath.GetDirectoryName(PathToOutputFile), BatFileName)), nil, nil, 1);

    AddLog(InfoProcessFinish);
  except
    on E: Exception do
      AddLog(Format(ErrorMessage, [E.Message]));
  end;
end;

procedure TFormMain.labAboutClick(Sender: TObject);
const
  UrlGitHub = 'https://github.com/AndrewEfimov/GenerateRC';
begin
  ShellExecute(0, 'open', PChar(UrlGitHub), nil, nil, 1);
end;

procedure TFormMain.rbFinalNamesChange(Sender: TObject);
begin
  layNameMask.Enabled := rbAutoNames.IsChecked;
end;

procedure TFormMain.rbSourceChange(Sender: TObject);
begin
  ShadowEffect1.Enabled := False;
  laySelectDir.Enabled := rbSelectDir.IsChecked;
  laySelectFiles.Enabled := rbSelectFiles.IsChecked;

  edDirPath.Text := string.Empty;
  edFilePaths.Text := string.Empty;

  ClearFileList;
end;

procedure TFormMain.sbSaveFileClick(Sender: TObject);
const
  MessageForDialog = 'Такой файл существует. Добавить записи в него?';
begin
  if SaveDialog1.Execute then
  begin
    ShadowEffect1.Enabled := False;
    if TFile.Exists(SaveDialog1.FileName) then
    begin
      cbAddToExistingFile.Enabled := True;
      cbAddToExistingFile.IsChecked := TDialogServiceSync.MessageDialog(MessageForDialog, TMsgDlgType.mtConfirmation,
        mbYesNo, TMsgDlgBtn.mbNo, 0) = mrYes;
    end
    else
      cbAddToExistingFile.Enabled := False;

    edPathToOutputFile.Text := SaveDialog1.FileName;
  end;
end;

procedure TFormMain.sbSelectDirClick(Sender: TObject);
const
  TitleSelectDirectory = 'Выберите папку...';
  InfoSelectFile = 'Режим автоматического выбора файлов';
var
  DirPath: string;
begin
  // ExtractFilePath(ParamStr(0));
  if SelectDirectory(TitleSelectDirectory, '', DirPath) then
  begin
    ShadowEffect1.Enabled := False;
    AddLog(InfoSelectFile, True);
    edDirPath.Text := DirPath;
    ScanDirectoryProcess(DirPath, cbScanSubDirs.IsChecked);
  end;
end;

procedure TFormMain.sbSelectFilesClick(Sender: TObject);
const
  InfoSelectFile = 'Режим ручного выбора файлов';
  InfoNumberOfFiles = 'Файлов выбрано: %d';
begin
  if OpenDialog1.Execute then
  begin
    ShadowEffect1.Enabled := False;
    AddLog(InfoSelectFile, True);

    FFileList := OpenDialog1.Files.ToStringArray;

    AddLog(Format(InfoNumberOfFiles, [Length(FFileList)]));

    edFilePaths.Text := SelectedFilesToStr(FFileList);
  end;
end;

procedure TFormMain.sbStartProcessClick(Sender: TObject);
const
  InfoLengthFileList = 'Выберите файлы';
  InfoPathToOutputFile = 'Укажите файл для сохранения';
begin
  ShadowEffect1.Enabled := False;
  if not(Length(FFileList) > 0) then
  begin
    if rbSelectDir.IsChecked then
      ShadowEffect1.Parent := edDirPath
    else
      ShadowEffect1.Parent := edFilePaths;

    ShadowEffect1.Enabled := True;
    AddLog(InfoLengthFileList);
  end
  else if edPathToOutputFile.Text.IsEmpty then
  begin
    ShadowEffect1.Parent := edPathToOutputFile;
    ShadowEffect1.Enabled := True;
    AddLog(InfoPathToOutputFile);
  end
  else
    DataProcessing;
end;

procedure TFormMain.ScanDirectoryProcess(const ADirPath: string; const ASearchInSubDirs: Boolean);
const
  MaskForFileNames = '*';
  InfoAllDirectories = 'Сканирование включая подкаталоги...';
  InfoTopDirectoryOnly = 'Сканирование без подкаталогов...';
  InfoNumberOfFiles = 'Файлов найдено: %d';
  ErrorMessage = 'Возникла ошибка: %s';
var
  SearchOption: TSearchOption;
begin
  if ASearchInSubDirs then
  begin
    AddLog(InfoAllDirectories);
    SearchOption := TSearchOption.soAllDirectories;
  end
  else
  begin
    AddLog(InfoTopDirectoryOnly);
    SearchOption := TSearchOption.soTopDirectoryOnly;
  end;

  try
    FFileList := TDirectory.GetFiles(ADirPath, MaskForFileNames, SearchOption);
    AddLog(Format(InfoNumberOfFiles, [Length(FFileList)]));
  except
    on E: Exception do
      AddLog(Format(ErrorMessage, [E.Message]));
  end;
end;

function TFormMain.SelectedFilesToStr(const AFiles: TArray<string>): string;
const
  FormatStr = '%s; %s';
var
  SelectedFilesStr, FileName: string;
  I: Integer;
begin
  SelectedFilesStr := '';

  for I := Low(AFiles) to High(AFiles) do
  begin
    FileName := TPath.GetFileName(AFiles[I]);

    if not SelectedFilesStr.IsEmpty then
      SelectedFilesStr := Format(FormatStr, [SelectedFilesStr, FileName])
    else
      SelectedFilesStr := FileName;
  end;

  Result := SelectedFilesStr;
end;

end.
