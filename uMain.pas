{ *********************************************************************
  *
  * Autor: Efimov A.A.
  * E-mail: infocean@gmail.com
  *
  ******************************************************************** }

unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.ListBox, FMX.StdCtrls,
  FMX.ScrollBox, FMX.Memo, FMX.Edit, FMX.Layouts, FMX.Controls.Presentation,
  FMX.Dialogs, FMX.Objects, FMX.Ani, FMX.DialogService.Sync, System.IOUtils, Winapi.ShellAPI, System.Generics.Collections;

type
  TFormMain = class(TForm)
    StyleBook1: TStyleBook;
    ePathFolder: TEdit;
    ePathFiles: TEdit;
    Layout1: TLayout;
    laySelectFolder: TLayout;
    Layout3: TLayout;
    SaveDialog1: TSaveDialog;
    sbSelectFolder: TSpeedButton;
    sbSelectFiles: TSpeedButton;
    cbScanSubFolders: TCheckBox;
    rbSelectFolder: TRadioButton;
    rbSelectFiles: TRadioButton;
    laySelectFiles: TLayout;
    Layout5: TLayout;
    Layout6: TLayout;
    sbSaveFile: TSpeedButton;
    ePathSaveFile: TEdit;
    Label1: TLabel;
    gpFilesName: TGroupBox;
    rbChangeNameSave: TRadioButton;
    rbChangeNameAuto: TRadioButton;
    eTemplateName: TEdit;
    Label2: TLabel;
    layChangeName: TLayout;
    Label3: TLabel;
    gbSettings: TGroupBox;
    Layout8: TLayout;
    Label4: TLabel;
    combTypeResource: TComboBox;
    lbiIcon: TListBoxItem;
    lbiBitmap: TListBoxItem;
    lbiCursor: TListBoxItem;
    lbiRcdata: TListBoxItem;
    lbiFont: TListBoxItem;
    cbCreateBatFile: TCheckBox;
    cbCreateResFile: TCheckBox;
    Layout9: TLayout;
    cbAddToTheOldFile: TCheckBox;
    gbSourceFiles: TGroupBox;
    gbReadyFile: TGroupBox;
    OpenDialog1: TOpenDialog;
    mLogs: TMemo;
    bCreateFile: TButton;
    laySettings: TLayout;
    AniIndicator1: TAniIndicator;
    Layout11: TLayout;
    layAni: TLayout;
    Label5: TLabel;
    FloatAnimation1: TFloatAnimation;
    Label6: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure ChangeFolderAndFile(Sender: TObject);
    procedure ChangeTypeName(Sender: TObject);
    procedure sbSelectFolderClick(Sender: TObject);
    procedure sbSelectFilesClick(Sender: TObject);
    procedure sbSaveFileClick(Sender: TObject);
    procedure bCreateFileClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure cbScanSubFoldersChange(Sender: TObject);
    procedure Label6Click(Sender: TObject);
  private
    { Private declarations }
    FFiles: TStringDynArray;
    FScanningProcess: Boolean;
    procedure ScanFolder(PathFolder: string);
    procedure OnOffbCreateFile;
    procedure ProcessCreateFile;
    procedure AddLog(value: string);
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}

procedure TFormMain.AddLog(value: string);
begin
  mLogs.Lines.Add(Format('%s: %s', [TimeToStr(Now), value]));
  mLogs.ScrollTo(0, mLogs.ContentBounds.Height);
end;

procedure TFormMain.bCreateFileClick(Sender: TObject);
begin
  bCreateFile.Enabled := False;
  TThread.CreateAnonymousThread(ProcessCreateFile).Start;
end;

procedure TFormMain.cbScanSubFoldersChange(Sender: TObject);
begin
  if not ePathFolder.Text.IsEmpty then
    ScanFolder(ePathFolder.Text);
end;

procedure TFormMain.ChangeFolderAndFile;
begin
  laySelectFolder.Enabled := rbSelectFolder.IsChecked;
  laySelectFiles.Enabled := not rbSelectFolder.IsChecked;

  if rbSelectFolder.IsChecked then
    ePathFiles.Text := ''
  else
    ePathFolder.Text := '';

  OnOffbCreateFile;
end;

procedure TFormMain.ChangeTypeName;
begin
  layChangeName.Enabled := rbChangeNameAuto.IsChecked;
end;

procedure TFormMain.ProcessCreateFile;
var
  I, J: Integer;
  TemplateName: string;
  FinishArrayFiles: TStringDynArray;
  SavePath, TypeResource: string;
  NameAuto, AddToTheOldFile: Boolean;
begin
  try
    SavePath := ePathSaveFile.Text.Trim;
    NameAuto := rbChangeNameAuto.IsChecked;
    TemplateName := eTemplateName.Text.Trim;
    TypeResource := combTypeResource.Selected.Text;
    AddToTheOldFile := cbAddToTheOldFile.IsChecked;

    TThread.Synchronize(nil,
      procedure
      begin
        gbSourceFiles.Enabled := False;
        gbReadyFile.Enabled := False;

        // Анимация
        layAni.Visible := True;
        AniIndicator1.Enabled := True;

        laySettings.Enabled := True;

        AddLog('Началась обработка данных...');
      end);

    // Обрабатываем исходный массив с данными
    SetLength(FinishArrayFiles, Length(FFiles));

    if NameAuto then
    begin
      if TemplateName.IsEmpty then
        TemplateName := 'file_';

      for I := 0 to Length(FFiles) - 1 do
        FinishArrayFiles[I] := Format('%s%d %s "%s"', [TemplateName, I+1, TypeResource, FFiles[I]]);
    end
    else
      for I := 0 to Length(FFiles) - 1 do
        FinishArrayFiles[I] := Format('%s %s "%s"', [TPath.GetFileNameWithoutExtension(FFiles[I]), TypeResource,
          FFiles[I]]);

    TArray.Sort<string>(FinishArrayFiles);

    if Length(FinishArrayFiles) > 0 then
      if AddToTheOldFile AND TFile.Exists(SavePath) then
        for J := 0 to Length(FinishArrayFiles) - 1 do
          // Создание нового файла, либо происходит запись в конец существующего файла
          // Без указания кодировки не работает
          TFile.AppendAllText(SavePath, FinishArrayFiles[J] + SLineBreak, TEncoding.ANSI)
      else
        // Создание нового файла, либо происходит перезапись существующего файла
        TFile.WriteAllLines(SavePath, FinishArrayFiles, TEncoding.ANSI);

    FinishArrayFiles := nil;

    if cbCreateBatFile.IsChecked then
      TFile.WriteAllText(TPath.Combine(TPath.GetDirectoryName(SavePath), 'CreateResFile.bat'),
        Format('brcc32.exe %s%spause', [TPath.GetFileName(SavePath), SLineBreak]));

    if cbCreateResFile.IsChecked then
      ShellExecute(0, 'open', 'cmd.exe', PWideChar('/c brcc32.exe ' + SavePath), nil, 1);
      //ShellExecute(0, 'open' , PWideChar(TPath.Combine(TPath.GetDirectoryName(ePathSaveFile.Text), 'CreateResFile.bat')), nil, nil, 1);
  finally
    TThread.Synchronize(nil,
      procedure
      begin
        AddLog('Обработка данных завершена.');
        gbSourceFiles.Enabled := True;
        gbReadyFile.Enabled := True;

        // Анимация
        AniIndicator1.Enabled := False;
        layAni.Visible := False;

        laySettings.Enabled := True;
        bCreateFile.Enabled := True;
      end);
  end;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  ChangeFolderAndFile(Self);

  OnOffbCreateFile;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  //SetLength(FFiles, 0);
  FFiles := nil;
end;

procedure TFormMain.Label6Click(Sender: TObject);
begin
  ShellExecute(0, 'open', PChar('http://delphifmandroid.blogspot.ru/2014/08/rc-res.html'), nil, nil, 1);
end;

procedure TFormMain.OnOffbCreateFile;
begin
  if ePathFolder.Text.IsEmpty xor ePathFiles.Text.IsEmpty then
    bCreateFile.Enabled := not(ePathSaveFile.Text.IsEmpty or FScanningProcess)
  else
    bCreateFile.Enabled := False;
end;

procedure TFormMain.sbSaveFileClick(Sender: TObject);
begin

  if SaveDialog1.Execute then
  begin
    if FileExists(SaveDialog1.FileName) then
    begin
      cbAddToTheOldFile.Enabled := True;
      cbAddToTheOldFile.IsChecked := TDialogServiceSync.MessageDialog
        (('Такой файл существует. Добавить записи в него?'), TMsgDlgType.mtConfirmation, mbYesNo, TMsgDlgBtn.mbNo,
        0) = mrYes;
    end
    else
    begin
      cbAddToTheOldFile.IsChecked := False;
      cbAddToTheOldFile.Enabled := False;
    end;
    ePathSaveFile.Text := SaveDialog1.FileName;
  end
  else
  begin
    ePathSaveFile.Text := '';
    cbAddToTheOldFile.IsChecked := False;
    cbAddToTheOldFile.Enabled := False;
  end;

  OnOffbCreateFile;
end;

procedure TFormMain.sbSelectFilesClick(Sender: TObject);
var
  I: Integer;
  PathFiles: string;
begin
  SetLength(FFiles, 0);

  if OpenDialog1.Execute then
  begin
    PathFiles := '';
    ePathFiles.Text := '';

    mLogs.Lines.Clear;
    AddLog('Режим ручного выбора');

    FFiles := TStringDynArray(OpenDialog1.Files.ToStringArray);

    AddLog('Файлов выбрано: ' + Length(FFiles).ToString);

    for I := 0 to OpenDialog1.Files.Count - 1 do
      if PathFiles.Length > 1 then
        PathFiles := Format('%s; %s', [PathFiles, ExtractFileName(OpenDialog1.Files[I])])
      else
        PathFiles := ExtractFileName(OpenDialog1.Files[I]);

    ePathFiles.Text := PathFiles;
  end
  else
    ePathFiles.Text := '';

  OnOffbCreateFile;
end;

procedure TFormMain.sbSelectFolderClick(Sender: TObject);
var
  PathFolder: string;
begin
  SetLength(FFiles, 0);

  // ExtractFilePath(ParamStr(0));
  if SelectDirectory('Выберите папку...', '', PathFolder) then
  begin
    ePathFolder.Text := PathFolder;
    ScanFolder(PathFolder);
  end
  else
    ePathFolder.Text := '';

  OnOffbCreateFile;
end;

function FilterPredicate(const Path: string; const SearchRec: TSearchRec): Boolean;
begin
  Result := (SearchRec.Attr = faArchive);
end;

procedure TFormMain.ScanFolder(PathFolder: string);
var
  FThread: TThread;
begin

  mLogs.Lines.Clear;

  AddLog('Режим автоматического сканирования');

  bCreateFile.Enabled := False;

  FThread := TThread.CreateAnonymousThread(
    procedure
    begin
      try
        if cbScanSubFolders.IsChecked then
        begin
          FScanningProcess := True;
          TThread.Synchronize(nil,
            procedure
            begin
              gbSourceFiles.Enabled := False;

              // Анимация
              layAni.Visible := True;
              AniIndicator1.Enabled := True;

              AddLog('Сканирование включая подкаталоги...');
            end);

          FFiles := TDirectory.GetFiles(PathFolder, TSearchOption.soAllDirectories, FilterPredicate);
        end
        else
        begin
          TThread.Synchronize(nil,
            procedure
            begin
              AddLog('Сканирование без подкаталогов...');
            end);

          FFiles := TDirectory.GetFiles(PathFolder, TSearchOption.soTopDirectoryOnly, FilterPredicate);
        end;

        FScanningProcess := False;
      finally
        TThread.Synchronize(nil,
          procedure
          begin
            gbSourceFiles.Enabled := True;

            // Анимация
            AniIndicator1.Enabled := False;
            layAni.Visible := False;

            //AddLog('Файлов найдено: ' + FFiles.Count.ToString);
            AddLog('Файлов найдено: ' + Length(FFiles).ToString);

            OnOffbCreateFile;
          end);
      end;
    end);
  FThread.Start;

end;

end.
