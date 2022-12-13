unit idesearchpanel;

{$mode objfpc}{$H+}
{_$DEFINE DebugSP}
{$MODESWITCH AdvancedRecords+}

interface

uses
  Types,
  Classes,
  Forms,
  Menus,
  SysUtils,
  Controls,
  StdCtrls,
  ExtCtrls,
  Buttons,
  IDECommands,
  ProjectIntf,
  IDEImagesIntf,
  LazIDEIntf,
  LCLType,
  LCLVersion,
  LazUTF8,
  MenuIntf,
  SrcEditorIntf,
  XMLConf,
  SynEdit,
  SynEditTypes,
  SynEditSearch,
  LResources,
  Graphics,
  LCLProc,
  fgl,
  Themes,
  DefaultTranslator,
  themedclosebutton
  {$IFDEF DebugSP},LazLoggerBase{$ENDIF}
  ;

procedure Register;

resourcestring
  mnuShowPanel = 'Show Search Panel';
  spFindNext = 'Find next';
  spFindPrev = 'Find previous';
  spWholeWords = 'Whole words only';
  spCaseSens = 'Case sensitive';
  spRegex = 'Regular expressions';
  spSAYT = 'Search-as-You-Type';
  spFromCursor = 'Search from Cursor';
  spIncremental = 'Show incremental search';
  spMulitline = 'Multiline pattern';
  spSearchOptions = 'Search options';
  spSearch = 'Search';
  spReplace = 'Replace';
  spClose = 'Close panel';
  spSearchError = 'Error in search term';

type
  TSearchEnum = (soCaseSens, soWholeWords, soRegex, soSAYT, soFromCursor, soIncremental,
    soRegExMultiLine, soReplace);

  TSearchState = record
    CaseSensitive: boolean;
    WholeWords: boolean;
    Regex: boolean;
    SAYT: boolean;
    FromCursor: boolean;
    Incremental: boolean;
    RegExMultiLine: boolean;
    InitiallyVisible: boolean;
    BlackIcons: boolean;
    ShortCut:String;
  end;

  { TSrchResult }

  TSrchResult = record
    Start: TPoint;
    Ende: TPoint;
    class operator = (Left, Right: TSrchResult): boolean;  //AdvancedRecords
  end;

  TSrchResultList = specialize TFpgList<TSrchResult>;


  { TIDESearchPanel }

  TIDESearchPanel = class(TObject)
  private
    FCurrentSrcWin: TWinControl;
    fSearchEdit, fReplaceEdit: TEdit;
    fNext, fPrev: TBitBtn;
    fOptions: TSpeedButton;
    fOptionsForm: TForm;
    fOptionsCheckGroup: TCheckGroup;
    fState: TSearchState;
    fClose: TThemedCloseButton;
    fLabel: TLabel;
    fPanel: TPanel;
    fSrch: TSynEditSearch;
    fSrchResultList: TSrchResultList;
    fSrchResultIndex: integer;
    fLastSynEdit: TSynEdit;
    fLastChangeStamp: int64;
    fLastSearchText: string;
    fSavedSelection: TSrchResult;
    fLastAutoSelection: TSrchResult;
    fSettingsChanged:Boolean;
    function CompareLastAutoSelection(ASynEdit: TSynEdit): boolean;
    procedure HideOptions;
    procedure MainWindowStateChange(Sender: TObject);
    procedure AEditChange(Sender: TObject);
    procedure AEditKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure RegisterLastAutoSelection(ASynEdit: TSynEdit);
    procedure RememberSynEdit(ASynEdit: TSynEdit);
    function SameSynEdit(ASynEdit: TSynEdit): boolean;
    function SameSynEditSelection(ASynEdit: TSynEdit): boolean;
    function SameSyneditSelTextAndSearchText(ASynEdit: TSynEdit): boolean;
    procedure SearchNext;
    procedure SearchPrev;
    procedure SynEditMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure CloseClick(Sender: TObject);
    procedure NextClick(Sender: TObject);
    procedure PrevClick(Sender: TObject);
    function GetSearchOptions: TSynSearchOptions;
    function GetSynEdit(out ASynEdit: TSynEdit): boolean;
    procedure OptionsCheckGroupItemClick(Sender: TObject; Index: integer);
    procedure OptionsClick(Sender: TObject);
    procedure PanelChangeBounds(Sender: TObject);
    procedure ScrollToCaret;
    procedure SearchFirst;
    function SyncButtonsFromSearchState: boolean;
    function SyncSearchStateFromButtons: boolean;
  protected
    function DoChangePanelVisibility(PanelVisible: boolean): boolean;
    procedure LoadConfig(cfg: TXMLConfig; const ConfigName: string);
    procedure LoadState(cfg: TXMLConfig; const StateName: string);
    procedure SaveState(cfg: TXMLConfig; const StateName: string);
    procedure LoadStates;
    procedure SaveStates;
    procedure AllocControls(AParent: TWinControl);
    procedure DeallocControls;
    procedure RealignControls;
    procedure SourceWindowCreated(Sender: TObject);
    procedure SourceWindowDestroyed(Sender: TObject);
  public
    ConfigPath: ansistring;
    constructor Create;
    destructor Destroy; override;
    procedure OnCmdClick(Sender: TObject);
    function OnProjectOpen(Sender: TObject; AProject: TLazProject): TModalResult;
    property State:TSearchState read fState;
  end;

var
  Cmd: TIDEMenuCommand = nil;
  ASearchPanel: TIDESearchPanel = nil;

const
  SPCfgRoot = 'IDESearchPanelConfig';
  SPCfgXML = 'idesearchpanelconfig.xml';
  StateNodeName = 'SPState';
  ConfigNodeName = 'SPConfig';

implementation

{$R searchpanel_images.res}

uses Math;

{$IFDEF DebugSP}
procedure DebugSP(Sendr: string; Msg: string);
begin
  debugln(['DEBUGSP: ', Sendr, ': ', Msg]);
end;
{$ENDIF}

{ TIDESearchPanel }

function TIDESearchPanel.GetSynEdit(out ASynEdit: TSynEdit): boolean;
begin
  Result := True;
  if (SourceEditorManagerIntf.ActiveSourceWindow.ActiveEditor.EditorControl is
    TSynEdit) then
  begin
    ASynEdit := TSynEdit(SourceEditorManagerIntf.ActiveSourceWindow.
      ActiveEditor.EditorControl);
    ASynEdit.OnMouseDown := @SynEditMouseDown;
  end
  else
    Result := False;
end;

function TIDESearchPanel.SameSynEdit(ASynEdit: TSynEdit): boolean;
begin
  if Assigned(ASynEdit) then
    Result := (ASynEdit = fLastSynEdit) and (ASynEdit.ChangeStamp = fLastChangeStamp)
  else
    Result := False;
  if not Result and Assigned(fLastSynEdit) then fLastSynEdit.OnMouseDown := nil;
end;

function TIDESearchPanel.SameSynEditSelection(ASynEdit: TSynEdit): boolean;
begin
  if Assigned(ASynEdit) then
    Result := SameSyneditSelTextAndSearchText(ASynEdit) or
      ((fSavedSelection.Start = ASynEdit.BlockBegin) and
      (fSavedSelection.Ende = ASynEdit.BlockEnd))
  else
    Result := False;
end;

function TIDESearchPanel.SameSyneditSelTextAndSearchText(ASynEdit: TSynEdit): boolean;
begin
  if fState.Regex then Exit(True);

  if Assigned(ASynEdit) then
  begin
    if fState.CaseSensitive then
      Result := ASynEdit.SelText = fSearchEdit.Text
    else
      Result := UTF8LowerCase(ASynEdit.SelText) = UTF8LowerCase(fSearchEdit.Text);
  end
  else
    Result := False;
end;

procedure TIDESearchPanel.RememberSynEdit(ASynEdit: TSynEdit);
begin
  if Assigned(ASynEdit) then
  begin
    fLastSynEdit := ASynEdit;
    fLastChangeStamp := ASynEdit.ChangeStamp;
    if not SameSyneditSelTextAndSearchText(ASynEdit) then
    begin
      fSavedSelection.Start := ASynEdit.BlockBegin;
      fSavedSelection.Ende := ASynEdit.BlockEnd;
    end;
  end;
end;

procedure TIDESearchPanel.RegisterLastAutoSelection(ASynEdit: TSynEdit);
begin
  fLastAutoSelection.Start := ASynEdit.BlockBegin;
  fLastAutoSelection.Ende := ASynEdit.BlockEnd;
end;

function TIDESearchPanel.CompareLastAutoSelection(ASynEdit: TSynEdit): boolean;
begin
  Result := (fLastAutoSelection.Start = ASynEdit.BlockBegin) and
    (fLastAutoSelection.Ende = ASynEdit.BlockEnd);
end;


procedure TIDESearchPanel.OptionsClick(Sender: TObject);
var
  aRect: TPoint;
  Synedit: TSynEdit;
  MaxLineWid: integer;
  LineText: string;
begin
  MaxLineWid := 0;
  for LineText in fOptionsCheckGroup.Items do
    MaxLineWid := Math.Max(fOptionsForm.Canvas.TextWidth(LineText), MaxLineWid);
  fOptionsForm.Width := MaxLineWid + 50;

  aRect := fOptions.ClientToScreen(Point(fOptions.Width, 0));
  fOptionsForm.Height := Round(fPanel.Canvas.TextHeight('AZ') * 1.6 *
    fOptionsCheckGroup.Items.Count + 1);
  fOptionsForm.Left := ARect.X;
  fOptionsForm.Top := ARect.Y - fOptionsForm.Height;
  fOptionsForm.Visible := fOptions.down;
  GetSynEdit(Synedit); //Hook events
end;


procedure TIDESearchPanel.ScrollToCaret;
var
  Synedit: TSynEdit;
begin
  if not GetSynEdit(SynEdit) then Exit;
  SynEdit.TopLine := SynEdit.BlockBegin.y - (SynEdit.LinesInWindow div 2);
end;

procedure TIDESearchPanel.HideOptions;
begin
  if Assigned(fOptions) and Assigned(fOptionsForm) then
  begin
    fOptions.down := False;
    fOptionsForm.Visible := False;
  end;
end;

procedure TIDESearchPanel.MainWindowStateChange(Sender: TObject);
begin
 {$IFDEF DebugSP}
  DebugSP('MainWindowStateChange', '');
 {$ENDIF}
  HideOptions;
end;

procedure TIDESearchPanel.AEditChange(Sender: TObject);
begin
  if fState.SAYT then SearchFirst;
end;

procedure TIDESearchPanel.AEditKeyDown(Sender: TObject; var Key: word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then
    if (fLastSearchText <> fSearchEdit.Text) then SearchFirst
    else
      SearchNext;
  //Tipp von Ally:
  // Strg+V abfangen und PasteFromClipboard ausführen, weil sonst Paste im Synedit ausgeführt wird. (Obwohl es nicht den Focus hat.)
  if (ssCtrl in Shift) and (Key = VK_V) then
  begin
    fSearchEdit.PasteFromClipboard;
    Key := 0;
  end;
  // Strg+C abfangen und CopyToClipboard ausführen, weil sonst Copy im Synedit ausgeführt wird. (Obwohl es nicht den Focus hat.)
  if (ssCtrl in Shift) and ((Key = VK_C)) then
  begin
    fSearchEdit.CopyToClipboard;
    Key := 0;
  end;
end;


procedure TIDESearchPanel.SearchFirst;
var
  Synedit: TSynEdit;
  SP, EP, FoundSP, FoundEP: TPoint;
  aRec: TSrchResult;
begin
  if GetSynEdit(SynEdit) then
  begin
    fSettingsChanged:=false;
    RememberSynEdit(SynEdit);
    if fSearchEdit.Text = '' then
    begin
     fLabel.Caption := '0 / 0';
     Exit;
    end;
    fLabel.Transparent := True;
    fLabel.Color := clNone;
    fSearchEdit.Font.Color:=clDefault;

    fLastSearchText := fSearchEdit.Text;

    if Synedit.SelAvail and (not CompareLastAutoSelection(SynEdit)) and
      (not fState.SAYT) then
    begin
      SP := SynEdit.BlockBegin;
      EP := SynEdit.BlockEnd;
    end
    else
    begin
      if fState.FromCursor then
        SP := Point(Synedit.CaretX, Synedit.CaretY)
      else
        SP := Point(1, 1);
      EP := Point(Maxint, Maxint);
    end;

    fSrch.ClearResults;
    fSrch.Pattern := fSearchEdit.Text;
    // fSrch.Replacement:=fReplaceEdit.Text;
    // fSrch.RegularExpressions:=true;

    fSrch.Sensitive := fState.CaseSensitive;
    fSrch.Whole := fState.WholeWords;
    fSrch.RegularExpressions := fState.Regex;
    fSrch.RegExprMultiLine := False;

    try
      if (Length(fSearchEdit.Text) > 1) and (fState.Incremental) then
        SynEdit.SetHighlightSearch(fSearchEdit.Text, GetSearchOptions)
      else
        SynEdit.SetHighlightSearch('', []);
    except
      fLabel.Transparent := False;
      fLabel.Color := $009595E6;
      fLabel.Caption := spSearchError;
      exit;
    end;

    try
      fSrchResultList.Clear;
      fSrchResultIndex := 0;
      if fSrch.FindNextOne(Synedit.Lines, SP, EP, FoundSP, FoundEP) then
      begin
        SynEdit.BlockBegin := FoundSP;
        SynEdit.BlockEnd := FoundEP;
        RegisterLastAutoSelection(SynEdit);
        //SynEdit.SelText:=fSrch.Replacement;
        ScrollToCaret;
        Application.ProcessMessages;
        ARec.Start := FoundSP;
        ARec.Ende := FoundEP;
        fSrchResultList.Add(aRec);
        while fSrch.FindNextOne(Synedit.Lines, FoundEP, EP, FoundSP, FoundEP) do
        begin
          ARec.Start := FoundSP;
          ARec.Ende := FoundEP;
          fSrchResultList.Add(aRec);
        end;
      end;
    except
      fLabel.Transparent := False;
      fLabel.Color := $009595E6;
      fLabel.Caption := spSearchError;
      exit;
    end;
    if fSrchResultList.Count > 0 then
      fLabel.Caption := '1 / ' + fSrchResultList.Count.ToString
    else
    begin
      fLabel.Caption := '0 / 0';
      fSearchEdit.Font.Color:=clRed;
      SynEdit.BlockBegin := SynEdit.BlockEnd;
      RegisterLastAutoSelection(SynEdit);
    end;
  end;
end;


procedure TIDESearchPanel.CloseClick(Sender: TObject);
begin
  DoChangePanelVisibility(False);
  fState.InitiallyVisible := false;
end;

procedure TIDESearchPanel.SearchNext;
var
  Synedit: TSynEdit;
  aRec: TSrchResult;
begin
  if GetSynEdit(SynEdit) then
    if (not SameSynEdit(SynEdit)) or (not SameSynEditSelection(SynEdit) or fSettingsChanged) then
      SearchFirst
    else
    if fSrchResultIndex < fSrchResultList.Count - 1 then
    begin
      Inc(fSrchResultIndex);
      aRec := fSrchResultList[fSrchResultIndex];
      SynEdit.BlockBegin := aRec.Start;
      SynEdit.BlockEnd := aRec.Ende;
      RegisterLastAutoSelection(SynEdit);
      ScrollToCaret;
      fLabel.Caption := (fSrchResultIndex + 1).ToString + ' / ' +
        fSrchResultList.Count.ToString;
      //Synedit.SetFocus; //For F3 Key
    end;
end;

procedure TIDESearchPanel.NextClick(Sender: TObject);
begin
  SearchNext;
end;

procedure TIDESearchPanel.SearchPrev;
var
  Synedit: TSynEdit;
  aRec: TSrchResult;
begin
  if GetSynEdit(SynEdit) then
    if (not SameSynEdit(SynEdit)) or (not SameSynEditSelection(SynEdit) or fSettingsChanged) then
      SearchFirst
    else
    if fSrchResultIndex > 0 then
    begin
      Dec(fSrchResultIndex);
      aRec := fSrchResultList[fSrchResultIndex];
      SynEdit.BlockBegin := aRec.Start;
      SynEdit.BlockEnd := aRec.Ende;
      RegisterLastAutoSelection(SynEdit);
      ScrollToCaret;
      fLabel.Caption := (fSrchResultIndex + 1).ToString + ' / ' +
        fSrchResultList.Count.ToString;
      //Synedit.SetFocus; //For F3 Key
    end;
end;

procedure TIDESearchPanel.PrevClick(Sender: TObject);
begin
  SearchPrev;
end;

function TIDESearchPanel.DoChangePanelVisibility(PanelVisible: boolean): boolean;
begin
  {$IFDEF DebugSP}
  DebugSP('DoChangePanelVisibility', PanelVisible.ToString(TUseBoolStrs.True));
  {$ENDIF}
  if PanelVisible then
  begin
    Result := False;
    if not (Assigned(SourceEditorManagerIntf) and
      Assigned(SourceEditorManagerIntf.ActiveSourceWindow)) then Exit;

    if not Assigned(fPanel) then
      AllocControls(SourceEditorManagerIntf.ActiveSourceWindow);

    if fPanel.Parent <> SourceEditorManagerIntf.ActiveSourceWindow then
    begin
      fPanel.Parent := SourceEditorManagerIntf.ActiveSourceWindow;
    end;
    fPanel.Visible := True;
    Cmd.Checked := True;
    fPanel.Height := 40;
    fSearchEdit.SetFocus;
    Result := True;
  end
  else
  begin
    if Assigned(fPanel) then
    begin
      HideOptions;
      fPanel.Visible := False;
      Cmd.Checked := False;
     if assigned(SourceEditorManagerIntf) and assigned(SourceEditorManagerIntf.ActiveSourceWindow) and
       assigned(SourceEditorManagerIntf.ActiveSourceWindow.ActiveEditor) then
        SourceEditorManagerIntf.ActiveSourceWindow.ActiveEditor.EditorControl.SetFocus;
    end;
    Result := True;
  end;
end;

constructor TIDESearchPanel.Create;
begin
{$IFDEF DebugSP}
  DebugSP('Create', '');
{$ENDIF}
  if SourceEditorManagerIntf <> nil then
  begin
    SourceEditorManagerIntf.RegisterChangeEvent(semWindowCreate, @SourceWindowCreated);
    SourceEditorManagerIntf.RegisterChangeEvent(semWindowDestroy,
      @SourceWindowDestroyed);
  end;
  fSrch := TSynEditSearch.Create;
  fSrchResultList := TSrchResultList.Create;
  ConfigPath := IncludeTrailingPathDelimiter(LazarusIDE.GetPrimaryConfigPath) +
    SPCfgXML;
  fSettingsChanged:=true;
end;

destructor TIDESearchPanel.Destroy;
begin
  {$IFDEF DebugSP} DebugSP('Destroy', ''); {$ENDIF}
  SaveStates;
  DeallocControls;
  fSrchResultList.Free;
  fSrch.Free;
  inherited Destroy;
end;

procedure TIDESearchPanel.OnCmdClick(Sender: TObject);
begin
  {$IFDEF DebugSP}
  DebugSP('OnCmdClick', Cmd.Checked.ToString(TUseBoolStrs.True));
  {$ENDIF}
  if Cmd <> nil then
  begin
    fState.InitiallyVisible := not Cmd.Checked;
    DoChangePanelVisibility(not Cmd.Checked);
  end;
  {$IFDEF DebugSP}
  DebugSP('OnCmdClick2', Cmd.Checked.ToString(TUseBoolStrs.True));
  {$ENDIF}
end;

function TIDESearchPanel.OnProjectOpen(Sender: TObject;
  AProject: TLazProject): TModalResult;
begin
  {$IFDEF DebugSP}
  DebugSP('OnProjectOpen', fState.InitiallyVisible.ToString(TUseBoolStrs.True));
  {$ENDIF}
  DoChangePanelVisibility(fState.InitiallyVisible);
  Result := mrOk;
  if Assigned(LazarusIDE.LastFormActivated) then
    LazarusIDE.LastFormActivated.BringToFront;
end;

procedure TIDESearchPanel.SynEditMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  {$IFDEF DebugSP} DebugSP('SynEditMouseDown', ''); {$ENDIF}
  HideOptions;
  TSynEdit(Sender).SetHighlightSearch('', []);
  fSavedSelection.Start := Point(1, 1);
  fSavedSelection.Ende := Point(MaxInt, MaxInt);
end;

function TIDESearchPanel.SyncSearchStateFromButtons: boolean;
begin
  if Assigned(fOptionsCheckGroup) then
  begin
    fState.CaseSensitive := fOptionsCheckGroup.Checked[Ord(soCaseSens)];
    fState.WholeWords := fOptionsCheckGroup.Checked[Ord(soWholeWords)];
    fState.Regex := fOptionsCheckGroup.Checked[Ord(soRegex)];
    fState.SAYT := fOptionsCheckGroup.Checked[Ord(soSAYT)];
    fState.FromCursor := fOptionsCheckGroup.Checked[Ord(soFromCursor)];
    fState.Incremental := fOptionsCheckGroup.Checked[Ord(soIncremental)];
    Result := True;
  end
  else
    Result := False;
end;

function TIDESearchPanel.SyncButtonsFromSearchState: boolean;
begin
  if Assigned(fOptionsCheckGroup) then
  begin
    fOptionsCheckGroup.Checked[Ord(soCaseSens)] := fState.CaseSensitive;
    fOptionsCheckGroup.Checked[Ord(soWholeWords)] := fState.WholeWords;
    fOptionsCheckGroup.Checked[Ord(soRegex)] := fState.Regex;
    fOptionsCheckGroup.Checked[Ord(soSAYT)] := fState.SAYT;
    fOptionsCheckGroup.Checked[Ord(soFromCursor)] := fState.FromCursor;
    fOptionsCheckGroup.Checked[Ord(soIncremental)] := fState.Incremental;
    Result := True;
  end
  else
    Result := False;
end;

function TIDESearchPanel.GetSearchOptions: TSynSearchOptions;
begin
  Result := [];
  if SyncSearchStateFromButtons then
  begin
    if fState.CaseSensitive then Include(Result, ssoMatchCase);
    if fState.WholeWords then Include(Result, ssoWholeWord);
    if fState.Regex then Include(Result, ssoRegExpr);
  end;
end;

procedure TIDESearchPanel.PanelChangeBounds(Sender: TObject);
begin
  RealignControls;
end;

procedure TIDESearchPanel.OptionsCheckGroupItemClick(Sender: TObject; Index: integer);
begin
  if Index = Ord(soSAYT) then
    if fOptionsCheckGroup.Checked[Ord(soSAYT)] then
      fOptionsCheckGroup.Checked[Ord(soRegex)] := False;
  if Index = Ord(soRegex) then
    if fOptionsCheckGroup.Checked[Ord(soRegex)] then
      fOptionsCheckGroup.Checked[Ord(soSAYT)] := False;
  fSettingsChanged:=true;
  SyncSearchStateFromButtons;
end;

procedure TIDESearchPanel.AllocControls(AParent: TWinControl);
var
  Black: string;
begin
  {$IFDEF DebugSP} DebugSP('AllocControls', ''); {$ENDIF}
  FCurrentSrcWin := AParent;
  if fState.BlackIcons then Black := '_black'
  else
    Black := '';
  fPanel := TPanel.Create(AParent);
  fPanel.Parent := AParent;
  fPanel.BorderStyle := bsNone;
  fPanel.Align := alBottom;
  fPanel.OnChangeBounds := @PanelChangeBounds;

  fSearchEdit := TEdit.Create(fPanel);
  fSearchEdit.TextHint := spSearch;
  fSearchEdit.Top := fPanel.Scale96ToFont(4);
  fSearchEdit.Left := fPanel.Scale96ToFont(4);
  fSearchEdit.Width := 200;
  fSearchEdit.Parent := fPanel;
  fSearchEdit.OnChange := @AEditChange;
  fSearchEdit.OnKeyDown := @AEditKeyDown;

  fNext := TBitBtn.Create(fPanel);
  fNext.AutoSize := False;
  fNext.GlyphShowMode := gsmAlways;
  fNext.Width := fPanel.Scale96ToFont(50);
  fNext.Caption := '';
  fNext.Hint := spFindNext;
  fNext.ShowHint := True;
  fNext.Parent := fPanel;
  fNext.Images := IDEImages.Images_16;
  fNext.ImageIndex := IDEImages.Images_16.GetImageIndex('arrow_28' + Black);
  fNext.ImageWidth:=13;
  fNext.OnClick := @NextClick;

  fPrev := TBitBtn.Create(fPanel);
  fPrev.AutoSize := False;
  fPrev.GlyphShowMode := gsmAlways;
  fPrev.Caption := '';
  fPrev.Width := fPanel.Scale96ToFont(50);
  fPrev.Hint := spFindPrev;
  fPrev.ShowHint := True;
  fPrev.Parent := fPanel;
  fPrev.Images := IDEImages.Images_16;
  fPrev.ImageIndex := IDEImages.Images_16.GetImageIndex('arrow_27' + Black);
  fPrev.ImageWidth:=13;
  fPrev.OnClick := @PrevClick;

  fOptions := TSpeedButton.Create(fpanel);
  fOptions.Hint := spSearchOptions;
  fOptions.ShowHint := True;
  fOptions.Parent := fPanel;
  fOptions.AllowAllUp := True;
  fOptions.GroupIndex := -1;
  fOptions.Images := IDEImages.Images_16;
  fOptions.ImageIndex := IDEImages.Images_16.GetImageIndex('setup_06' + Black);
  fOptions.ImageWidth:=13;
  fOptions.OnClick := @OptionsClick;

  fLabel := TLabel.Create(fPanel);
  fLabel.Parent := fPanel;
  fLabel.Caption := '0/0';
  fLabel.AutoSize := True;
  //  fLabel.Width := 60;         // kann nach AutoSize=true nicht mehr geändert werden
  fLabel.Alignment := taCenter;
  fLabel.Caption := '0/0';

  fOptionsForm := TForm.CreateNew(fPanel);
  fOptionsForm.FormStyle := fsStayOnTop;
  fOptionsForm.ShowInTaskBar := stNever;
  fOptionsForm.BorderStyle := bsNone;
  fOptionsForm.Height := 100;
  fOptionsForm.Width := 300;

  fOptionsCheckGroup := TCheckGroup.Create(fOptionsForm);
  fOptionsCheckGroup.Caption := spSearchOptions;
  fOptionsCheckGroup.Parent := fOptionsForm;
  fOptionsCheckGroup.Align := AlClient;
  fOptionsCheckGroup.Items.Add(spCaseSens);
  fOptionsCheckGroup.Items.Add(spWholeWords);
  fOptionsCheckGroup.Items.Add(spRegex);
  fOptionsCheckGroup.Items.Add(spSAYT);
  fOptionsCheckGroup.Items.Add(spFromCursor);
  fOptionsCheckGroup.Items.Add(spIncremental);
  fOptionsCheckGroup.OnItemClick := @OptionsCheckGroupItemClick;

  SyncButtonsFromSearchState;

  fClose:=TThemedCloseButton.Create(fPanel);
  fClose.Parent:=fPanel;
  fClose.SetPreferredSize;
  fClose.OnClick := @CloseClick;
  fClose.Hint := spClose;
  fClose.ShowHint := True;

  RealignControls;
end;

procedure TIDESearchPanel.RealignControls;
const
  MARGIN = 5;
var
  PrevCtrl: TControl;
  scaledMargin: integer;
begin
  scaledMargin := fPanel.Scale96ToFont(MARGIN);

  fClose.Left := fPanel.Width -fClose.Width - 6;
  fClose.Top := 6;

  fPanel.Height := fSearchEdit.Height + fPanel.Scale96ToFont(12);

  fSearchEdit.Width := fPanel.Width div 3;

  PrevCtrl := fSearchEdit;

  if Assigned(fReplaceEdit) then
  begin
    fReplaceEdit.Left := PrevCtrl.Left + PrevCtrl.Width + scaledMargin;
    fReplaceEdit.Width := PrevCtrl.Width;
    PrevCtrl := fReplaceEdit;
  end;

  fNext.Top := PrevCtrl.Top;
  fNext.Left := PrevCtrl.Left + PrevCtrl.Width + scaledMargin;
  fNext.Height := PrevCtrl.Height;

  PrevCtrl := fNext;
  fPrev.Top := PrevCtrl.Top;
  fPrev.Left := PrevCtrl.Left + PrevCtrl.Width + scaledMargin;
  fPrev.Height := PrevCtrl.Height;

  PrevCtrl := fPrev;
  fOptions.Height := PrevCtrl.Height;
  fOptions.Width := fOptions.Height;
  fOptions.Left := PrevCtrl.Left + PrevCtrl.Width + scaledMargin;
  fOptions.Top := PrevCtrl.Top + PrevCtrl.Height - fOptions.Height;

  PrevCtrl := fOptions;
  fLabel.Left := PrevCtrl.Left + PrevCtrl.Width + 2 * scaledMargin;
  fLabel.Top := PrevCtrl.Top + (PrevCtrl.Height - fLabel.Height) div 2;

end;

procedure TIDESearchPanel.DeallocControls;
begin
   {$IFDEF DebugSP} DebugSP('DeAllocControls', ''); {$ENDIF}
  fSearchEdit := nil;
  fReplaceEdit := nil;
  fNext := nil;
  fPrev := nil;
  fLabel := nil;

  fPanel := nil;
  fOptionsCheckGroup := nil;
  fOptionsForm := nil;
end;

procedure TIDESearchPanel.SourceWindowCreated(Sender: TObject);
begin
  {$IFDEF DebugSP} DebugSP('SourceWindowCreated', ''); {$ENDIF}
  if Assigned(FCurrentSrcWin) or (SourceEditorManagerIntf.SourceWindowCount > 1) then
    Exit;
end;

procedure TIDESearchPanel.SourceWindowDestroyed(Sender: TObject);
begin
   {$IFDEF DebugSP} DebugSP('SourceWindowDestroyed', ''); {$ENDIF}
  if FCurrentSrcWin <> Sender then Exit;
  DoChangePanelVisibility(False);
  DeallocControls;
  FCurrentSrcWin := nil;
end;

procedure TIDESearchPanel.LoadConfig(cfg: TXMLConfig; const ConfigName: string);
begin
  fState.ShortCut:=UTF8Encode(cfg.GetValue(UTF8Decode(ConfigName + '/ShortCut'), 'Ctrl+P'));
  fState.BlackIcons := cfg.GetValue(UTF8Decode(ConfigName + '/BlackIcons'), True);
end;

procedure TIDESearchPanel.LoadState(cfg: TXMLConfig; const StateName: string);

  function _GetValue(APath: string; ADefault: boolean): boolean;
  begin
    Result := cfg.GetValue(UTF8Decode(APath), ADefault);
  end;

begin
  fState.CaseSensitive := _GetValue(StateName + '/CaseSensitive', False);
  fState.WholeWords := _GetValue(StateName + '/WholeWords', False);
  fState.Regex := _GetValue(StateName + '/Regex', False);
  fState.SAYT := _GetValue(StateName + '/SAYT', True);
  fState.FromCursor := _GetValue(StateName + '/FromCursor', False);
  fState.Incremental := _GetValue(StateName + '/Incremental', True);
  fState.InitiallyVisible := _GetValue(StateName + '/InitiallyVisible', True);
  {$IFDEF DebugSP} DebugSP('LoadState: InitiallyVisible=',
    fState.InitiallyVisible.ToString(TUseBoolStrs.True)); {$ENDIF}
end;

procedure TIDESearchPanel.SaveState(cfg: TXMLConfig; const StateName: string);

  procedure _SetValue(APath: string; AValue: boolean);
  begin
    cfg.SetValue(UTF8Decode(APath), AValue);
  end;

begin
{$IFDEF DebugSP} DebugSP('SaveState InitiallyVisible=',
    fState.InitiallyVisible.ToString(TUseBoolStrs.True)); {$ENDIF}
  _SetValue(StateName + '/CaseSensitive', fState.CaseSensitive);
  _SetValue(StateName + '/WholeWords', fState.WholeWords);
  _SetValue(StateName + '/Regex', fState.Regex);
  _SetValue(StateName + '/SAYT', fState.SAYT);
  _SetValue(StateName + '/FromCursor', fState.FromCursor);
  _SetValue(StateName + '/Incremental', fState.Incremental);
  _SetValue(StateName + '/InitiallyVisible', fState.InitiallyVisible);
end;

function CreateXMLConfig(const xmlfile: string): TXMLConfig;
begin
  Result := TXMLConfig.Create(nil);
  Result.RootName := SPCfgRoot;
  Result.Filename := xmlfile;
end;

procedure TIDESearchPanel.LoadStates;
var
  cfg: TXMLConfig;
begin
  {$IFDEF DebugSP} DebugSP('LoadStates', ''); {$ENDIF}
  cfg := CreateXMLConfig(ConfigPath);
  try
    LoadConfig(cfg, ConfigNodeName);
    LoadState(cfg, StateNodeName);
  finally
    cfg.Free;
  end;
end;

procedure TIDESearchPanel.SaveStates;
var
  cfg: TXMLConfig;
begin
  {$IFDEF DebugSP} DebugSP('SaveStates', ''); {$ENDIF}
  cfg := CreateXMLConfig(ConfigPath);
  try
    SaveState(cfg, StateNodeName)
  finally
    cfg.Free;
  end;
end;

{ TSrchResult }

class operator TSrchResult. = (Left, Right: TSrchResult): boolean;
begin
  Result := Left.Start.Y > Right.Start.Y; //Dummy.
end;

procedure Register;
var Key: TIDEShortCut;
    Cat: TIDECommandCategory;
    CmdSP: TIDECommand;
    Key1:Word;
    Shift1:TShiftState;
begin
  ASearchPanel := TIDESearchPanel.Create;
  try
    ASearchPanel.LoadStates;
  except
 {$IFDEF DebugSP}
    DebugSP('Exception LoadStates', '');
 {$ENDIF}
  end;
  ShortCutToKey(TextToShortCutRaw(Trim(ASearchPanel.State.ShortCut)),Key1,Shift1);
  Key := IDEShortCut(Key1,Shift1,VK_UNKNOWN,[]);
  Cat:=IDECommandList.FindIDECommand(ecFind).Category;
  CmdSP := RegisterIDECommand(Cat,mnuShowPanel, mnuShowPanel, Key, @ASearchPanel.OnCmdClick, nil);
  Cmd := RegisterIDEMenuCommand(itmSearchFindReplace, 'showSearchPanel', mnuShowPanel,  nil, nil, CmdSP);
  LazarusIDE.AddHandlerOnProjectOpened(@ASearchPanel.OnProjectOpen, False);
  LazarusIDE.GetMainBar.OnWindowStateChange := @ASearchPanel.MainWindowStateChange;
end;

initialization

finalization
  ASearchPanel.Free;

end.
