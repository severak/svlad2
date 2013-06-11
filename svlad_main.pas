unit svlad_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Menus, Grids, PairSplitter, CSVDocument, lua52, SvladAbout;

type

  { TForm1 }

  TForm1 = class(TForm)
    FontDialog: TFontDialog;
    MainMenu: TMainMenu;
    MenuAbout: TMenuItem;
    MenuActions: TMenuItem;
    MenuSheetAutosize: TMenuItem;
    MenuSheetFont: TMenuItem;
    MenuSheetSeparatorII: TMenuItem;
    MenuSheetFixedCol: TMenuItem;
    MenuSheetSeparator: TMenuItem;
    MenuSheetFixedRow: TMenuItem;
    PairSplitter1: TPairSplitter;
    PairSplitterSide1: TPairSplitterSide;
    PairSplitterSide2: TPairSplitterSide;
    PopupQueryRun: TMenuItem;
    PopupQueryClear: TMenuItem;
    MenuFile: TMenuItem;
    PopupQueryOpen: TMenuItem;
    PopupQuerySave: TMenuItem;
    MenuSheetOpen: TMenuItem;
    MenuSheetSave: TMenuItem;
    MenuSheetClear: TMenuItem;
    OpenDialogQuery: TOpenDialog;
    OpenDialogTable: TOpenDialog;
    Query: TMemo;
    PopupMenu1: TPopupMenu;
    Grid: TStringGrid;
    SaveDialogQuery: TSaveDialog;
    SaveDialogTable: TSaveDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure GridSetEditText(Sender: TObject; ACol, ARow: integer;
      const Value: string);
    procedure MenuAboutClick(Sender: TObject);
    procedure MenuSheetAutosizeClick(Sender: TObject);
    procedure MenuSheetClearClick(Sender: TObject);
    procedure MenuSheetFixedColClick(Sender: TObject);
    procedure MenuSheetFixedRowClick(Sender: TObject);
    procedure MenuSheetFontClick(Sender: TObject);
    procedure MenuSheetOpenClick(Sender: TObject);
    procedure MenuSheetSaveClick(Sender: TObject);
    procedure PopupQueryOpenClick(Sender: TObject);
    procedure PopupQueryRunClick(Sender: TObject);
    procedure PopupQueryClearClick(Sender: TObject);
    procedure PopupQuerySaveClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

  TLuaMenuItem = class(TMenuItem)
  public
    LuaFunctionName: string;
    procedure Clicked(Sender: TObject);
  end;

  TSvladCoords = record
    row: integer;
    col: integer;
  end;

var
  Form1: TForm1;
  L: Plua_State;
  Result: integer;
  maxR: integer;
  maxC: integer;

const
  TAB = #9;

implementation

{$R *.lfm}

{ TForm1 }

const
  SVLAD_RELEASE = 1;

function h_coords(row: integer; col: integer): TSvladCoords;
begin
  Result.row := row;
  Result.col := col;
end;

function h_confirm(Question: string): boolean;
var
  Confirmed: integer;
begin
  Confirmed := MessageDlg('Confirm', Question, mtConfirmation, [mbOK, mbCancel], 0);
  if Confirmed = mrOk then
    Result := True
  else
    Result := False;
end;

procedure g_checkBounds(L: PLua_state; maxR: integer; maxC: integer);
begin
  if ((maxR > Form1.Grid.RowCount) or (maxC > Form1.Grid.ColCount)) then
  begin
    luaL_error(L, 'Grid coordinates [%f,%f] out of bounds!', [maxR, maxC]);
  end;
end;

procedure g_clean();
begin
  Form1.Grid.Clean;
  Form1.Grid.AutoSizeColumns;
  maxR := 0;
  maxC := 0;
end;

procedure g_set_max(ARow: integer; ACol: integer);
begin
  if ARow > maxR then
    maxR := ARow;
  if ACol > maxC then
    maxC := ACol;
end;

procedure g_update_max();
var
  row, col, maxRC, maxCC, z: integer;
begin
  if Trim(Form1.Grid.Cells[maxC,maxR])='' then
  begin
    (*maxRC := maxR;
    maxCC := maxC;
    for row := maxR downto 0 do
    begin
      for col := maxC downto 0 do
      begin
        if Trim(Form1.Grid.Cells[col,row])<>'' then
        begin
          maxR := row;
          maxC := col;
          break;
        end;
      end;
    end; *)
  end;
end;

function svlad_alert(L: Plua_state): integer; cdecl;
var
  Text: string;
begin
  Text := lua_tostring(L, -1);
  ShowMessage(Text);
  Result := 0;
end;

function svlad_confirm(L: Plua_state): integer; cdecl;
var
  Question: string;
  Confirmed: boolean;
begin
  Question := lua_tostring(L, -1);
  Confirmed := h_confirm(Question);
  lua_pushboolean(L, Confirmed);
  Result := 1;
end;

function svlad_prompt(L: Plua_state): integer; cdecl;
var
  Prompt: string;
  Default: string;
  Reply: string;
begin
  Prompt := lua_tostring(L, -2);
  Default := lua_tostring(L, -1);
  Reply := InputBox('Svlad', Prompt, Default);
  lua_pushstring(L, Reply);
  Result := 1;
end;

function svlad_get(L: Plua_state): integer; cdecl;
var
  row, col: integer;
  Text: string;
begin
  row := lua_tointeger(L, -2);
  col := lua_tointeger(L, -1);
  g_checkBounds(L, row, col);
  Text := Form1.Grid.Cells[col, row];
  lua_pushstring(L, Text);
  Result := 1;
end;

function svlad_focus(L: Plua_state): integer; cdecl;
var
  row, col: integer;
begin
  row := lua_tointeger(L, -2);
  col := lua_tointeger(L, -1);
  g_checkBounds(L, row, col);
  Form1.Grid.row := row;
  Form1.Grid.col := col;
  Form1.Grid.SetFocus;
  Result := 0;
end;

function svlad_set(L: Plua_state): integer; cdecl;
var
  row, col: integer;
  Text: string;
begin
  row := lua_tointeger(L, -3);
  col := lua_tointeger(L, -2);
  Text := lua_tostring(L, -1);
  g_checkBounds(L, row, col);
  Form1.Grid.Cells[col, row] := Text;
  g_set_max(row, col);
  Result := 0;
end;

function svlad_autosize(L: Plua_state): integer; cdecl;
begin
  Form1.Grid.AutoSizeColumns;
  Result := 0;
end;

function svlad_get_max(L: Plua_state): integer; cdecl;
begin
  g_update_max();
  lua_pushinteger(L, maxR);
  lua_pushinteger(L, maxC);
  Result := 2;
end;

function svlad_get_bounds(L: Plua_state): integer; cdecl;
begin
  lua_pushinteger(L, Form1.Grid.RowCount);
  lua_pushinteger(L, Form1.Grid.ColCount);
  Result := 2;
end;

function svlad_clean(L: Plua_state): integer; cdecl;
begin
  g_clean();
  Result := 0;
end;

function svlad_menu_add(L: Plua_state): integer; cdecl;
var
  item: TLuaMenuItem;
  Caption: string;
  functionName: string;
begin
  Caption := lua_tostring(L, -2);
  functionName := lua_tostring(L, -1);
  item := TLuaMenuItem.Create(Form1.MenuActions);
  item.Caption := Caption;
  item.LuaFunctionName := functionName;
  item.OnClick := @item.Clicked;
  Form1.MenuActions.Add(item);
  Form1.MenuActions.Enabled := True;
  Result := 0;
end;

function svlad_menu_clean(L: Plua_state): integer; cdecl;
begin
  Form1.MenuActions.Clear;
  Form1.MenuActions.Enabled := False;
  Result := 0;
end;

procedure LuaH_table_set_function(L: Plua_state; field: string; fun: lua_CFunction);
begin
  lua_pushcfunction(L, fun);
  lua_setfield(L, -2, PChar(field));
end;

procedure LuaH_error_handle(L: Plua_state);
var
  msg: string;
const
  NL = #10#13;
begin
  msg := lua_tostring(L, -1);
  ShowMessage('Lua error:' + NL + msg);
end;

procedure TluaMenuItem.Clicked(Sender: TObject);
begin
  lua_getglobal(L, PChar(LuaFunctionName));
  if lua_pcall(L, 0, 0, 0) <> LUA_OK then
  begin
    LuaH_error_handle(L);
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  L := luaL_newstate(); (* 5.2 change *)
  luaL_openlibs(L);
  lua_newtable(L);
  LuaH_table_set_function(L, 'alert', @svlad_alert);
  LuaH_table_set_function(L, 'confirm', @svlad_confirm);
  LuaH_table_set_function(L, 'prompt', @svlad_prompt);
  LuaH_table_set_function(L, 'get', @svlad_get);
  LuaH_table_set_function(L, 'set', @svlad_set);
  LuaH_table_set_function(L, 'focus', @svlad_focus);
  LuaH_table_set_function(L, 'autosize', @svlad_autosize);
  LuaH_table_set_function(L, 'get_max', @svlad_get_max);
  LuaH_table_set_function(L, 'get_bounds', @svlad_get_bounds);
  LuaH_table_set_function(L, 'clean', @svlad_clean);
  LuaH_table_set_function(L, 'menu_add', @svlad_menu_add);
  LuaH_table_set_function(L, 'menu_clean', @svlad_menu_clean);
  lua_setglobal(L, 'svlad');
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  lua_close(L);
end;

procedure TForm1.GridSetEditText(Sender: TObject; ACol, ARow: integer;
  const Value: string);
begin
  g_set_max(ARow, ACol);
end;

procedure TForm1.MenuSheetClearClick(Sender: TObject);
begin
  if h_confirm('Really want to clean sheet?') then
    g_clean();
end;

procedure TForm1.MenuSheetFixedColClick(Sender: TObject);
begin
  MenuSheetFixedCol.Checked := not MenuSheetFixedCol.Checked;
  if (MenuSheetFixedCol.Checked) then
    Form1.Grid.FixedCols := 1
  else
    Form1.Grid.FixedCols := 0;
end;

procedure TForm1.MenuSheetFixedRowClick(Sender: TObject);
begin
  MenuSheetFixedRow.Checked := not MenuSheetFixedRow.Checked;
  if (MenuSheetFixedRow.Checked) then
    Form1.Grid.FixedRows := 1
  else
    Form1.Grid.FixedRows := 0;
end;

procedure TForm1.MenuSheetFontClick(Sender: TObject);
begin
  if FontDialog.Execute then
  begin
    Grid.Font.Name := FontDialog.Font.Name;
    Grid.Font.Style := FontDialog.Font.Style;
    Grid.Font.Size := FontDialog.Font.Size;
    Grid.Font.Color := FontDialog.Font.Color;
  end;
end;

procedure TForm1.MenuSheetOpenClick(Sender: TObject);
var
  Parser: TCSVParser;
  FileStream: TFileStream;
begin
  if OpenDialogTable.Execute then
  begin
    maxR := 0;
    maxC := 0;
    Grid.Clean;
    Parser := TCSVParser.Create;
    if not (FileExistsUTF8(OpenDialogTable.FileName)) then
      exit;
    Parser := TCSVParser.Create;
    FileStream := TFileStream.Create(OpenDialogTable.FileName,
      fmOpenRead + fmShareDenyWrite);
    try
      Parser.Delimiter := TAB;
      Parser.SetSource(FileStream);
      Grid.BeginUpdate;
      while Parser.ParseNextCell do
      begin
        Grid.Cells[Parser.CurrentCol, Parser.CurrentRow] := Parser.CurrentCellText;
        maxR := Parser.CurrentRow;
        maxC := Parser.CurrentCol;
      end;
    finally
      Parser.Free;
      FileStream.Free;
      Grid.EndUpdate;
    end;
  end;
end;

procedure TForm1.MenuSheetSaveClick(Sender: TObject);
var
  Builder: TCSVBuilder;
  FileStream: TFileStream;
  r, c: integer;
begin
  if SaveDialogTable.Execute then
  begin
    Builder := TCSVBuilder.Create;
    FileStream := TFileStream.Create(SaveDialogTable.FileName, fmOpenWrite + fmCreate);
    try
      Builder.Delimiter := TAB;
      Builder.SetOutput(FileStream);
      for r := 0 to maxR do
      begin
        for c := 0 to maxC do
        begin
          Builder.AppendCell(Grid.Cells[c, r]);
        end;
        Builder.AppendRow;
      end;
    finally
      Builder.Free;
      FileStream.Free;
    end;
  end;
end;

procedure TForm1.PopupQueryOpenClick(Sender: TObject);
begin
  if OpenDialogQuery.Execute then
  begin
    Query.Lines.LoadFromFile(OpenDialogQuery.FileName);
  end;
end;

procedure TForm1.PopupQueryRunClick(Sender: TObject);
var
  code: PChar;
begin
  code := Query.Lines.GetText;
  if Length(code) > 0 then
  begin
    if luaL_dostring(L, code) <> LUA_OK then
    begin
      LuaH_error_handle(L);
    end;
  end;
end;

procedure TForm1.PopupQueryClearClick(Sender: TObject);
begin
  if h_confirm('Really want to clean query?') then
    Query.Clear;
end;

procedure TForm1.PopupQuerySaveClick(Sender: TObject);
begin
  if SaveDialogQuery.Execute then
  begin
    Query.Lines.SaveToFile(SaveDialogQuery.FileName);
  end;
end;

procedure TForm1.MenuAboutClick(Sender: TObject);
begin
  SvladAbout.FormSvladAbout.ShowModal;
end;

procedure TForm1.MenuSheetAutosizeClick(Sender: TObject);
begin
  Grid.AutoSizeColumns;
end;

end.