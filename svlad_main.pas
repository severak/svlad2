unit svlad_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Menus, Grids, PairSplitter, CSVDocument, lua52;

type

  { TForm1 }

  TForm1 = class(TForm)
    MainMenu: TMainMenu;
    MenuAbout: TMenuItem;
    MenuActions: TMenuItem;
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
    procedure MenuSheetClearClick(Sender: TObject);
    procedure MenuSheetFixedColClick(Sender: TObject);
    procedure MenuSheetFixedRowClick(Sender: TObject);
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
      LuaFunctionName : String;
      procedure Clicked(Sender : TObject);
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

const SVLAD_RELEASE = 1;

procedure g_checkBounds(L : PLua_state; maxR : Integer; maxC: Integer);
begin
  if ((maxR>Form1.Grid.RowCount) OR (maxC>Form1.Grid.ColCount)) then
  begin
    luaL_error(L,'Grid coordinates [%f,%f] out of bounds!',[maxR,maxC]);
  end;
end;

function svlad_alert(L: Plua_state): Integer; cdecl;
var
  text : String;
begin
  text := lua_tostring(L, -1);
  ShowMessage(text);
  Result:=0;
end;

function svlad_confirm(L: Plua_state): Integer; cdecl;
var
  Question : String;
  Confirmed : Integer;
begin
  Question := lua_tostring(L, -1);
  Confirmed := MessageDlg('Svlad', Question, mtConfirmation, [mbOK, mbCancel], 0);
  if Confirmed=mrOK then
    lua_pushboolean(L,true)
  else
    lua_pushboolean(L,false);
  Result := 1;
end;

function svlad_prompt(L: Plua_state): Integer; cdecl;
var
  Prompt : String;
  Default : String;
  Reply : String;
begin
  Prompt := lua_tostring(L, -2);
  Default := lua_tostring(L, -1);
  Reply := InputBox('Svlad',Prompt,Default);
  lua_pushstring(L, Reply);
  Result := 1;
end;

function svlad_get(L: Plua_state): Integer; cdecl;
var
  row, col : Integer;
  text : String;
begin
  row := lua_tointeger(L, -2);
  col := lua_tointeger(L, -1);
  g_checkBounds(L, row, col);
  text := Form1.Grid.Cells[col,row];
  lua_pushstring(L, text);
  Result := 1;
end;

function svlad_set(L: Plua_state): Integer; cdecl;
var
  row, col : Integer;
  text : String;
begin
  row := lua_tointeger(L, -3);
  col := lua_tointeger(L, -2);
  text := lua_tostring(L, -1);
  Form1.Grid.Cells[col,row] := text;
  if row > maxR then
    maxR := row;
  if col > maxC then
    maxC := col;
  Result := 0;
end;

function svlad_clean(L: Plua_state): Integer; cdecl;
begin
  Form1.Grid.Clean;
  maxR := 0;
  maxC := 0;
  Result := 0;
end;

function svlad_menu_add(L: Plua_state): Integer; cdecl;
var
  item : TLuaMenuItem;
  caption : String;
  functionName : String;
  index : Integer;
begin
  caption := lua_tostring(L, -2);
  functionName := lua_tostring(L, -1);
  item := TLuaMenuItem.Create(Form1.MenuActions);
  item.caption := caption;
  item.LuaFunctionName := functionName;
  item.OnClick:= @item.Clicked;
  Form1.MenuActions.Add(item);
  index := Form1.MenuActions.IndexOf(item);
  Form1.MenuActions.Enabled:=true;
  lua_pushinteger(L, index);
  Result := 1;
end;

procedure LuaH_table_set_function(L: Plua_state; field: string; fun: lua_CFunction);
begin
  lua_pushcfunction(L, fun);
  lua_setfield(L, -2, PChar(field));
end;

procedure LuaH_error_handle(L: Plua_state);
var
  msg : String;
const
  NL = #10#13;
begin
  msg := lua_tostring(L,-1);
  ShowMessage('Lua error:' + NL + msg);
end;

procedure TluaMenuItem.Clicked(Sender: TObject);
begin
  lua_getglobal(L, PChar(LuaFunctionName));
  if lua_pcall(L,0,0,0)<>LUA_OK then
  begin
    LuaH_error_handle(L);
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  nitem : TLuaMenuItem;
begin
  L := luaL_newstate(); (* 5.2 change *)
  luaL_openlibs(L);
  lua_newtable(L);
  LuaH_table_set_function(L, 'alert', @svlad_alert);
  LuaH_table_set_function(L, 'confirm', @svlad_confirm);
  LuaH_table_set_function(L, 'prompt', @svlad_prompt);
  LuaH_table_set_function(L, 'get', @svlad_get);
  LuaH_table_set_function(L, 'set', @svlad_set);
  LuaH_table_set_function(L, 'clean', @svlad_clean);
  LuaH_table_set_function(L, 'menu_add', @svlad_menu_add);
  lua_setglobal(L, 'svlad');
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  lua_close(L);
end;

procedure TForm1.GridSetEditText(Sender: TObject; ACol, ARow: integer;
  const Value: string);
begin
  if ARow > maxR then
    maxR := ARow;
  if ACol > maxC then
    maxC := ACol;
end;

procedure TForm1.MenuSheetClearClick(Sender: TObject);
begin
  Grid.Clean;
  maxR := 0;
  maxC := 0;
end;

procedure TForm1.MenuSheetFixedColClick(Sender: TObject);
begin
  MenuSheetFixedCol.checked := not MenuSheetFixedCol.checked;
  if (MenuSheetFixedCol.Checked) then
    Form1.Grid.FixedCols:=1
  else
    Form1.Grid.FixedCols:=0;
end;

procedure TForm1.MenuSheetFixedRowClick(Sender: TObject);
begin
  MenuSheetFixedRow.checked := not MenuSheetFixedRow.checked;
  if (MenuSheetFixedRow.Checked) then
    Form1.Grid.FixedRows:=1
  else
    Form1.Grid.FixedRows:=0;
end;

procedure TForm1.MenuSheetOpenClick(Sender: TObject);
var
  Parser: TCSVParser;
  FileStream : TFileStream;
begin
  if OpenDialogTable.Execute then
  begin
    maxR := 0;
    maxC := 0;
    Grid.Clean;
    Parser := TCSVParser.create;
    if not(FileExistsUTF8(OpenDialogTable.FileName)) then exit;
    Parser:=TCSVParser.Create;
    FileStream := TFileStream.Create(OpenDialogTable.FileName, fmOpenRead+fmShareDenyWrite);
    try
      Parser.Delimiter:=TAB;
      Parser.SetSource(FileStream);
      Grid.BeginUpdate;
      while Parser.ParseNextCell do
      begin
        Grid.Cells[Parser.CurrentCol,Parser.CurrentRow] := Parser.CurrentCellText;
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
  Builder : TCSVBuilder;
  FileStream : TFileStream;
  r, c : Integer;
begin
  if SaveDialogTable.Execute then
  begin
    Builder := TCSVBuilder.Create;
    FileStream := TFileStream.Create(SaveDialogTable.FileName, fmOpenWrite+fmCreate);
    try
      Builder.Delimiter:=TAB;
      Builder.SetOutput(FileStream);
      for r:=0 to maxR do
      begin
        for c:=0 to maxC do
        begin
          Builder.AppendCell(Grid.Cells[c,r])
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
  msg: PChar;
begin
  code := Query.Lines.GetText;
  if Length(code)>0 then
  begin
    if luaL_dostring(L, code)<>0 then
    begin
      msg := lua_tostring(L,-1);
      ShowMessage('Lua error: ' + msg);
    end;
  end;
end;

procedure TForm1.PopupQueryClearClick(Sender: TObject);
begin
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
  ShowMessage('Svlad2 release ' + IntToStr(SVLAD_RELEASE) + ' by Severák');
end;

end.
