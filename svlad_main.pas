unit svlad_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Menus, Grids, lua52, CSVDocument;

type

  { TForm1 }

  TForm1 = class(TForm)
    MainMenu: TMainMenu;
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
    procedure MenuSheetClearClick(Sender: TObject);
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

function svlad_alert(L: Plua_state): Integer; cdecl;
var
  text : String;
begin
  text := lua_tostring(L, -1);
  ShowMessage(text);
  Result:=0;
end;

function svlad_get(L: Plua_state): Integer; cdecl;
var
  row, col : Integer;
  text : String;
begin
  row := lua_tointeger(L, -2);
  col := lua_tointeger(L, -1);
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

procedure lua_help_register(L: PLua_state; table: String; field: String; fun: lua_CFunction);
begin
  lua_getglobal(L, PChar(table));
  lua_pushcfunction(L, fun);
  lua_setfield(L, -2, PChar(field));
  lua_pop(L,1);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  L := luaL_newstate(); (* 5.2 change *)
  luaL_openlibs(L);
  lua_newtable(L);
  lua_setglobal(L, 'svlad');
  lua_help_register(L, 'svlad', 'alert', @svlad_alert);
  lua_help_register(L, 'svlad', 'get', @svlad_get);
  lua_help_register(L, 'svlad', 'set', @svlad_set);
  lua_help_register(L, 'svlad', 'clean', @svlad_clean);
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
    Query.Lines.SaveToFile(OpenDialogQuery.FileName);
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


end.

