unit svlad_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Menus, Grids, lua52;

type

  { TForm1 }

  TForm1 = class(TForm)
    MainMenu: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuFile: TMenuItem;
    MenuFileOpen: TMenuItem;
    MenuFileSave: TMenuItem;
    OpenDialogTable: TOpenDialog;
    Query: TMemo;
    PopupMenu1: TPopupMenu;
    Grid: TStringGrid;
    SaveDialogTable: TSaveDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure GridEditingDone(Sender: TObject);
    procedure GridSetEditText(Sender: TObject; ACol, ARow: integer;
      const Value: string);
    procedure MenuFileOpenClick(Sender: TObject);
    procedure MenuFileSaveClick(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuFileClick(Sender: TObject);
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

implementation

{$R *.lfm}

{ TForm1 }


procedure TForm1.FormCreate(Sender: TObject);
begin
  L := luaL_newstate(); (* 5.2 change *)
  luaL_openlibs(L);
  maxR := 1;
  maxC := 1;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  lua_close(L);
end;

procedure TForm1.GridEditingDone(Sender: TObject);
begin

end;

procedure TForm1.GridSetEditText(Sender: TObject; ACol, ARow: integer;
  const Value: string);
begin
  if ARow > maxR then
    maxR := ARow;
  if ACol > maxC then
    maxC := ACol;

end;

procedure TForm1.MenuFileOpenClick(Sender: TObject);
begin
  if OpenDialogTable.Execute then
  begin
    Grid.LoadFromCSVFile(OpenDialogTable.FileName);
  end;
end;

procedure TForm1.MenuFileSaveClick(Sender: TObject);
var
  txt: string;
begin
  Str(maxR, txt);
  ShowMessage('Max R:' + txt);
  Str(maxC, txt);
  ShowMessage('Max C:' + txt);
  if SaveDialogTable.Execute then
  begin
    Grid.SaveToCSVFile(SaveDialogTable.FileName);
  end;
end;

procedure TForm1.MenuItem1Click(Sender: TObject);
var
  code: PChar;
begin
  code := Query.Lines.GetText;
  luaL_dostring(L, code);
end;

procedure TForm1.MenuItem2Click(Sender: TObject);
begin
  Query.Clear;
end;

procedure TForm1.MenuFileClick(Sender: TObject);
begin

end;

end.

