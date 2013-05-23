unit SvladAbout;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls;

type

  { TFormSvladAbout }

  TFormSvladAbout = class(TForm)
    Button1: TButton;
    Image1: TImage;
    Label1: TLabel;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  FormSvladAbout: TFormSvladAbout;

implementation

{$R *.lfm}

{ TFormSvladAbout }

procedure TFormSvladAbout.Button1Click(Sender: TObject);
begin
  FormSvladAbout.Close;
end;

end.

