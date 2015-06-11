unit Unit2;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Magnetic, StdCtrls;

type
  TForm2 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

uses Unit1;

{$R *.dfm}

procedure TForm2.Button1Click(Sender: TObject);
begin
  MagneticWnd.RemoveWindow(Handle);
end;

procedure TForm2.Button2Click(Sender: TObject);
begin
  MagneticWnd.AddWindow(Handle,Form1.Handle);
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  MagneticWnd.AddWindow(Handle,Form1.Handle);
end;

end.
