unit Unit3;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Magnetic, StdCtrls;

type
  TForm3 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

uses Unit1;

{$R *.dfm}

procedure TForm3.Button1Click(Sender: TObject);
begin
  MagneticWnd.RemoveWindow(Handle);
end;

procedure TForm3.Button2Click(Sender: TObject);
begin
  MagneticWnd.AddWindow(Handle,Form1.Handle);
end;

procedure TForm3.FormCreate(Sender: TObject);
begin
  MagneticWnd.AddWindow(Handle,Form1.Handle);
end;

end.
