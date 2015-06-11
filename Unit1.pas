unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Magnetic, StdCtrls, ComCtrls;

type
  TForm1 = class(TForm)
    TrackBar1: TTrackBar;
    Label1: TLabel;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses Unit2;

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  MagneticWnd.AddWindow(Handle,0);
end;

procedure TForm1.TrackBar1Change(Sender: TObject);
begin
 MagneticWnd.SnapWidth:=TrackBar1.Position;
 Label1.Caption:=inttostr(TrackBar1.Position);
end;

end.
