unit Details;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.Buttons;

type
  TF_Detail = class(TForm)
    P_Detail: TPanel;
    BB_DetailClose: TBitBtn;
    M_Detail: TMemo;
    L_DetailTag: TLabel;
    E_DetailTag: TEdit;
    P_DetailTag: TPanel;
    P_Button: TPanel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  F_Detail: TF_Detail;

implementation

{$R *.dfm}

end.
