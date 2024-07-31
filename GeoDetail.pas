unit GeoDetail;

interface

uses
    Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
    StdCtrls, ExtCtrls, Buttons;

type
    TF_GeoTiffKeyDetail = class(TForm)
        L_GeoKeyTag: TLabel;
        L_GeoKeyName: TLabel;
        L_GeoKeyType: TLabel;
        L_GeoKeyCount: TLabel;
        L_GeoKeyValue: TLabel;
        L_GeoKeyDescription: TLabel;
        E_GeoKeyTag: TEdit;
        E_GeoKeyProperty: TEdit;
        E_GeoKeyType: TEdit;
        E_GeoKeyCount: TEdit;
        M_GeoKeyValues: TMemo;
        M_GeoKeyDescription: TMemo;
        BB_Close: TBitBtn;
        P_GeoKey: TPanel;
        procedure FormCreate(Sender: TObject);
    private
        { Private declarations }
    public
        { Public declarations }
    end;

var
    F_GeoTiffKeyDetail: TF_GeoTiffKeyDetail;

implementation

{$R *.DFM}

procedure TF_GeoTiffKeyDetail.FormCreate(Sender: TObject);
begin
    E_GeoKeyTag.Text := '';
    E_GeoKeyProperty.Text := '';
    E_GeoKeyType.Text := '';
    E_GeoKeyCount.Text := '';
    M_GeoKeyValues.Lines.Clear;
    M_GeoKeyDescription.Lines.Clear;
end;

end.
