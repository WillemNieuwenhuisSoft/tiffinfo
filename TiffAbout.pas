unit TiffAbout;

interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
    Buttons, ExtCtrls, Menus, Vcl.Imaging.jpeg; // , rxverinf;

type
    TAboutBox = class(TForm)
        Panel1: TPanel;
        ProgramIcon: TImage;
        ProductName: TLabel;
        L_Version: TLabel;
        Copyright: TLabel;
        Comments: TLabel;
        OKButton: TButton;
        L_Build: TLabel;
        LL_ImageAttribution: TLinkLabel;
        procedure FormCreate(Sender: TObject);
        procedure LL_ImageAttributionLinkClick(Sender: TObject;
          const Link: string; LinkType: TSysLinkType);
    private
        { Private declarations }
    public
        { Public declarations }
    end;

var
    AboutBox: TAboutBox;

implementation

uses shellapi;

{$R *.DFM}

procedure TAboutBox.FormCreate(Sender: TObject);
var
    exe: string;
    fdate: integer;
    dt: TDateTime;
    y, m, d: word;
begin
    exe := Application.ExeName;
    fdate := FileAge(exe);
    dt := FileDateToDateTime(fdate);
    DecodeDate(dt, y, m, d);
    L_Build.Caption := 'Build: ' + format('%2d.%2d.%2d', [y mod 100, m, d]);
end;

procedure TAboutBox.LL_ImageAttributionLinkClick(Sender: TObject;
  const Link: string; LinkType: TSysLinkType);
begin
    shellapi.ShellExecute(0, 'Open', PChar(Link), nil, nil, SW_SHOWNORMAL);
end;

end.
