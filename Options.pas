unit Options;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Mask, ExtCtrls, Buttons;//, PBFolderDialog; //ToolEdit;

type
  TF_Options = class(TForm)
//    FNE_Reportfile: TFilenameEdit;
    P_Options: TPanel;
    L_ReportFile: TLabel;
    BB_Cancel: TBitBtn;
    BB_OK: TBitBtn;
    E_Reportfile: TEdit;
    BB_Browse: TBitBtn;
    OD_Browse: TOpenDialog;
    procedure FormActivate(Sender: TObject);
    procedure BB_BrowseClick(Sender: TObject);
    procedure BB_OKClick(Sender: TObject);
  private
    { Private declarations }
    m_Filename : TFilename;
    procedure SetFilename(Value : TFileName);
  public
    { Public declarations }
    property Filename : TFileName read m_Filename write SetFilename;
  end;

//var
//  F_Options: TF_Options;

implementation

{$R *.DFM}

{ TF_Options }

procedure TF_Options.SetFilename(Value: TFileName);
begin
    m_Filename := value;
end;

procedure TF_Options.FormActivate(Sender: TObject);
begin
    E_Reportfile.Text := m_Filename;
end;

procedure TF_Options.BB_BrowseClick(Sender: TObject);
begin
    OD_Browse.InitialDir := ExtractFilePath(E_Reportfile.Text);
    if OD_Browse.Execute then
        E_Reportfile.Text := OD_Browse.FileName;
end;

procedure TF_Options.BB_OKClick(Sender: TObject);
begin
    m_Filename := E_Reportfile.Text;
end;

end.
