program TiffInfo;

uses
  Forms,
  TiffInfoMain in 'TiffInfoMain.pas' {formTiffInfo},
  ReadTiff in 'ReadTiff.pas',
  TiffAbout in 'TiffAbout.pas' {AboutBox},
  GeoDetail in 'GeoDetail.pas' {F_GeoTiffKeyDetail},
  Options in 'Options.pas' {F_Options},
  Details in 'Details.pas' {F_Detail},
  hexviewer in 'hexviewer.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Tiff Info Viewer';
  Application.CreateForm(TformTiffInfo, formTiffInfo);
  Application.CreateForm(TAboutBox, AboutBox);
  Application.CreateForm(TF_GeoTiffKeyDetail, F_GeoTiffKeyDetail);
  Application.CreateForm(TF_Detail, F_Detail);
  Application.Run;
end.
