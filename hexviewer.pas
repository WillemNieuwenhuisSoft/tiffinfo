unit hexviewer;

interface

uses
  Winapi.Windows, System.Character, System.SysUtils, System.Variants, System.Classes;

type
  THexConvert = class
  private
    { Private declarations }
    _nrbytes : integer;
  public
    { Public declarations }
    property groupby : integer read _nrbytes write _nrbytes;
    function BytesToHex(bytes:TBytes) : String;
  end;

var
    hexconvert : THexConvert;

implementation


{ THexConvert }


function THexConvert.BytesToHex(bytes: TBytes) : String;
var
    pos,
    total : integer;
    all : TStringBuilder;
    l, todo: Integer;
    s : String;
begin
    pos := 0;
    total := length(bytes);
    all := TStringBuilder.Create;
    if total > MAXSHORT then begin
        all.AppendLine('More than 32000 bytes, not displaying all');
        all.AppendLine('-----------');
        total := MAXSHORT;
    end;
    while pos < total do begin
        // Address
        all.Append(StringReplace(Format('%4x:', [pos]),' ','0',[rfReplaceAll]));
        all.Append('  ');
        todo := 15;
        if total - pos < 15 then todo := total - pos;
        s := '';
        for l := 0 to todo do begin
            if isLetterOrDigit(bytes[pos + l]) then
                s := s + char(bytes[pos + l])
            else
                s := s + '.';
            
            all.Append(StringReplace(Format('%2x', [integer(bytes[pos + l])]), ' ', '0', [rfReplaceAll]));
            all.Append('  ');
            if l = 7 then all.Append('    ');
        end;
        if todo < 7 then
            all.Append('    ');
        if todo < 15 then
            all.AppendFormat('%*s', [4 * (15 - todo), ' ']);
        all.Append('    ');
        all.AppendLine(s);
        pos := pos + 16;
    end;
    result := all.ToString;
end;

end.
