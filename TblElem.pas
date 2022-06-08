unit TblElem;

interface
{ ������, ����������� ��������� ������ ���������
  ������� ��������������� }

type

{ ����� ��� �������� ��������� ������,
  ��������� � ��������� ������� }
TAddVarInfo = class(TObject)
 public
  procedure SetInfo(iIdx: integer; iInfo: longint);
   virtual; abstract;
  function GetInfo(iIdx: integer): longint;
   virtual; abstract;
  property Info[iIdx: integer]: longint
   read GetInfo write SetInfo; default;
end;

{ ����� ��� �������� �������� ���-������� }
TVarInfo = class(TObject)
 protected
  { ��� �������� }
  sName: string;
  { �������������� ���������� (��� �������� �������������)}
  pInfo: TAddVarInfo;
  { ������ �� ������� � ������� ��������
    ��� ����������� ��������� ������ }
  minEl,maxEl: TVarInfo;
 public
  { ����������� �������� �������� ���-������� }
  constructor Create(const sN: string);
  { ���������� ��� ������������ ������, ������� ��������� }
  destructor Destroy; override;
  { ������� ���������� �������������� ���������� �������� }
  procedure SetInfo(pI: TAddVarInfo);
  { ������� ��� �������� �������������� ���������� }
  procedure ClearInfo;
  procedure ClearAllInfo;
  { �������� "��� ��������" }
  property VarName: string read sName;
  { �������� "�������������� ����������"
   (��� �������� �������������) }
  property Info: TAddVarInfo read pInfo write SetInfo;
  { ������� ��� ���������� �������� � �������� ������ }
  function AddElCnt(const sAdd: string;
                    var iCnt: integer): TVarInfo;
  function AddElem(const sAdd: string): TVarInfo;
  { ������� ��� ������ �������� � �������� ������ }
  function FindElCnt(const sN: string;
                     var iCnt: integer): TVarInfo;
  function FindElem(const sN: string): TVarInfo;
  { ������� ������ ���� ���� ���������������
    � ���� ������ }
  function GetElList(const sLim,sInp,sOut: string): string;
end;

function Upper(const x:string): string;

implementation

uses SysUtils;

{ �������� ����������: ���� ���������� ��� REGNAME,
  �� ����� ���������� ��������� ������������������,
  ����� - �������������������� }
{$IFDEF REGNAME}
function Upper(const x:string): string;
begin Result := UpperCase(x); end;
{$ELSE}
function Upper(const x:string): string;
begin Result := x; end;
{$ENDIF}

constructor TVarInfo.Create(const sN: string);
{ ����������� �������� �������� ���-������� }
begin
  { �������� ����������� �������� ������ TObject }
  inherited Create;
  { ���������� ��� �������� � �������� ��� ������ }
  sName := sN;
  pInfo := nil;
  minEl := nil;
  maxEl := nil;
end;

destructor TVarInfo.Destroy;
{ ���������� ��� ������������ ������, ������� ��������� }
begin
  { ����������� ������ �� ������ ������
   (���� ��� �� �������), ��� ���� � ������ ����������
   ����� ����������� ������ ��� ���� ��������� }
  ClearAllInfo;
  minEl.Free;
  maxEl.Free;
  { �������� ���������� �������� ������ TObject }
  inherited Destroy;
end;

function TVarInfo.GetElList(const sLim{����������� ������},
 sInp,sOut{�����, �� ���������� � ������}: string): string;
{ ������� ������ ���� ���� ��������������� � ���� ������ }
var sAdd: string;
begin
  { ������������� ������ ����� }
  Result := '';
  { ���� ������� ������� �� ��������� � �����
    �� ������������ ����, �� ��� ����� �������� � ������ }
  if (Upper(sName) <> Upper(sInp))
  and (Upper(sName) <> Upper(sOut)) then Result := sName;
  { ���� ���� ����� ����� ������ }
  if minEl <> nil then
  begin
    { ��������� ������ ��� ���� ����� }
    sAdd := minEl.GetElList(sLim,sInp,sOut);
    { ���� ��� �� ������, ��������� �� ����� ����������� }
    if sAdd <> '' then
    begin
      if Result <> '' then Result := Result + sLim + sAdd
                      else Result := sAdd;
    end;
  end;
  { ���� ���� ������ ����� ������ }
  if maxEl <> nil then
  begin
    { ��������� ������ ��� ���� ����� }
    sAdd := maxEl.GetElList(sLim,sInp,sOut);
    { ���� ��� �� ������, ��������� �� ����� ����������� }
    if sAdd <> '' then
    begin
      if Result <> '' then Result := Result + sLim + sAdd
                      else Result := sAdd;
    end;
  end;
end;

procedure TVarInfo.SetInfo(pI: TAddVarInfo);
{ ������� ���������� �������������� ���������� �������� }
begin
  pInfo := pI;
end;

procedure TVarInfo.ClearInfo;
{ ������� �������� �������������� ���������� �������� }
begin
  pInfo.Free;
  pInfo := nil;
end;

procedure TVarInfo.ClearAllInfo;
{ ������� �������� �������������� ���������� ��������
  � ��� ������ }
begin
  if minEl <> nil then minEl.ClearAllInfo;
  if maxEl <> nil then maxEl.ClearAllInfo;
  pInfo.Free;
  pInfo := nil;
end;

function TVarInfo.AddElCnt(const sAdd: string;
                           var iCnt: integer): TVarInfo;
{ ������� ���������� �������� � �������� ������
  � ������ �������� ��������� }
var i: integer;
begin
  { ����������� ������� ��������� }
  Inc(iCnt);
  { ���������� ����� ������ � �������� ��������
    (����� ��������!) }
  i := StrComp(PChar(Upper(sAdd)),PChar(Upper(sName)));
  if i < 0 then
  { ���� ����� ������� ������, ������� ������
    �� ������� ������� }
  begin
    { ���� ������ �� ������, ���������� �������� �������
      ���������� �������� }
    if minEl <> nil then
     Result := minEl.AddElCnt(sAdd,iCnt)
    else
    { ���� ������ ������, ������� ����� �������
      � ���������� ������ �� ���� }
    begin
       Result := TVarInfo.Create(sAdd);
       minEl := Result;
    end;
  end
  else
  { ���� ����� ������� ������, ������� ������
    �� ������� ������� }
  if i > 0 then
  begin
    { ���� ������ �� ������, ���������� �������� �������
      ���������� �������� }
    if maxEl <> nil then
     Result := maxEl.AddElCnt(sAdd,iCnt)
    else
    { ���� ������ ������, ������� ����� �������
      � ���������� ������ �� ���� }
    begin
       Result := TVarInfo.Create(sAdd);
       maxEl := Result;
    end;
  end
  { ���� ����� ���������, �� ����� ������� ��� ����
    � ������ - ��� ������� ������� }
  else Result := Self;
end;

function TVarInfo.AddElem(const sAdd: string): TVarInfo;
{ ������� ���������� �������� � �������� ������
  ��� ����� �������� ��������� }
var iCnt: integer;
begin
  Result := AddElCnt(sAdd,iCnt);
end;

function TVarInfo.FindElCnt(const sN: string;
                            var iCnt: integer): TVarInfo;
{ ������� ������ �������� � �������� ������
  � ������ �������� ��������� }
var i: integer;
begin
  { ����������� ������� ��������� }
  Inc(iCnt);
  { ���������� ����� �������� � �������� ��������
   (����� ��������!) }
  i := StrComp(PChar(Upper(sN)),PChar(Upper(sName)));
  if i < 0 then
  { ���� ������� ������� ������, ������� ������
    �� ������� ������� }
  begin
    { ���� ������ �� ������, ���������� �������� ��� ���
      ������� ������ ��������, ����� ������� �� ������ }
    if minEl <> nil then Result := minEl.FindElCnt(sN,iCnt)
                    else Result := nil;
  end
  else
  if i > 0 then
  { ���� ������� ������� ������, ������� ������
    �� ������� ������� }
  begin
    { ���� ������ �� ������, ���������� �������� ��� ���
      ������� ������ ��������, ����� ������� �� ������ }
    if maxEl <> nil then Result := maxEl.FindElCnt(sN,iCnt)
                    else Result := nil;
  end
  { ���� ����� ���������, �� ������� �������
    ������ - ��� ������� ������� }
  else Result := Self;
end;

function TVarInfo.FindElem(const sN: string): TVarInfo;
{ ������� ������ �������� � �������� ������
  ��� ����� �������� ��������� }
var iCnt: integer;
begin
  Result := FindElCnt(sN,iCnt);
end;

end.
