unit LexElem;

interface
{ ������, ����������� ��������� ��������� ������� ������ }

uses Classes, TblElem, LexType;

type

{ ��������� ��� ���������� � �������� }
TLexInfo = record
  case LexType: TLexType of
    LEX_VAR: (VarInfo: TVarInfo);
    LEX_CONST: (ConstVal: integer);
    LEX_START: (szInfo: PChar);
end;

{ ��������� ��� �������� ������� }
TLexem = class(TObject)
 protected
  { ���������� � ������� }
  LexInfo: TLexInfo;
  { ������� ������� � �������� ������ ��������� }
  iStr,iPos,iAllP: integer;
 public
  { ������������ ��� �������� ������ ������ �����}
  constructor CreateKey(LexKey: TLexType;
                        iA,iSt,iP: integer);
  constructor CreateVar(VarInf: TVarInfo;
                        iA,iSt,iP: integer);
  constructor CreateConst(iVal: integer;
                          iA,iSt,iP: integer);
  constructor CreateInfo(sInf: string;
                         iA,iSt,iP: integer);
  destructor Destroy; override;
  { �������� ��� ��������� ���������� � ������� }
  property LexType: TLexType read LexInfo.LexType;
  property VarInfo: TVarInfo read LexInfo.VarInfo;
  property ConstVal: integer read LexInfo.ConstVal;
  { �������� ��� ������ ������� �������
    � �������� ������ ��������� }
  property StrNum: integer read iStr;
  property PosNum: integer read iPos;
  property PosAll: integer read iAllP;
  { ��������� ���������� � ���� ������� }
  function LexInfoStr: string;
  { ��� ��� �������-���������� }
  function VarName: string;
end;

{ ��������� ��� �������� ������ ������ }
TLexList = class(TList)
 public
  { ���������� ��� ������������ ������
    ��� ����������� ������ }
  destructor Destroy; override;
  { ��������� ������� ������ }
  procedure Clear; override;
  { ��������� � �������� ��� ��������� ����������
    � ������� �� �� ������ }
  function GetLexem(iIdx: integer): TLexem;
  property Lexem[iIdx: integer]: TLexem read GetLexem;
           default;
end;

implementation

uses SysUtils, LexAuto;

constructor TLexem.CreateKey(LexKey: TLexType;
                             iA,iSt,iP: integer);
{ ����������� �������� ������� ���� "�������� �����" }
begin
  inherited Create;
  LexInfo.LexType := LexKey;
  { ���������� ��� ��������� ����� }
  iStr := iSt; { ���������� ������� ������� }
  iPos := iP;
  iAllP := iA;
end;

constructor TLexem.CreateVar(VarInf: TVarInfo;
                             iA,iSt,iP: integer);
{ ����������� �������� ������� ���� "�������� �����" }
begin
  inherited Create;
  LexInfo.LexType := LEX_VAR; { ��� - "����������" }
  { ���������� ������ �� ������� ��������������� }
  LexInfo.VarInfo := VarInf;
  iStr := iSt; { ���������� ������� ������� }
  iPos := iP;
  iAllP := iA;
end;

constructor TLexem.CreateConst(iVal: integer;
                               iA,iSt,iP: integer);
{ ����������� �������� ������� ���� "���������" }
begin
  inherited Create;
  LexInfo.LexType := LEX_CONST; { ��� - "���������" }
  { ���������� �������� ��������� }
  LexInfo.ConstVal := iVal;
  iStr := iSt; { ���������� ������� ������� }
  iPos := iP;
  iAllP := iA;
end;

constructor TLexem.CreateInfo(sInf: string;
                              iA,iSt,iP: integer);
{ ����������� �������� �������������� ������� }
begin
  inherited Create;
  LexInfo.LexType := LEX_START; { ��� - "���. �������" }
  { �������� ������ ��� ���������� }
  LexInfo.szInfo :=  StrAlloc(Length(sInf)+1);
  { ���������� ���������� }
  StrPCopy(LexInfo.szInfo,sInf);
  iStr := iSt; { ���������� ������� ������� }
  iPos := iP;
  iAllP := iA;
end;

destructor TLexem.Destroy;
begin
  { ����������� ������� ������,
    ���� ��� �������������� ������� }
  if LexType = LEX_START then StrDispose(LexInfo.szInfo);
  inherited Destroy;
end;

function TLexem.VarName: string;
{ ������� ��������� ����� ������� ���� "����������" }
begin
  Result := VarInfo.VarName;
end;

function TLexem.LexInfoStr: string;
{ ��������� ���������� � ���� ������� }
begin
  case LexType of
    LEX_VAR:   Result := VarName;
    { ��� ���������� - �� ���  }
    LEX_CONST: Result := IntToStr(ConstVal);
    { ��� ��������� - �������� }
    LEX_START: Result := StrPas(LexInfo.szInfo);
    { ��� ���. ������� - ���������� }
    else       Result := LexTypeInfo(LexType);
    { ��� ��������� - ��� ���� }
  end;
end;

procedure TLexList.Clear;
{ ��������� ������� ������ }
var i: integer;
begin
  { ���������� ��� �������� ������ }
  for i:=Count-1 downto 0 do Lexem[i].Free;
  inherited Clear; { �������� ������� �������� ������ }
end;

destructor TLexList.Destroy;
{ ���������� ��� ������������ ������
  ��� ����������� ������ }
begin
  Clear; { ���������� ��� �������� ������ }
  { �������� ���������� �������� ������ }
  inherited Destroy;
end;

function TLexList.GetLexem(iIdx: integer): TLexem;
{ ��������� ������� �� ������ �� �� ������ }
begin
  Result := TLexem(Items[iIdx]);
end;

end.

