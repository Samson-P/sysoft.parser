unit FncTree;

interface
{ ������, �������������� ������ � ��������������� ��������
  ���������������, ����������� �� ������ ���-������� �
  ��������� ������ }

uses TblElem;

{ ������� ��������� ������������� ���-������� }
procedure InitTreeVar;
{ ������� ������������ ������ ���-������� }
procedure ClearTreeVar;
{ ������� �������� �������������� ���������� � ������� }
procedure ClearTreeInfo;
{ ���������� �������� � ������� ��������������� }
function AddTreeVar(const sName: string): TVarInfo;
{ ����� �������� � ������� ��������������� }
function GetTreeVar(const sName: string): TVarInfo;
{ �������, ������������ ���������� �������� ��������� }
function GetTreeCount: integer;

{ ������� ������ ���� ���� ��������������� � ���� ������ }
function IdentList(const sLim,sInp,sOut: string): string;

implementation

const
{ ����������� � ������������ �������� ���-�������
 (���������� ���� �������� �������� ���-�������) }
  HASH_MIN = Ord('0')+Ord('0');
  HASH_MAX = Ord('z')+Ord('z');

var
  HashArray : array[HASH_MIN..HASH_MAX] of TVarInfo;
{ ������ ��� ���-������� }
  iCmpCount : integer;
{ ������� ���������� ��������� }

function GetTreeCount: integer;
begin
  Result := iCmpCount;
end;

function IdentList(const sLim,sInp,sOut: string): string;
{ ������� ������ ���� ���� ��������������� � ���� ������ }
var
  i: integer;   { ������� ��������������� }
  sAdd: string; { ����� ��� ���������� �������� ������ }
begin
  { ������������� ������ ����� }
  Result := '';
  { ���� �� ���� ��������������� � ������� }
  for i:=HASH_MIN to HASH_MAX do
  begin
    { ���� ������ ������� ������, �� ��������� ������
      �� �����, }
    if HashArray[i] = nil then sAdd := ''
    { ����� ��������� ���������� ����� ������ }
    else sAdd := HashArray[i].GetElList(sLim,sInp,sOut);
    { ���� ���������� ����� ������ �� �����,
      �� ��������� �� � ����� ������ ����� ����������� }
    if sAdd <> '' then
    begin
      if Result <> '' then Result := Result + sLim + sAdd
                      else Result := sAdd;
    end;
  end{for};
end;

function VarHash(const sName: string): longint;
{ ��� ������� - ����� ����� ������� � ��������
  �������� ������ }
begin
  Result := (Ord(sName[1])
           + Ord(sName[(Length(sName)+1) div 2])
           - HASH_MIN) mod (HASH_MAX-HASH_MIN+1)+HASH_MIN;
  if Result < HASH_MIN then Result := HASH_MIN;
end;

procedure InitTreeVar;
{ ��������� ������������� ���-������� -
  ��� �������� ������ }
var i : integer;
begin
  for i:=HASH_MIN to HASH_MAX do HashArray[i] := nil;
end;

procedure ClearTreeVar;
{ ������������ ������ ��� ���� ��������� ���-������� }
var i : integer;
begin
  for i:=HASH_MIN to HASH_MAX do
  begin
    HashArray[i].Free;
    HashArray[i] := nil;
  end;
end;

procedure ClearTreeInfo;
{ �������� �������������� ���������� ��� ����
  ��������� ���-������� }
var i : integer;
begin
  for i:=HASH_MIN to HASH_MAX do
   if HashArray[i] <> nil then HashArray[i].ClearAllInfo;
end;

function AddTreeVar(const sName: string): TVarInfo;
{ ���������� �������� � ���-������� � ������ }
var iHash: integer;
begin
  { �������� ������� ���������� ��������� }
  iCmpCount := 0;
  { ��������� ���-����� � ������� ���-������� }
  iHash := VarHash(Upper(sName));
  if HashArray[iHash] <> nil then
   Result := HashArray[iHash].AddElCnt(sName,iCmpCount)
  else
  begin
    Result := TVarInfo.Create(sName);
    HashArray[iHash] := Result;
  end;
end;

function GetTreeVar(const sName: string): TVarInfo;
var iHash: integer;
begin
  iCmpCount := 0;
  iHash := VarHash(Upper(sName));
  if HashArray[iHash] = nil then Result := nil
  else
   Result := HashArray[iHash].FindElCnt(sName,iCmpCount)
end;

initialization
{ ����� ��������� ������������� �������
  ��� �������� ������ }
  InitTreeVar;

finalization
{ ����� ������������ ������ ������� ��� �������� ������ }
  ClearTreeVar;

end.
