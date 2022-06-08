unit LexType; {!!! ������� �� �������� ����� !!!}

interface
{ ������, ���������� �������� ���� ����� ������ }

type
{ ��������� ���� ������ � ��������� }
TLexType = (
  LEX_SEMI, LEX_IF, LEX_THEN, LEX_ELSE, LEX_VAR, LEX_CONST,
  LEX_ASSIGN, LEX_OR, LEX_XOR, LEX_AND,
  LEX_OPEN, LEX_CLOSE, LEX_START);

{ ������� ��������� ������ ������������ ���� ������� }
function LexTypeName(lexT: TLexType): string;
{ ������� ��������� ��������� ���������� � ���� ������� }
function LexTypeInfo(lexT: TLexType): string;

implementation

function LexTypeName(lexT: TLexType): string;
{ ������� ��������� ������ ������������ ���� ������� }
begin
  case lexT of
    LEX_OPEN:   Result := '����������� ������';
    LEX_CLOSE:  Result := '����������� ������';
    LEX_ASSIGN: Result := '���� ����������';
    LEX_VAR:    Result := '����������';
    LEX_CONST:  Result := '���������';
    LEX_SEMI:   Result := '�����������';
    else        Result := '�������� �����';
  end;
end;

function LexTypeInfo(lexT: TLexType): string;
{ ������� ��������� ��������� ���������� � ���� ������� }
begin
  case lexT of
    LEX_IF:     Result := 'if';
    LEX_THEN:   Result := 'then';
    LEX_ELSE:   Result := 'else';
    LEX_OR:     Result := 'or';
    LEX_XOR:    Result := 'xor';
    LEX_AND:    Result := 'and';
    LEX_OPEN:   Result := '(';
    LEX_CLOSE:  Result := ')';
    LEX_ASSIGN: Result := ':=';
    LEX_SEMI:   Result := ';';
    LEX_START:  Result := '';
    else        Result := 'a';
  end;
end;

end.
