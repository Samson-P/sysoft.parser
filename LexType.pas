unit LexType; {!!! Зависит от входного языка !!!}

interface
{ Модуль, содержащий описание всех типов лексем }

type
{ Возможные типы лексем в программе }
TLexType = (
  LEX_SEMI, LEX_IF, LEX_THEN, LEX_ELSE, LEX_VAR, LEX_CONST,
  LEX_ASSIGN, LEX_OR, LEX_XOR, LEX_AND,
  LEX_OPEN, LEX_CLOSE, LEX_START);

{ Функция получения строки наименования типа лексемы }
function LexTypeName(lexT: TLexType): string;
{ Функция получения текстовой информации о типе лексемы }
function LexTypeInfo(lexT: TLexType): string;

implementation

function LexTypeName(lexT: TLexType): string;
{ Функция получения строки наименования типа лексемы }
begin
  case lexT of
    LEX_OPEN:   Result := 'Открывающая скобка';
    LEX_CLOSE:  Result := 'Закрывающая скобка';
    LEX_ASSIGN: Result := 'Знак присвоения';
    LEX_VAR:    Result := 'Переменная';
    LEX_CONST:  Result := 'Константа';
    LEX_SEMI:   Result := 'Разделитель';
    else        Result := 'Ключевое слово';
  end;
end;

function LexTypeInfo(lexT: TLexType): string;
{ Функция получения текстовой информации о типе лексемы }
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
