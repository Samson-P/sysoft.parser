unit LexElem;

interface
{ Модуль, описывающий структуру элементов таблицы лексем }

uses Classes, TblElem, LexType;

type

{ Структура для информации о лексемах }
TLexInfo = record
  case LexType: TLexType of
    LEX_VAR: (VarInfo: TVarInfo);
    LEX_CONST: (ConstVal: integer);
    LEX_START: (szInfo: PChar);
end;

{ Структура для описания лексемы }
TLexem = class(TObject)
 protected
  { Информация о лексеме }
  LexInfo: TLexInfo;
  { Позиция лексемы в исходном тексте программы }
  iStr,iPos,iAllP: integer;
 public
  { Конструкторы для создания лексем разных типов}
  constructor CreateKey(LexKey: TLexType;
                        iA,iSt,iP: integer);
  constructor CreateVar(VarInf: TVarInfo;
                        iA,iSt,iP: integer);
  constructor CreateConst(iVal: integer;
                          iA,iSt,iP: integer);
  constructor CreateInfo(sInf: string;
                         iA,iSt,iP: integer);
  destructor Destroy; override;
  { Свойства для получения информации о лексеме }
  property LexType: TLexType read LexInfo.LexType;
  property VarInfo: TVarInfo read LexInfo.VarInfo;
  property ConstVal: integer read LexInfo.ConstVal;
  { Свойства для чтения позиции лексемы
    в исходном тексте программы }
  property StrNum: integer read iStr;
  property PosNum: integer read iPos;
  property PosAll: integer read iAllP;
  { Текстовая информация о типе лексемы }
  function LexInfoStr: string;
  { Имя для лексемы-переменной }
  function VarName: string;
end;

{ Структура для описания списка лексем }
TLexList = class(TList)
 public
  { Деструктор для освобождения памяти
    при уничтожении списка }
  destructor Destroy; override;
  { Процедура очистки списка }
  procedure Clear; override;
  { Процедура и свойство для получения информации
    о лексеме по ее номеру }
  function GetLexem(iIdx: integer): TLexem;
  property Lexem[iIdx: integer]: TLexem read GetLexem;
           default;
end;

implementation

uses SysUtils, LexAuto;

constructor TLexem.CreateKey(LexKey: TLexType;
                             iA,iSt,iP: integer);
{ Конструктор создания лексемы типа "ключевое слово" }
begin
  inherited Create;
  LexInfo.LexType := LexKey;
  { запоминаем тип ключевого слова }
  iStr := iSt; { запоминаем позицию лексемы }
  iPos := iP;
  iAllP := iA;
end;

constructor TLexem.CreateVar(VarInf: TVarInfo;
                             iA,iSt,iP: integer);
{ Конструктор создания лексемы типа "ключевое слово" }
begin
  inherited Create;
  LexInfo.LexType := LEX_VAR; { тип - "переменная" }
  { запоминаем ссылку на таблицу идентификаторов }
  LexInfo.VarInfo := VarInf;
  iStr := iSt; { запоминаем позицию лексемы }
  iPos := iP;
  iAllP := iA;
end;

constructor TLexem.CreateConst(iVal: integer;
                               iA,iSt,iP: integer);
{ Конструктор создания лексемы типа "константа" }
begin
  inherited Create;
  LexInfo.LexType := LEX_CONST; { тип - "константа" }
  { запоминаем значение константы }
  LexInfo.ConstVal := iVal;
  iStr := iSt; { запоминаем позицию лексемы }
  iPos := iP;
  iAllP := iA;
end;

constructor TLexem.CreateInfo(sInf: string;
                              iA,iSt,iP: integer);
{ Конструктор создания информационной лексемы }
begin
  inherited Create;
  LexInfo.LexType := LEX_START; { тип - "доп. лексема" }
  { выделяем память для информации }
  LexInfo.szInfo :=  StrAlloc(Length(sInf)+1);
  { запоминаем информацию }
  StrPCopy(LexInfo.szInfo,sInf);
  iStr := iSt; { запоминаем позицию лексемы }
  iPos := iP;
  iAllP := iA;
end;

destructor TLexem.Destroy;
begin
  { Освобождаем занятую память,
    если это информационная лексема }
  if LexType = LEX_START then StrDispose(LexInfo.szInfo);
  inherited Destroy;
end;

function TLexem.VarName: string;
{ Функция получения имени лексемы типа "переменная" }
begin
  Result := VarInfo.VarName;
end;

function TLexem.LexInfoStr: string;
{ Текстовая информация о типе лексемы }
begin
  case LexType of
    LEX_VAR:   Result := VarName;
    { для переменной - ее имя  }
    LEX_CONST: Result := IntToStr(ConstVal);
    { для константы - значение }
    LEX_START: Result := StrPas(LexInfo.szInfo);
    { для инф. лексемы - информация }
    else       Result := LexTypeInfo(LexType);
    { для остальных - имя типа }
  end;
end;

procedure TLexList.Clear;
{ Процедура очистки списка }
var i: integer;
begin
  { Уничтожаем все элементы списка }
  for i:=Count-1 downto 0 do Lexem[i].Free;
  inherited Clear; { вызываем функцию базового класса }
end;

destructor TLexList.Destroy;
{ Деструктор для освобождения памяти
  при уничтожении списка }
begin
  Clear; { Уничтожаем все элементы списка }
  { Вызываем деструктор базового класса }
  inherited Destroy;
end;

function TLexList.GetLexem(iIdx: integer): TLexem;
{ Получение лексемы из списка по ее номеру }
begin
  Result := TLexem(Items[iIdx]);
end;

end.

