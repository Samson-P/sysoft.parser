unit TblElem;

interface
{ Модуль, описывающий структуру данных элементов
  таблицы идентификаторов }

type

{ Класс для описания некоторых данных,
  связанных с элементом таблицы }
TAddVarInfo = class(TObject)
 public
  procedure SetInfo(iIdx: integer; iInfo: longint);
   virtual; abstract;
  function GetInfo(iIdx: integer): longint;
   virtual; abstract;
  property Info[iIdx: integer]: longint
   read GetInfo write SetInfo; default;
end;

{ Класс для описания элемента хэш-таблицы }
TVarInfo = class(TObject)
 protected
  { Имя элемента }
  sName: string;
  { Дополнительная информация (для будущего использования)}
  pInfo: TAddVarInfo;
  { Ссылки на меньший и больший элементы
    для организации бинарного дерева }
  minEl,maxEl: TVarInfo;
 public
  { Конструктор создания элемента хэш-таблицы }
  constructor Create(const sN: string);
  { Деструктор для освобождения памяти, занятой элементом }
  destructor Destroy; override;
  { Функция заполнения дополнительной информации элемента }
  procedure SetInfo(pI: TAddVarInfo);
  { Функции для удаления дополнительной информации }
  procedure ClearInfo;
  procedure ClearAllInfo;
  { Свойство "Имя элемента" }
  property VarName: string read sName;
  { Свойство "Дополнительная информация"
   (для будущего использования) }
  property Info: TAddVarInfo read pInfo write SetInfo;
  { Функции для добавления элемента в бинарное дерево }
  function AddElCnt(const sAdd: string;
                    var iCnt: integer): TVarInfo;
  function AddElem(const sAdd: string): TVarInfo;
  { Функции для поиска элемента в бинарном дереве }
  function FindElCnt(const sN: string;
                     var iCnt: integer): TVarInfo;
  function FindElem(const sN: string): TVarInfo;
  { Функция записи всех имен идентификаторов
    в одну строку }
  function GetElList(const sLim,sInp,sOut: string): string;
end;

function Upper(const x:string): string;

implementation

uses SysUtils;

{ Условная компиляция: если определено имя REGNAME,
  то имена переменных считаются регистрозависимыми,
  иначе - регистронезависимыми }
{$IFDEF REGNAME}
function Upper(const x:string): string;
begin Result := UpperCase(x); end;
{$ELSE}
function Upper(const x:string): string;
begin Result := x; end;
{$ENDIF}

constructor TVarInfo.Create(const sN: string);
{ Конструктор создания элемента хэш-таблицы }
begin
  { Вызываем конструктор базового класса TObject }
  inherited Create;
  { Запоминаем имя элемента и обнуляем все ссылки }
  sName := sN;
  pInfo := nil;
  minEl := nil;
  maxEl := nil;
end;

destructor TVarInfo.Destroy;
{ Деструктор для освобождения памяти, занятой элементом }
begin
  { Освобождаем память по каждой ссылке
   (если она не нулевая), при этом в дереве рекурсивно
   будет освобождена память для всех элементов }
  ClearAllInfo;
  minEl.Free;
  maxEl.Free;
  { Вызываем деструктор базового класса TObject }
  inherited Destroy;
end;

function TVarInfo.GetElList(const sLim{разделитель списка},
 sInp,sOut{имена, не включаемые в строку}: string): string;
{ Функция записи всех имен идентификаторов в одну строку }
var sAdd: string;
begin
  { Первоначально строка пуста }
  Result := '';
  { Если элемент таблицы не совпадает с одним
    из невключаемых имен, то его нужно включить в строку }
  if (Upper(sName) <> Upper(sInp))
  and (Upper(sName) <> Upper(sOut)) then Result := sName;
  { Если есть левая ветвь дерева }
  if minEl <> nil then
  begin
    { Вычисляем строку для этой ветви }
    sAdd := minEl.GetElList(sLim,sInp,sOut);
    { Если она не пустая, добавляем ее через разделитель }
    if sAdd <> '' then
    begin
      if Result <> '' then Result := Result + sLim + sAdd
                      else Result := sAdd;
    end;
  end;
  { Если есть правая ветвь дерева }
  if maxEl <> nil then
  begin
    { Вычисляем строку для этой ветви }
    sAdd := maxEl.GetElList(sLim,sInp,sOut);
    { Если она не пустая, добавляем ее через разделитель }
    if sAdd <> '' then
    begin
      if Result <> '' then Result := Result + sLim + sAdd
                      else Result := sAdd;
    end;
  end;
end;

procedure TVarInfo.SetInfo(pI: TAddVarInfo);
{ Функция заполнения дополнительной информации элемента }
begin
  pInfo := pI;
end;

procedure TVarInfo.ClearInfo;
{ Функция удаления дополнительной информации элемента }
begin
  pInfo.Free;
  pInfo := nil;
end;

procedure TVarInfo.ClearAllInfo;
{ Функция удаления дополнительной информации элемента
  и его связок }
begin
  if minEl <> nil then minEl.ClearAllInfo;
  if maxEl <> nil then maxEl.ClearAllInfo;
  pInfo.Free;
  pInfo := nil;
end;

function TVarInfo.AddElCnt(const sAdd: string;
                           var iCnt: integer): TVarInfo;
{ Функция добавления элемента в бинарное дерево
  с учетом счетчика сравнений }
var i: integer;
begin
  { Увеличиваем счетчик сравнений }
  Inc(iCnt);
  { Сравниваем имена нового и текущего элемента
    (одной функцией!) }
  i := StrComp(PChar(Upper(sAdd)),PChar(Upper(sName)));
  if i < 0 then
  { Если новый элемент меньше, смотрим ссылку
    на меньший элемент }
  begin
    { Если ссылка не пустая, рекурсивно вызываем функцию
      добавления элемента }
    if minEl <> nil then
     Result := minEl.AddElCnt(sAdd,iCnt)
    else
    { Если ссылка пустая, создаем новый элемент
      и запоминаем ссылку на него }
    begin
       Result := TVarInfo.Create(sAdd);
       minEl := Result;
    end;
  end
  else
  { Если новый элемент больше, смотрим ссылку
    на больший элемент }
  if i > 0 then
  begin
    { Если ссылка не пустая, рекурсивно вызываем функцию
      добавления элемента }
    if maxEl <> nil then
     Result := maxEl.AddElCnt(sAdd,iCnt)
    else
    { Если ссылка пустая, создаем новый элемент
      и запоминаем ссылку на него }
    begin
       Result := TVarInfo.Create(sAdd);
       maxEl := Result;
    end;
  end
  { Если имена совпадают, то такой элемент уже есть
    в дереве - это текущий элемент }
  else Result := Self;
end;

function TVarInfo.AddElem(const sAdd: string): TVarInfo;
{ Функция добавления элемента в бинарное дерево
  без учета счетчика сравнений }
var iCnt: integer;
begin
  Result := AddElCnt(sAdd,iCnt);
end;

function TVarInfo.FindElCnt(const sN: string;
                            var iCnt: integer): TVarInfo;
{ Функция поиска элемента в бинарном дереве
  с учетом счетчика сравнений }
var i: integer;
begin
  { Увеличиваем счетчик сравнений }
  Inc(iCnt);
  { Сравниваем имена искомого и текущего элемента
   (одной функцией!) }
  i := StrComp(PChar(Upper(sN)),PChar(Upper(sName)));
  if i < 0 then
  { Если искомый элемент меньше, смотрим ссылку
    на меньший элемент }
  begin
    { Если ссылка не пустая, рекурсивно вызываем для нее
      функцию поиска элемента, иначе элемент не найден }
    if minEl <> nil then Result := minEl.FindElCnt(sN,iCnt)
                    else Result := nil;
  end
  else
  if i > 0 then
  { Если искомый элемент больше, смотрим ссылку
    на больший элемент }
  begin
    { Если ссылка не пустая, рекурсивно вызываем для нее
      функцию поиска элемента, иначе элемент не найден }
    if maxEl <> nil then Result := maxEl.FindElCnt(sN,iCnt)
                    else Result := nil;
  end
  { Если имена совпадают, то искомый элемент
    найден - это текущий элемент }
  else Result := Self;
end;

function TVarInfo.FindElem(const sN: string): TVarInfo;
{ Функция поиска элемента в бинарном дереве
  без учета счетчика сравнений }
var iCnt: integer;
begin
  Result := FindElCnt(sN,iCnt);
end;

end.
