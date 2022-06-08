unit FncTree;

interface
{ Модуль, обеспечивающий работу с комбинированной таблицей
  идентификаторов, построенной на основе хэш-функции и
  бинарного дерева }

uses TblElem;

{ Функция начальной инициализации хэш-таблицы }
procedure InitTreeVar;
{ Функция освобождения памяти хэш-таблицы }
procedure ClearTreeVar;
{ Функция удаления дополнительной информации в таблице }
procedure ClearTreeInfo;
{ Добавление элемента в таблицу идентификаторов }
function AddTreeVar(const sName: string): TVarInfo;
{ Поиск элемента в таблице идентификаторов }
function GetTreeVar(const sName: string): TVarInfo;
{ Функция, возвращающая количество операций сравнения }
function GetTreeCount: integer;

{ Функция записи всех имен идентификаторов в одну строку }
function IdentList(const sLim,sInp,sOut: string): string;

implementation

const
{ Минимальный и максимальный элементы хэш-таблицы
 (охватывают весь диапазон значений хэш-функции) }
  HASH_MIN = Ord('0')+Ord('0');
  HASH_MAX = Ord('z')+Ord('z');

var
  HashArray : array[HASH_MIN..HASH_MAX] of TVarInfo;
{ Массив для хэш-таблицы }
  iCmpCount : integer;
{ Счетчик количества сравнений }

function GetTreeCount: integer;
begin
  Result := iCmpCount;
end;

function IdentList(const sLim,sInp,sOut: string): string;
{ Функция записи всех имен идентификаторов в одну строку }
var
  i: integer;   { счетчик идентификаторов }
  sAdd: string; { стока для временного хранения данных }
begin
  { Первоначально строка пуста }
  Result := '';
  { Цикл по всем идентификаторам в таблице }
  for i:=HASH_MIN to HASH_MAX do
  begin
    { Если ячейка таблицы пустая, то добавлять ничего
      не нужно, }
    if HashArray[i] = nil then sAdd := ''
    { иначе вычисляем добавочную часть строки }
    else sAdd := HashArray[i].GetElList(sLim,sInp,sOut);
    { Если добавочная часть строки не пуста,
      то добавляем ее в общую строку через разделитель }
    if sAdd <> '' then
    begin
      if Result <> '' then Result := Result + sLim + sAdd
                      else Result := sAdd;
    end;
  end{for};
end;

function VarHash(const sName: string): longint;
{ Хэш функция - сумма кодов первого и среднего
  символов строки }
begin
  Result := (Ord(sName[1])
           + Ord(sName[(Length(sName)+1) div 2])
           - HASH_MIN) mod (HASH_MAX-HASH_MIN+1)+HASH_MIN;
  if Result < HASH_MIN then Result := HASH_MIN;
end;

procedure InitTreeVar;
{ Начальная инициализация хэш-таблицы -
  все элементы пустые }
var i : integer;
begin
  for i:=HASH_MIN to HASH_MAX do HashArray[i] := nil;
end;

procedure ClearTreeVar;
{ Освобождение памяти для всех элементов хэш-таблицы }
var i : integer;
begin
  for i:=HASH_MIN to HASH_MAX do
  begin
    HashArray[i].Free;
    HashArray[i] := nil;
  end;
end;

procedure ClearTreeInfo;
{ Удаление дополнительной информации для всех
  элементов хэш-таблицы }
var i : integer;
begin
  for i:=HASH_MIN to HASH_MAX do
   if HashArray[i] <> nil then HashArray[i].ClearAllInfo;
end;

function AddTreeVar(const sName: string): TVarInfo;
{ Добавление элемента в хэш-таблицу и дерево }
var iHash: integer;
begin
  { Обнуляем счетчик количества сравнений }
  iCmpCount := 0;
  { Вычисляем хэш-адрес с помощью хэш-функции }
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
{ Вызов начальной инициализации таблицы
  при загрузке модуля }
  InitTreeVar;

finalization
{ Вызов освобождения памяти таблицы при выгрузке модуля }
  ClearTreeVar;

end.
