unit LexAuto; {!!! Зависит от входного языка !!!}

interface
{ Модуль, обеспечивающий построение таблицы лексем
  по исходному тексту программы }

uses Classes, TblElem, LexType, LexElem;

{ Функция создания списка лексем
  по исходному тексту программы }
function MakeLexList(listFile: TStrings;
                     listLex: TLexList): integer;

implementation

uses SysUtils, FncTree;

type
{ Перечень всех возможных состояний конечного автомата }
TAutoPos = (
  AP_START,AP_THEN1,AP_THEN2,AP_THEN3,AP_THEN4,
  AP_ELSE1,AP_ELSE2,AP_ELSE3,AP_ELSE4,AP_IF1,AP_IF2,
  AP_XOR1,AP_XOR2,AP_XOR3,AP_AND1,AP_AND2,AP_AND3,
  AP_OR1,AP_OR2,AP_ASSIGN,AP_VAR,AP_CONST,AP_COMM,AP_ERR);

function MakeLexList(listFile: TStrings;
                     listLex: TLexList): integer;
{ Функция создания списка лексем
   по исходному тексту программы }
var
  i,j,iCnt,iStr,   { Переменные и счетчики циклов }
  iAll,    { Счетчик общего количества входных символов }
  iStComm,iStart: integer;
  { Переменные для запоминания позиции начала лексемы }
  posCur: TAutoPos; { Текущее состояние КА }
  sCurStr,sTmp: string;
  { Строки для временного хранения результатов }
{ Несколько простых процедур для работы со списком лексем }
procedure AddVarToList(posNext: TAutoPos; iP: integer);
{ Процедура добавления переменной в список }
begin
  { Выделяем имя переменной из текущей строки }
  sTmp := System.Copy(sCurStr,iStart,iP-iStart);
  { При создании переменной она сначала заносится в таблицу
    идентификаторов, а потом ссылка на нее -
    в таблицу лексем }
  listLex.Add(TLexem.CreateVar(AddTreeVar(sTmp),iStComm,
                               i,iStart));
  iStart := j;
  iStComm := iAll-1;
  posCur := posNext;
end;
procedure AddVarKeyToList(keyAdd: TLexType;
                          posNext: TAutoPos);
{ Процедура добавления переменной и разделителя в список }
begin
  { Выделяем имя переменной из текущей строки }
  sTmp := System.Copy(sCurStr,iStart,j-iStart);
  { При создании переменной она сначала заносится в таблицу
    идентификаторов, а потом ссылка на нее -
    в таблицу лексем }
  listLex.Add(TLexem.CreateVar(AddTreeVar(sTmp),iStComm,
                               i,iStart));
  listLex.Add(TLexem.CreateKey(keyAdd,iAll,i,j));
  iStart := j;
  iStComm := iAll-1;
  posCur := posNext;
end;
procedure AddConstToList(posNext: TAutoPos; iP: integer);
{ Процедура добавления константы в список }
begin
  { Выделяем константу из текущей строки }
  sTmp := System.Copy(sCurStr,iStart,iP-iStart);
  { Заносим константу в список вместе с ее значением }
  listLex.Add(TLexem.CreateConst(StrToInt(sTmp),iStComm,
                                 i,iStart));
  iStart := j;
  iStComm := iAll-1;
  posCur := posNext;
end;
procedure AddConstKeyToList(keyAdd: TLexType;
                            posNext: TAutoPos);
{ Процедура добавления константы и разделителя в список }
begin
  { Выделяем константу из текущей строки }
  sTmp := System.Copy(sCurStr,iStart,j-iStart);
  { Заносим константу в список вместе с ее значением }
  listLex.Add(TLexem.CreateConst(StrToInt(sTmp),iStComm,
                                 i,iStart));
  listLex.Add(TLexem.CreateKey(keyAdd,iAll,i,j));
  iStart := j;
  iStComm := iAll-1;
  posCur := posNext;
end;
procedure AddKeyToList(keyAdd: TLexType;
                       posNext: TAutoPos);
{ Процедура добавления ключевого или разделителя слова
  в список }
begin
  listLex.Add(TLexem.CreateKey(keyAdd,iStComm,i,iStart));
  iStart := j;
  iStComm := iAll-1;
  posCur := posNext;
end;
procedure Add2KeysToList(keyAdd1,keyAdd2: TLexType;
                         posNext: TAutoPos);
{ Процедура добавления ключевого слова и разделителя
  в список подряд }
begin
  listLex.Add(TLexem.CreateKey(keyAdd1,iStComm,i,iStart));
  listLex.Add(TLexem.CreateKey(keyAdd2,iAll,i,j));
  iStart := j;
  iStComm := iAll-1;
  posCur := posNext;
end;
procedure KeyLetter(chNext: char; posNext: TAutoPos);
{ Процедура проверки очередного символа ключевого слова }
begin
  case sCurStr[j] of
    ':': AddVarToList(AP_ASSIGN,j);
    '(': AddVarKeyToList(LEX_OPEN,AP_START);
    ')': AddVarKeyToList(LEX_CLOSE,AP_START);
    ';': AddVarKeyToList(LEX_SEMI,AP_START);
    '{': AddVarToList(AP_COMM,j);
    ' ',#10,#13,#9: AddVarToList(AP_START,j);
    else
      if sCurStr[j] = chNext then posCur := posNext
      else
      if sCurStr[j] in ['0'..'9','A'..'Z','a'..'z','_']
      then posCur := AP_VAR
      else posCur := AP_ERR;
  end{case list};
end;
procedure KeyFinish(keyAdd: TLexType);
{ Процедура проверки завершения ключевого слова }
begin
  case sCurStr[j] of
    ':': AddKeyToList(keyAdd,AP_ASSIGN);
    '(': Add2KeysToList(keyAdd,LEX_OPEN,AP_START);
    ')': Add2KeysToList(keyAdd,LEX_CLOSE,AP_START);
    ';': Add2KeysToList(keyAdd,LEX_SEMI,AP_START);
    '0'..'9','A'..'Z','a'..'z','_': posCur := AP_VAR;
    '{': AddKeyToList(keyAdd,AP_COMM);
    ' ',#10,#13,#9: AddKeyToList(keyAdd,AP_START);
    else posCur := AP_ERR;
  end{case list};
end;
begin
  { Обнуляем общий счетчик символов и результат функции }
  iAll := 0;
  Result := 0;
  iStComm := 0;
  { Устанавливаем начальное состояние конечного автомата }
  posCur := AP_START;
  { Цикл по всем строкам входного файла }
  iCnt := listFile.Count-1;
  for i:=0 to iCnt do
  begin
    { Позиция начала лексемы - первый символ }
    iStart := 1;
    { Запоминаем текущую строку }
    sCurStr := listFile[i];
    { Цикл по всем символам текущей строки }
    iStr := Length(sCurStr);
    for j:=1 to iStr do
    begin
      { Увеличиваем общий счетчик символов }
      Inc(iAll);
      { Моделируем работу КА в зависимости от состояния
        автомата и текущего символа входной строки }
      case posCur of
        AP_START:
        begin
          { В начальном состоянии запоминаем позицию
            начала лексемы }
          iStart := j;
          iStComm := iAll-1;
          case sCurStr[j] of
            'i': posCur := AP_IF1;
            't': posCur := AP_THEN1;
            'e': posCur := AP_ELSE1;
            'o': posCur := AP_OR1;
            'x': posCur := AP_XOR1;
            'a': posCur := AP_AND1;
            ':': posCur := AP_ASSIGN;
            '(': listLex.Add(TLexem.CreateKey(LEX_OPEN,
                                              iAll,i,j));
            ')': listLex.Add(TLexem.CreateKey(LEX_CLOSE,
                                              iAll,i,j));
            ';': listLex.Add(TLexem.CreateKey(LEX_SEMI,
                                              iAll,i,j));
            '0'..'9': posCur := AP_CONST;
            'A'..'Z','b'..'d','f'..'h','j'..'n','p'..'s',
            'u'..'w','y','z','_': posCur := AP_VAR;
            '{': posCur := AP_COMM;
            ' ',#10,#13,#9: ;
            else posCur := AP_ERR;
          end{case list};
        end;
        AP_IF1:   KeyLetter('f',AP_IF2);
        AP_IF2:   KeyFinish(LEX_IF);
        AP_THEN1: KeyLetter('h',AP_THEN2);
        AP_THEN2: KeyLetter('e',AP_THEN3);
        AP_THEN3: KeyLetter('n',AP_THEN4);
        AP_THEN4: KeyFinish(LEX_THEN);
        AP_ELSE1: KeyLetter('l',AP_ELSE2);
        AP_ELSE2: KeyLetter('s',AP_ELSE3);
        AP_ELSE3: KeyLetter('e',AP_ELSE4);
        AP_ELSE4: KeyFinish(LEX_ELSE);
        AP_OR1:   KeyLetter('r',AP_OR2);
        AP_OR2:   KeyFinish(LEX_OR);
        AP_XOR1:  KeyLetter('o',AP_XOR2);
        AP_XOR2:  KeyLetter('r',AP_XOR3);
        AP_XOR3:  KeyFinish(LEX_XOR);
        AP_AND1:  KeyLetter('n',AP_AND2);
        AP_AND2:  KeyLetter('d',AP_AND3);
        AP_AND3:  KeyFinish(LEX_AND);
        AP_ASSIGN:
          case sCurStr[j] of
            '=': AddKeyToList(LEX_ASSIGN,AP_START);
            else posCur := AP_ERR;
          end{case list};
        AP_VAR:
          case sCurStr[j] of
            ':': AddVarToList(AP_ASSIGN,j);
            '(': AddVarKeyToList(LEX_OPEN,AP_START);
            ')': AddVarKeyToList(LEX_CLOSE,AP_START);
            ';': AddVarKeyToList(LEX_SEMI,AP_START);
            '0'..'9','A'..'Z','a'..'z','_':
                 posCur := AP_VAR;
            '{': AddVarToList(AP_COMM,j);
            ' ',#10,#13,#9: AddVarToList(AP_START,j);
            else posCur := AP_ERR;
          end{case list};
        AP_CONST:
          case sCurStr[j] of
            ':': AddConstToList(AP_ASSIGN,j);
            '(': AddConstKeyToList(LEX_OPEN,AP_START);
            ')': AddConstKeyToList(LEX_CLOSE,AP_START);
            ';': AddConstKeyToList(LEX_SEMI,AP_START);
            '0'..'9': posCur := AP_CONST;
            '{': AddConstToList(AP_COMM,j);
            ' ',#10,#13,#9: AddConstToList(AP_START,j);
            else posCur := AP_ERR;
          end{case list};
        AP_COMM:
          case sCurStr[j] of
            '}': posCur := AP_START;
          end{case list};
      end{case pos};
      { Проверяем, не достигнут ли конец строки }
      if j = iStr then
      begin
        { Конец строки - это конец текущей лексемы }
        case posCur of
          AP_IF2:   AddKeyToList(LEX_IF,AP_START);
          AP_THEN4: AddKeyToList(LEX_THEN,AP_START);
          AP_ELSE4: AddKeyToList(LEX_ELSE,AP_START);
          AP_OR2:   AddKeyToList(LEX_OR,AP_START);
          AP_XOR3:  AddKeyToList(LEX_XOR,AP_START);
          AP_AND3:  AddKeyToList(LEX_AND,AP_START);
          AP_CONST: AddConstToList(AP_START,j+1);
          AP_ASSIGN: posCur := AP_ERR;
          AP_IF1,AP_THEN1,AP_THEN2,AP_THEN3,
          AP_ELSE1,AP_ELSE2,AP_ELSE3,AP_OR1,
          AP_XOR1,AP_XOR2,AP_AND1,AP_AND2,AP_VAR:
                    AddVarToList(AP_START,j+1);
        end{case pos2};
      end;
      { Проверяем не была ли ошибка в лексемах }
      if posCur = AP_ERR then
      begin
        { Если была ошибка, вычисляем позицию
          ошибочной лексемы }
        iStart := (j - iStart)+1;
        { и запоминаем ее в виде фиктивной лексемы в начале
          списка для детальной диагностики ошибки }
        listLex.Insert(0,
                  TLexem.CreateInfo('Недопустимая лексема',
                                    iAll-iStart,i,iStart));
        { Если ошибка, прерываем цикл }
        Break;
      end;
    end{for j};
    { В конце строки увеличиваем общий счетчик символов
      на 2: конец строки и возврат каретки }
    Inc(iAll,2);
    { Если ошибка, запоминаем номер ошибочной строки
      и прерываем цикл }
    if posCur = AP_ERR then
    begin
      Result := i+1;
      Break;
    end;
  end{for i};
  { Если комментарий не был закрыт, то это ошибка }
  if posCur = AP_COMM then
  begin
    listLex.Insert(0,
             TLexem.CreateInfo('Незакрытый комментарий',
                               iStComm,iCnt,iAll-iStComm));
    Result := iCnt;
  end
  else
  if not (posCur in [AP_START,AP_ERR]) then
  { Если КА не в начальном состоянии-это неверная лексема }
  begin
    listLex.Insert(0,
               TLexem.CreateInfo('Незавершенная лексема',
                                 iAll-iStart,iCnt,iStart));
    Result := iCnt;
  end;
end;

end.

