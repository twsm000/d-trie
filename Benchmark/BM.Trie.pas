unit BM.Trie;

interface

uses
  DS.Trie,
  Spring.Benchmark;

procedure BM_AddAllMinutesInADay(const State: TState);
procedure BM_ReadAllWordsIntoTrie(const State: TState);
procedure BM_ReadAllWordsIntoDictionary(const State: TState);

implementation

uses
  System.Classes,
  System.SysUtils,
  System.DateUtils,
  System.Generics.Collections;

const
  FILE_NAME = '.\br-utf8.txt';
var
  WordList: TStrings;  

procedure BM_AddAllMinutesInADay(const State: TState);
var
  Trie: ITrie<TDateTime, TDateTime>;
  Current: TDateTime;
begin
  Trie := TTrie<Word, TDateTime, TDateTime>.Create(
    function(Key: TDateTime; Consumer: TConsumer<Word>): Boolean
    const
      cHora = 0;
      cMin  = 1;
      cSec  = 2;
      cMili = 3;
      LIMIT = cMin;
    var
      Time: array[0..3] of Word;
      Index: Integer;
    begin
      System.SysUtils.DecodeTime(Key, Time[cHora], Time[cMin], Time[cSec], Time[cMili]);
      Index := cHora;
      while (Index <= LIMIT) and Consumer(Time[Index]) do
        Inc(Index);
      Result := Index > LIMIT;
    end);

  Current := 0;
  for var _ in state do
  begin
    Trie.Add(Current, Current);
    Current := IncMinute(Current);
  end;
end;

procedure BM_ReadAllWordsIntoTrie(const State: TState);
var
  Trie: ITrie<String, Integer>;
begin
  if not Assigned(WordList) then
    Exit;
  
  Trie := TTrie<Char, String, Integer>.Create(
    function(Key: String; Consumer: TConsumer<Char>): Boolean
    var
      AChar: Char;
    begin
      Result := Length(Key) > 0;
      for AChar in Key do
        if not Consumer(AChar) then
          Exit(False);
    end);

  for var _ in state do
  begin
    for var Index: Integer := 0 to Pred(WordList.Count) do
      Trie.Add(WordList[Index], Length(WordList[Index]));
  end;
end;

procedure BM_ReadAllWordsIntoDictionary(const State: TState);
var  
  WordDict: TDictionary<String, Integer>;  
begin
  if not Assigned(WordList) then
    Exit;
  WordDict := TDictionary<String, Integer>.Create(WordList.Count);
  try
  
    for var _ in state do
    begin
      for var Index: Integer := 0 to Pred(WordList.Count) do
        WordDict.AddOrSetValue(WordList[Index], Length(WordList[Index]));
    end;        
  finally
    WordDict.Free;
  end;
end;

initialization
  if not FileExists(FILE_NAME) then
  begin
    WriteLn(Format('File %s does not exists', [FILE_NAME]));
    Exit;
  end;
  
  WordList := TStringList.Create;
  WordList.LoadFromFile(FILE_NAME);  

finalization
  FreeAndNil(WordList);
    
end.
