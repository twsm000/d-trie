unit BM.Trie;

interface

uses
  DS.Trie,
  Spring.Benchmark;

procedure BM_AddAllMinutesInADay(const State: TState);

implementation

uses
  System.SysUtils,
  System.DateUtils,
  System.Generics.Collections;

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

end.
