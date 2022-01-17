unit Benchmark.Trie;

interface

uses
  Spring.Benchmark;

procedure BM_QuickSort(const State: TState);

implementation

uses
  System.Generics.Collections;

procedure BM_QuickSort(const State: TState);
var
  count: Integer;
  numbers: TArray<Integer>;
  I: Integer;
  V: TState.TValue;
begin
  count := State[0];
  SetLength(numbers, count);
  for I := 0 to Pred(count) do
  begin
    numbers[I] := count - I;
  end;

  for V in State do
    TArray.Sort<Integer>(numbers);
end;

end.
