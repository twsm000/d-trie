program TrieBenchmark;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Spring.Benchmark,
  BM.Trie in 'BM.Trie.pas';

var
  WaitOutput: string;

begin
  try
    // Register the function as a benchmark
    Benchmark(BM_AddAllMinutesInADay, 'BM_AddAllMinutesInADay');
    Benchmark(BM_ReadAllWordsIntoTrie, 'BM_ReadAllWordsIntoTrie');
    Benchmark(BM_ReadAllWordsIntoDictionary, 'BM_ReadAllWordsIntoDictionary');

    // Run the benchmark
    Benchmark_Main(True);

    WriteLn('Press ENTER to exit:');
    ReadLn(WaitOutput);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
