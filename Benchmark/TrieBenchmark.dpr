program TrieBenchmark;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Spring.Benchmark in 'dep\benchmark\Spring.Benchmark.pas',
  Benchmark.Trie in 'Benchmark.Trie.pas';

var
  WaitOutput: string;

begin
  try
    // Register the function as a benchmark
    Spring.Benchmark.Benchmark(BM_QuickSort, 'QuickSort').Arg(8).Arg(64).Arg(512).Arg(1024).Arg(8192);

    // Run the benchmark
    Spring.Benchmark.Benchmark_Main;

    WriteLn('Press ENTER to exit:');
    ReadLn(WaitOutput);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
