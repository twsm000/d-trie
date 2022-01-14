unit Test.Trie;

interface

uses
  DUnitX.TestFramework, 
  Trie;

type
  [TestFixture]
  TTrieTests = class
  private
    FTrie: ITrie<TDateTime, string>;
  public
    [Test]
    procedure Instantiate;
  end;

implementation

uses
  System.SysUtils;

{ TTrieTests }

procedure TTrieTests.Instantiate;
var
  Output: string;
  ADate: TDateTime;
begin
  FTrie := TTrie<string, TDateTime, string>.Create(function(Key: TDateTime): TArray<string>
  begin
    Result := FormatDateTime('hh:ss', Key).Split([':']);
  end);

  ADate := Now;
  Assert.IsFalse(FTrie.TryGetValue(ADate, Output));

  FTrie.Add(ADate, 'Texto');
  Assert.IsTrue(FTrie.TryGetValue(ADate, Output));  
  Assert.AreEqual('Texto', Output);

  Assert.IsTrue(FTrie.TryGetValue(ADate, Output));  
  Assert.AreEqual('Texto', Output);  
end;

initialization
  TDUnitX.RegisterTestFixture(TTrieTests);

end.
