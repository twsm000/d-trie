unit Test.Trie;

interface

uses
  DUnitX.TestFramework,
  DS.Trie;

type

  [TestFixture]
  TTrieTests = class
  private
    FTrie: ITrie<TDateTime, string>;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    [Test]
    procedure CountShouldHasZeroElement;

    [Test]
    procedure CountShouldHasOneElement;

    [Test]
    procedure ContainsKeyShouldReturnFalse;

    [Test]
    procedure ContainsKeyShouldReturnTrue;

    [Test]
    procedure TryGetValueShouldReturnFalse;

    [Test]
    procedure TryGetValueShouldReturnTrueAndSameValueAdded;

    [Test]
    procedure RemoveShouldRemoveKey;

    [Test]
    procedure AddAndRemoveMultiple;
  end;

implementation

uses
  System.DateUtils,
  System.SysUtils;

{ TTrieTests }

procedure TTrieTests.Setup;
begin
  FTrie := TTrie<Word, TDateTime, string>.Create(
    function(Key: TDateTime; Consumer: TConsumer<Word>): Boolean
    const
      cHora = 0;
      cMin  = 1;
      cSec  = 2;
      cMili = 3;
    var
      Time: array[0..3] of Word;
      Index: Integer;
    begin
      System.SysUtils.DecodeTime(Key, Time[cHora], Time[cMin], Time[cSec], Time[cMili]);
      Index := cHora;
      while (Index <= cSec) and Consumer(Time[Index]) do
        Inc(Index);
      Result := Index > cSec;
    end);
end;

procedure TTrieTests.TearDown;
begin
  FTrie := nil;
end;

procedure TTrieTests.CountShouldHasZeroElement;
begin
  Assert.AreEqual<Int64>(0, FTrie.Count);
end;

procedure TTrieTests.CountShouldHasOneElement;
const
  INPUT = 'CountShouldHasOneElement';
var
  Time: TDateTime;
  Output: string;
begin
  Time := Now;
  Assert.IsFalse(FTrie.TryGetValue(Time, Output));

  FTrie.Add(Time, INPUT);
  Assert.AreEqual<Int64>(1, FTrie.Count);

  FTrie.Add(Time, INPUT);
  Assert.AreEqual<Int64>(1, FTrie.Count);
end;

procedure TTrieTests.ContainsKeyShouldReturnFalse;
begin
  Assert.IsFalse(FTrie.ContainsKey(Now));
end;

procedure TTrieTests.ContainsKeyShouldReturnTrue;
const
  INPUT = 'ContainsKeyShouldReturnTrue';
var
  Time: TDateTime;
begin
  Time := Now;
  FTrie.Add(Time, INPUT);

  Assert.IsTrue(FTrie.ContainsKey(Time));
end;

procedure TTrieTests.TryGetValueShouldReturnFalse;
var
  Output: string;
begin
  Assert.IsFalse(FTrie.TryGetValue(Now, Output));
end;

procedure TTrieTests.TryGetValueShouldReturnTrueAndSameValueAdded;
const
  INPUT = 'TryGetValueShouldReturnTrueAndSameValueAdded';
var
  Time: TDateTime;
  Output: string;
begin
  Time := Now;
  FTrie.Add(Time, INPUT);

  Assert.IsTrue(FTrie.TryGetValue(Time, Output));
  Assert.AreEqual(INPUT, Output);
end;

procedure TTrieTests.RemoveShouldRemoveKey;
var
  T1: TDateTime;
  T2: TDateTime;
begin
  T1 := Now;
  T2 := IncMinute(T1);
  FTrie.Add(T1, '1');
  FTrie.Add(T2, '2');
  Assert.AreEqual<Int64>(2, FTrie.Count);

  FTrie.Remove(T1);
  Assert.AreEqual<Int64>(1, FTrie.Count);

  FTrie.Remove(T2);
  Assert.AreEqual<Int64>(0, FTrie.Count);
end;

procedure TTrieTests.AddAndRemoveMultiple;
var
  Elements: Integer;
  Time: TDateTime;
  Hour: Word;
  Minute: Word;
  Second: Word;
  Output: string;
  Total: Int64;
begin
  Elements := 0;
  for Hour := 0 to 23 do
    for Minute := 0 to 59 do
      for Second := 0 to 59 do
        begin
          Time := EncodeTime(Hour, Minute, Second, 0);
          Inc(Elements);
          FTrie.Add(Time, Elements.ToString);
          Assert.AreEqual<Int64>(Elements, FTrie.Count);
        end;

  Total := FTrie.Count;
  Elements := 0;
  for Hour := 0 to 23 do
    for Minute := 0 to 59 do
      for Second := 0 to 59 do
        begin
          Time := EncodeTime(Hour, Minute, Second, 0);
          Inc(Elements);
          Assert.IsTrue(FTrie.TryGetValue(Time, Output));
          Assert.AreEqual(Elements.ToString, Output);

          FTrie.Remove(Time);
          Dec(Total);
          Assert.AreEqual<Int64>(Total, FTrie.Count);
        end;
end;

initialization

TDUnitX.RegisterTestFixture(TTrieTests);

end.
