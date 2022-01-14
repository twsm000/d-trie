unit Trie;

interface

uses
  System.Generics.Collections,
  System.SysUtils;

type
  ITrie<K, V> = interface
    ['{3AA3415F-271E-4257-8702-9492D74D0D5E}']
    function TryGetValue(Key: K; out Value: V): Boolean;
    procedure Add(Key: K; Value: V);
  end;

  ITrieNode<K, V> = interface
    ['{ACAA5F31-7914-4D85-BB29-C7FD3D343D10}']
    function TryGetNext(Key: K; out Next: ITrieNode<K, V>): Boolean;
    function Add(Key: K): ITrieNode<K, V>;
    function GetValue: V;
    procedure SetValue(const Value: V);
    property Value: V read GetValue write SetValue;
  end;

  TTrieNode<K, V> = class(TInterfacedObject, ITrieNode<K, V>)
  strict private
    FMapper: TDictionary<K, ITrieNode<K, V>>;
    FValue: V;
    function GetValue: V; inline;
    procedure SetValue(const Value: V); inline;
  public
    constructor Create;
    destructor Destroy; override;
    function TryGetNext(Key: K; out Next: ITrieNode<K, V>): Boolean; inline;
    function Add(Key: K): ITrieNode<K, V>; inline;
    property Value: V read GetValue write SetValue;
  end;

  TTrie<I, K, V> = class(TInterfacedObject, ITrie<K, V>)
  private
    FMapper: TFunc<K, TArray<I>>;
    FRoot: ITrieNode<I, V>;
  public
    constructor Create(Mapper: TFunc<K, TArray<I>>);
    destructor Destroy; override;
    function TryGetValue(Key: K; out Value: V): Boolean;
    procedure Add(Key: K; Value: V);
  end;

implementation

{ TTrieNode<K, V> }

constructor TTrieNode<K, V>.Create;
begin
  FMapper := TDictionary<K, ITrieNode<K, V>>.Create;
end;

destructor TTrieNode<K, V>.Destroy;
begin
  FMapper.Free;
  inherited Destroy;
end;

function TTrieNode<K, V>.TryGetNext(Key: K; out Next: ITrieNode<K, V>): Boolean;
begin
  Result := FMapper.TryGetValue(Key, Next);
end;

function TTrieNode<K, V>.Add(Key: K): ITrieNode<K, V>;
begin
  if not Self.TryGetNext(Key, Result) then
  begin
    Result := TTrieNode<K, V>.Create;
    FMapper.Add(Key, Result);
  end;
end;

function TTrieNode<K, V>.GetValue: V;
begin
  Result := FValue;
end;

procedure TTrieNode<K, V>.SetValue(const Value: V);
begin
  FValue := Value;
end;

{ TTrie<I, K, V> }

constructor TTrie<I, K, V>.Create(Mapper: TFunc<K, TArray<I>>);
begin
  if not Assigned(Mapper) then
    raise EArgumentNilException.Create('TTrie: Mapper not assigned.');

  FMapper := Mapper;
  FRoot := TTrieNode<I, V>.Create;
end;

destructor TTrie<I, K, V>.Destroy;
begin
  FRoot := nil;
  inherited;
end;

function TTrie<I, K, V>.TryGetValue(Key: K; out Value: V): Boolean;
var
  Indexes: TArray<I>;
  Index: I;
  CurrentNode: ITrieNode<I, V>;
  NextNode: ITrieNode<I, V>;
begin
  Indexes := FMapper(Key);
  CurrentNode := FRoot;
  for Index in Indexes do
  begin
    if not CurrentNode.TryGetNext(Index, NextNode) then
      Exit;
    CurrentNode := NextNode;
  end;

  Value := CurrentNode.Value;
  Result := True;
end;

procedure TTrie<I, K, V>.Add(Key: K; Value: V);
var
  Indexes: TArray<I>;
  Index: I;
  CurrentNode: ITrieNode<I, V>;
  NextNode: ITrieNode<I, V>;
begin
  Indexes := FMapper(Key);
  CurrentNode := FRoot;
  for Index in Indexes do
    CurrentNode := CurrentNode.Add(Index);

  CurrentNode.Value := Value;
end;

end.
