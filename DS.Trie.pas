unit DS.Trie;

interface

uses
  DS.TrieNode,
  System.Generics.Collections,
  System.SysUtils;

type
  ITrie<K, V> = interface
    ['{3AA3415F-271E-4257-8702-9492D74D0D5E}']
    function ContainsKey(Key: K): Boolean;
    function TryGetValue(Key: K; out Value: V): Boolean;
    procedure Add(Key: K; Value: V);
    procedure Remove(Key: K);
    function Count: Int64;
  end;

  TConsumer<E> = reference to function(Element: E): Boolean;
  TMapper<K, E> = reference to function(Key: K; Consumer: TConsumer<E>): Boolean;

  TTrie<E, K, V> = class(TInterfacedObject, ITrie<K, V>)
  private
    FMapper: TMapper<K, E>;
    FRoot: ITrieNode<E, V>;
    FNodeCache: TList<ITrieNode<E, V>>;
    FElementCache: TList<E>;
    FCount: Int64;
  public
    constructor Create(Mapper: TMapper<K, E>);
    destructor Destroy; override;
    function ContainsKey(Key: K): Boolean;
    function TryGetValue(Key: K; out Value: V): Boolean;
    procedure Add(Key: K; Value: V);
    procedure Remove(Key: K);
    function Count: Int64;
  end;

implementation

{ TTrie<E, K, V> }

constructor TTrie<E, K, V>.Create(Mapper: TMapper<K, E>);
begin
  if not Assigned(Mapper) then
    raise EArgumentNilException.Create('TTrie: Mapper not assigned.');

  FMapper := Mapper;
  FRoot := TTrieNode<E, V>.Create;
  FNodeCache := TList<ITrieNode<E, V>>.Create;
  FElementCache := TList<E>.Create;
end;

destructor TTrie<E, K, V>.Destroy;
begin
  FreeAndNil(FElementCache);
  FreeAndNil(FNodeCache);
  FRoot := nil;
  inherited;
end;

function TTrie<E, K, V>.ContainsKey(Key: K): Boolean;
var
  Value: V;
begin
  Result := Self.TryGetValue(Key, Value);
end;

function TTrie<E, K, V>.TryGetValue(Key: K; out Value: V): Boolean;
var
  CurrentNode: ITrieNode<E, V>;
  NextNode: ITrieNode<E, V>;
begin
  CurrentNode := FRoot;
  Result := FMapper(Key, function(Element: E): Boolean
  begin
    Result := CurrentNode.TryGetNext(Element, NextNode);
    CurrentNode := NextNode;
  end);

  if Result then
    Value := CurrentNode.Value;
end;

procedure TTrie<E, K, V>.Add(Key: K; Value: V);
var
  CurrentNode: ITrieNode<E, V>;
  AddValue: Boolean;
begin
  CurrentNode := FRoot;
  if FMapper(Key, function(Element: E): Boolean
  begin
    Result := True;
    CurrentNode := CurrentNode.Add(Element);
  end) then
    begin
      if not CurrentNode.Assigned then
        Inc(FCount);
      CurrentNode.Value := Value;
    end;
end;

procedure TTrie<E, K, V>.Remove(Key: K);
var
  NextNode: ITrieNode<E, V>;
  Index: Integer;
begin
  Index := FNodeCache.Add(FRoot);
  if FMapper(Key, function(Element: E): Boolean
  begin
    FElementCache.Add(Element);
    Result := FNodeCache[Index].TryGetNext(Element, NextNode);
    Index := FNodeCache.Add(NextNode);
  end) then
  begin
    if FNodeCache[Pred(FNodeCache.Count)].Assigned then
      Dec(FCount);
    FNodeCache[Index].Unassign;
    FNodeCache.Delete(Index);
    for Index := Pred(FNodeCache.Count) downto 0 do
      FNodeCache[Index].Remove(FElementCache[Index]);
  end;
  FElementCache.Clear;
  FNodeCache.Clear;
end;

function TTrie<E, K, V>.Count: Int64;
begin
  Result := FCount;
end;

end.
