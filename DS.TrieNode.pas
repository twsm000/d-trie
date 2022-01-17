unit DS.TrieNode;

interface

uses
  System.Generics.Collections;

type
  ITrieNode<K, V> = interface
    ['{ACAA5F31-7914-4D85-BB29-C7FD3D343D10}']
    function TryGetNext(Key: K; out Next: ITrieNode<K, V>): Boolean;
    function Add(Key: K): ITrieNode<K, V>;
    function GetValue: V;
    procedure SetValue(const Value: V);
    property Value: V read GetValue write SetValue;
    function Assigned: Boolean;
    procedure Unassign;
    function Remove(Key: K): Boolean;
    function Count: Int64;
  end;

  TTrieNode<K, V> = class(TInterfacedObject, ITrieNode<K, V>)
  strict private
    FMapper: TDictionary<K, ITrieNode<K, V>>;
    FValueAssigned: Boolean;
    FValue: V;
    function GetValue: V; inline;
    procedure SetValue(const Value: V); inline;
  public
    constructor Create;
    destructor Destroy; override;
    function TryGetNext(Key: K; out Next: ITrieNode<K, V>): Boolean; inline;
    function Add(Key: K): ITrieNode<K, V>; inline;
    property Value: V read GetValue write SetValue;
    function Assigned: Boolean;
    procedure Unassign;
    function Remove(Key: K): Boolean;
    function Count: Int64; inline;
  end;

implementation

{ TTrieNode<K, V> }

constructor TTrieNode<K, V>.Create;
begin
  FMapper := TDictionary < K, ITrieNode < K, V >>.Create;
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
  FValueAssigned := True;
end;

function TTrieNode<K, V>.Assigned: Boolean;
begin
  Result := FValueAssigned;
end;

procedure TTrieNode<K, V>.Unassign;
begin
  FValue := System.Default(V);
  FValueAssigned := False;
end;

function TTrieNode<K, V>.Remove(Key: K): Boolean;
var
  NextNode: ITrieNode<K, V>;
begin
  Result := Self.TryGetNext(Key, NextNode);
  if Result and (NextNode.Count = 0) then
    FMapper.Remove(Key);
end;

function TTrieNode<K, V>.Count: Int64;
begin
  Result := FMapper.Count;
end;

end.
