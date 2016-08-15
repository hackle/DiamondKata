module FataTests

open Fata.Dimaond
open Xunit
open Swensen.Unquote

[<Fact>]
let ``For 'A' the diamond is just 'A'`` () = 
    let actual = diamond 'A'
    let expected = [ [ 'A' ] ]
    Assert.Equal<char list list>(actual, expected)
     
[<Fact>]
let ``For 'B' I have ABA vertically`` () = 
    let actual = 
        diamond 'B' 
        |> List.map (fun l -> l |> List.filter (fun x -> x <> ' '))
    let expected = [ [ 'A' ]; ['B']; ['A'] ]
    Assert.Equal<char list list>(actual, expected)

let splitHead ls = 
    match ls with
    | h::t -> h, t
    | _ -> failwith "must have at least 2 element"

let takeAllHead ls =
    ls |> List.map (fun item ->
                        let (l, r) = item
                        l)
let takeAllTail ls =
    ls |> List.map (fun item ->
                        let (l, r) = item
                        r)

let rec folder (ls: 'a list) (carry: 'a list list)  =
    match carry, ls with
    | lh::ltail, h::tail -> (h::lh) :: (folder tail ltail)
    | _ -> carry

let rotate lists = 
    List.foldBack folder lists (lists.Head |> List.map (fun l -> List.empty<char>))

[ [ 'A'; 'B'; 'C' ]; [ 'D'; 'E'; 'F' ]]
|> rotate

[<Fact>]
let ``For 'B' I have 'BAB' horozontally`` () =
    let actual =
        diamond 'B'
        |> slice
        |> 