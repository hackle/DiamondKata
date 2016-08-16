module FataTests

open Fata.Diamond
open Xunit
open Swensen.Unquote

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

let explode (line: string) =
    [ for l in line -> l]
 
[<Fact>]
let ``For 'B' I have ABA vertically`` () = 
    let matrix = 
        diamond 'B' 
    
    let actual =
        matrix.Split([| System.Environment.NewLine |], System.StringSplitOptions.RemoveEmptyEntries)
        |> List.ofArray
        |> List.map explode
        |> List.map (fun l -> 
                            l 
                            |> List.filter (fun x -> x <> ' ') 
                            |> List.distinct)
    let expected = [ [ 'A' ]; ['B']; ['A'] ]
    Assert.Equal<char list list>(actual, expected)

[<Fact>]
let ``For 'B' I have 'BAB' horozontally`` () =
    let matrix =
        diamond 'B'
    let actual =
        matrix.Split([| System.Environment.NewLine |], System.StringSplitOptions.RemoveEmptyEntries)
        |> List.ofArray
        |> List.map explode
        |> rotate
        |> List.map (fun ls -> 
                        ls 
                        |> List.filter (fun i -> i <> ' ')
                        |> List.distinct)
    Assert.Equal<char list>(actual, [ ['B']; ['A']; ['B'] ]);
        