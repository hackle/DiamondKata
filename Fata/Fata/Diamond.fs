namespace Fata

module Diamond =
    let diamond (letter: char) = 
        let vertical = 
            [ (int 'A') .. (int letter)] @ [(int letter) - 1 .. -1 .. (int 'A')]
            |> List.map char
        let cntRows = vertical |> List.length
        
        [ (int letter) .. -1 .. (int 'A')] @ [ (int 'B') .. (int letter) ]
        |> List.map char
        |> List.replicate cntRows
        |> List.map2 (fun v h -> h |> List.map (fun l -> if l = v then v else ' ')) vertical
        |> List.map (fun l -> l |> List.map string |> Seq.ofList |> String.concat "")
        |> Seq.ofList |> String.concat System.Environment.NewLine